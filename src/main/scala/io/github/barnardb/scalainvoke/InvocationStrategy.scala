package io.github.barnardb.scalainvoke

import scala.reflect.macros.blackbox

import io.github.barnardb.scalainvoke.macroutil.OwnerChainCorrector

object InvocationStrategy {
  class MacroImplementations(val c: blackbox.Context) {
    import c.universe._

    protected def findStrategy[Environment]: Expr[InvocationStrategy[Environment]] =
      if (c.prefix.actualType <:< typeOf[InvocationStrategy[_]]) c.Expr(c.prefix.tree)
      else c.prefix.tree.symbol.typeSignature.member(TermName("strategy")) match {
        case NoSymbol => c.abort(c.enclosingPosition, s"Can't find the InvocationStrategy to use (prefix symbol ${c.prefix.tree.symbol}, sig: ${c.prefix.tree.symbol.typeSignature}})")
        case strategy => c.Expr(q"${c.prefix}.$strategy")
      }

    protected def extractParameter[Environment](parameter: Symbol)(implicit strategy: Expr[InvocationStrategy[Environment]]): Tree = {
      val extractionMethod =
        if (parameter.isImplicit) TermName("extractImplicit")
        else TermName("extract")
      q"$strategy.$extractionMethod[${parameter.typeSignature}](environment, ${parameter.name.toString})"
    }

    protected def extractFunctionParamSymbols(tree: Tree): List[Symbol] = tree match {
      case Function(params, _) => params.map(_.symbol)
      case Block(_, expr)      => extractFunctionParamSymbols(expr)
      case Typed(expr, _)      => extractFunctionParamSymbols(expr)
      case Ident(_)            => c.abort(tree.pos, s"Can't unwrap a function from ${showRaw(tree)}")
    }

    protected def createLiftedFunction[Environment: WeakTypeTag, R: WeakTypeTag](function: Tree, parameterLists: List[List[Symbol]]): Expr[Environment => R] = c.Expr {
      implicit val strategy = findStrategy[Environment]
      c.typecheck(
        tree = q"(environment: ${weakTypeOf[Environment]}) => $function(...${parameterLists.map(_.map(extractParameter[Environment]))})",
        pt = weakTypeOf[Environment => R]
      )
    }

    def liftImpl[Environment: WeakTypeTag, R: WeakTypeTag](function: Tree): Expr[Environment => R] = function match {
      case Block(stats, expr) =>
        import c.internal._, decorators._
        c.Expr(Block(stats, liftImpl[Environment, R](expr).tree) setType weakTypeOf[Environment => R])
      case Apply(TypeApply(Select(Select(This(TypeName("scalainvoke")), TermName("FunctionReturning")), TermName("apply")), List(_)), List(unwrappedFunction)) =>
        createLiftedFunction[Environment, R](OwnerChainCorrector.splice(c)(unwrappedFunction), List(extractFunctionParamSymbols(unwrappedFunction)))
      case t =>
        c.abort(t.pos, s"Don't know how to lift $t\nRaw Tree: ${showRaw(t)}")
    }

    protected def firstAccessibleConstructorIn(tpe: Type): MethodSymbol = {
      val constructors = tpe.members.sorted.filter(_.isConstructor).map(_.asMethod)
      constructors
        .find(m => c.typecheck(q"new $tpe(...${m.paramLists.map(_.map(p => q"null.asInstanceOf[${p.typeSignature}]"))})", silent = true) != EmptyTree)
        .getOrElse(c.abort(c.enclosingPosition, s"None of the ${constructors.length} constructor(s) in $tpe seem to be accessible"))
    }

    def liftConstructorImpl[Environment: WeakTypeTag, A: WeakTypeTag]: Expr[Environment => A] = {
      val A = weakTypeOf[A]
      createLiftedFunction[Environment, A](
        function       = Select(New(TypeTree(A)), termNames.CONSTRUCTOR),
        parameterLists = firstAccessibleConstructorIn(A).paramLists
      )
    }
  }
}

/**
 * A strategy for invoking functions given an object of type [[Environment]].
 *
 * Implementers of this class must implement methods for extracting argument values from an [[Environment]].
 * The methods accept an environment and a parameter name,
 * and must have signatures such that invoking code could appear to be calling methods declared as:
 * {{{
 *   def extract[A](environment: Environment, name: String): A
 *   def extractImplicit[A](environment: Environment, name: String): A
 * }}}
 * These methods aren't explicitly declared on this class so that implementers are free to
 * add implicit parameters, use macros, etc.
 */
abstract class InvocationStrategy[Environment] { is =>

  /* Conceptual methods (see class documentation above)
  def extract[A](environment: Environment, name: String): A
  def extractImplicit[A](environment: Environment, name: String): A
  */

  /**
   * Lifts a function into one that takes an [[Environment]], uses the strategy to extract arguments,
   * invokes the original underlying function, and returns the result.
   * @tparam R the function's result type
   */
  def lift[R](function: FunctionReturning[R]): Environment => R = macro InvocationStrategy.MacroImplementations.liftImpl[Environment, R]

  /**
   * Lifts the first accessible constructor for class [[A]] into a function that takes an [[Environment]],
   * uses the strategy to extract arguments, invokes the constructor, and returns the new instance.
   */
  def liftConstructor[A]: Environment => A =
    macro InvocationStrategy.MacroImplementations.liftConstructorImpl[Environment, A]

  def method[Target]: MethodInvocationStrategy[Target] = new MethodInvocationStrategy[Target](this)
  final class MethodInvocationStrategy[Target](val strategy: is.type) {
    /**
     * Builds method invokers from prototypes of the form {{{strategy.method[A]("methodOnA")}}}
     */
    def apply(methodName: String): (Target, Environment) => _ =
      macro MethodInvoker.WhiteboxMacroImplementations.deriveByName[Target, Environment]

    /**
     * Builds method invokers from prototypes of the form {{{strategy.method[A](_.methodOnA(_, _))}}}
     * @tparam Result the method's result type
     */
    def apply[Result](prototype: Target => FunctionReturning[Result]): (Target, Environment) => Result =
      macro MethodInvoker.MacroImplementations.deriveFromEtaExpansionWithFunctionReturning[Target, Environment]

    /**
     * Syntactic shortcut for Nothing in invariant positions,
     * used to mean "we don't care about this type and will accept anything" when _ doesn't cut it.
     */
    private type * = Nothing

    /**
     * These methods build method invokers from eta-expansion prototypes of the form {{{strategy.method[A](_.methodOnA _)}}}
     * @tparam Result the method's result type
     */
    def apply[Result](prototype: (Target, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
  }
}
