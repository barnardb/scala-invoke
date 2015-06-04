package io.github.barnardb.scalainvoke

import scala.reflect.macros.blackbox

import io.github.barnardb.scalainvoke.macroutil.OwnerChainCorrector

object InvocationStrategy {
  class MacroImplementations(val c: blackbox.Context) {
    import c.universe._

    protected def typecheckedExpr[T: WeakTypeTag](tree: Tree): Expr[T] =
      c.Expr[T](c.typecheck(tree, pt = weakTypeOf[T]))

    protected def findStrategy[Environment: WeakTypeTag](tree: Tree = c.prefix.tree): Expr[InvocationStrategy[Environment]] = {
      tree match {
        case _ if tree.tpe <:< weakTypeOf[InvocationStrategy[Environment]] => typecheckedExpr[InvocationStrategy[Environment]](tree)
        case           Select(prefix, _)     => findStrategy(prefix)
        case TypeApply(Select(prefix, _), _) => findStrategy(prefix)
        case _ => c.abort(c.prefix.tree.pos, s"Can't find the InvocationStrategy in prefix. Prefix tree: ${showRaw(c.prefix.tree)}")
      }
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
      implicit val strategy = findStrategy[Environment]()
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

    def liftConstructorImpl[Environment: WeakTypeTag, T: WeakTypeTag]: Expr[Environment => T] = {
      val T = weakTypeOf[T]
      createLiftedFunction[Environment, T](
        function       = Select(New(TypeTree(T)), termNames.CONSTRUCTOR),
        parameterLists = firstAccessibleConstructorIn(T).paramLists
      )
    }
  }
}

/**
 * A strategy for invoking arbitrary functions, methods and constructors given an object of type `Environment`.
 *
 * Implementers of this class must implement methods for extracting argument values from an `Environment`.
 * The methods accept an environment and a parameter name,
 * and must have signatures such that invoking code could appear to be calling methods declared as:
 * {{{
 *   def extract[A](environment: Environment, name: String): A
 *   def extractImplicit[A](environment: Environment, name: String): A
 * }}}
 * These methods aren't explicitly declared on this class so that implementers are free to
 * add implicit parameters, use macros, etc.
 *
 * This class provides a family of methods to lift functions, methods and constructors
 * into functions that take an `Environment`, extract argument values using the extraction methods described above,
 * invoke the underlying function, method or constructor, and return the result.
 *
 * The main [[lift]] method handles function literals and eta-expanded methods.
 *
 * [[liftConstructor]] lifts the first accessible constructor of a class type.
 */
abstract class InvocationStrategy[Environment] { is =>

  /* Conceptual methods (see class documentation above)
  def extract[A](environment: Environment, name: String): A
  def extractImplicit[A](environment: Environment, name: String): A
  */

  /**
   * Lifts a function into one that takes an `Environment`, uses the strategy to extract arguments,
   * invokes the original underlying function, and returns the result.
   * @tparam R the function's result type
   */
  def lift[R](function: FunctionReturning[R]): Environment => R = macro InvocationStrategy.MacroImplementations.liftImpl[Environment, R]

  /**
   * Lifts the first accessible constructor for class `T` into a function that takes an `Environment`,
   * uses the strategy to extract arguments, invokes the constructor, and returns the new instance.
   */
  def liftConstructor[T]: Environment => T =
    macro InvocationStrategy.MacroImplementations.liftConstructorImpl[Environment, T]

  def method[Target]: MethodLifter[Target] = null  // no need to instantiate, since everything on the lifter is made of macro magic
  final abstract class MethodLifter[Target] {
    /**
     * Builds method invokers from prototypes of the form {{{strategy.method[A]("methodOnA")}}}
     */
    def apply(methodName: String): (Target, Environment) => _ =
      macro MethodInvoker.WhiteboxMacroImplementations.deriveByName[Target, Environment]

    /**
     * Lifts methods denoted by eta-expansion prototypes of the form {{{strategy.method[Target](_.methodOnTarget _)}}}
     * into binary functions that take a `Target` and an `Environment` and return a value of type `R`.
     * @tparam R the method's result type
     */
    def apply[R](prototype: Target => FunctionReturning[R]): (Target, Environment) => R =
      macro MethodInvoker.MacroImplementations.liftMethodImplFromFunctionReturningWrappedEtaExpansion[Target, Environment]

    /**
     * Syntactic shortcut for Nothing in contravariant positions,
     * used to mean "we don't care about this type and will accept anything" when _ doesn't cut it.
     */
    private type * = Nothing

    /**
     * These methods lift methods denoted by prototypes of the form {{{strategy.method[Target](_.methodOnTarget(_, _))}}}
     * into binary functions that take a `Target` and an `Environment` and return a value of type `R`.
     * @tparam R the method's result type
     */
    def apply[R](prototype: (Target, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[R](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): (Target, Environment) => R = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
  }
}
