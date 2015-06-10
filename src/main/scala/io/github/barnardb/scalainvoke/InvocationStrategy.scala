package io.github.barnardb.scalainvoke

import scala.language.higherKinds
import scala.reflect.macros.blackbox

import io.github.barnardb.scalainvoke.macroutil.OwnerChainCorrector
import io.github.barnardb.scalainvoke.macroutil.WeakTypeTagCorrector._

object InvocationStrategy {
  class MacroImplementations(val c: blackbox.Context) {
    import c.universe._

    protected def typecheckedExpr[T: WeakTypeTag](tree: Tree): Expr[T] =
      c.Expr[T](c.typecheck(tree, pt = weakTypeOf[T]))

    protected def findStrategy[Environment, R[_]](tree: Tree = c.prefix.tree)(implicit wttE: WeakTypeTag[Environment], wttIS: WeakTypeTag[InvocationStrategy[Environment, R]]): Expr[InvocationStrategy[Environment, R]] = {
      tree match {
        case _ if tree.tpe.baseType(symbolOf[InvocationStrategy[Environment, R]]) != NoType =>
          c.Expr[InvocationStrategy[Environment, R]](tree)
        case           Select(prefix, _)     => findStrategy(prefix)
        case TypeApply(Select(prefix, _), _) => findStrategy(prefix)
        case _ => c.abort(c.prefix.tree.pos, s"Can't find the InvocationStrategy in prefix. Prefix tree: ${showRaw(c.prefix.tree)}")
      }
    }

    protected def extractParameter[Environment, R[_]](parameter: Symbol)(implicit strategy: Expr[InvocationStrategy[Environment, R]]): Tree = {
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

    protected def createLiftedFunction[Environment: WeakTypeTag, R[_], RApplied <: R[_]: WeakTypeTag, A: WeakTypeTag](function: Tree, parameterLists: List[List[Symbol]]): Expr[Environment => R[A]] = c.Expr {
      implicit val wttra: WeakTypeTag[R[A]] = weakTypeTagForAppliedType[R, RApplied, A](c)
      implicit val strategy = findStrategy[Environment, R]()
      val namedParameterLists = parameterLists.map(_.map(p => c.freshName(p.name.asInstanceOf[TermName]) -> p))
      val invocation = q"$function(...${namedParameterLists.map(_.map{case (name, _) => q"$name"})})"
      val wrappedInvocation = q"$strategy.wrapInvocation[${weakTypeOf[A]}]($invocation)"
      c.typecheck(
        tree =
//          $function(...${namedParameterLists.map(_.map{case (name, _) => q"$strategy.unwrap($name)"})})
          q"""(environment: ${weakTypeOf[Environment]}) => {
              ..${namedParameterLists.flatten.map {case (name, sym) => q"val $name = ${extractParameter[Environment, R](sym)}"}}
              $wrappedInvocation
          }""",
        pt = weakTypeOf[Environment => R[A]]
      )
    }

    def liftImpl[Environment: WeakTypeTag, R[_], RApplied <: R[_]: WeakTypeTag, A: WeakTypeTag](function: Tree): Expr[Environment => R[A]] = function match {
      case Block(stats, expr) =>
        import c.internal._, decorators._
        implicit val wttra: WeakTypeTag[R[A]] = weakTypeTagForAppliedType[R, RApplied, A](c)
        c.Expr(Block(stats, liftImpl[Environment, R, RApplied, A](expr).tree) setType weakTypeOf[Environment => R[A]])
      case Apply(TypeApply(Select(Select(This(TypeName("scalainvoke")), TermName("FunctionReturning")), TermName("apply")), List(_)), List(unwrappedFunction)) =>
        createLiftedFunction[Environment, R, RApplied, A](OwnerChainCorrector.splice(c)(unwrappedFunction), List(extractFunctionParamSymbols(unwrappedFunction)))
      case t =>
        c.abort(t.pos, s"Don't know how to lift $t\nRaw Tree: ${showRaw(t)}")
    }

    protected def firstAccessibleConstructorIn(tpe: Type): MethodSymbol = {
      val constructors = tpe.members.sorted.filter(_.isConstructor).map(_.asMethod)
      constructors
        .find(m => c.typecheck(q"new $tpe(...${m.paramLists.map(_.map(p => q"null.asInstanceOf[${p.typeSignature}]"))})", silent = true) != EmptyTree)
        .getOrElse(c.abort(c.enclosingPosition, s"None of the ${constructors.length} constructor(s) in $tpe seem to be accessible"))
    }

    def liftConstructorImpl[Environment: WeakTypeTag, R[_], RApplied <: R[_]: WeakTypeTag, A: WeakTypeTag]: Expr[Environment => R[A]] = {
      val A = weakTypeOf[A]
      createLiftedFunction[Environment, R, RApplied, A](
        function       = Select(New(TypeTree(A)), termNames.CONSTRUCTOR),
        parameterLists = firstAccessibleConstructorIn(A).paramLists
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
 *   def wrapInvocation[A](invocation: A): R[A]
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
class InvocationStrategy[Environment, R[_]] {
  is: ExtractionStrategy[Environment] with ReturnValueStrategy[R] =>

  /* Conceptual methods (see class documentation above)
  def extract[A](environment: Environment, name: String): A
  def extractImplicit[A](environment: Environment, name: String): A
  */

  /**
   * Lifts a function into one that takes an `Environment`, uses the strategy to extract arguments,
   * invokes the original underlying function, and returns the result.
   * @tparam A the function's result type
   */
  def lift[A](function: FunctionReturning[A]): Environment => R[A] = macro InvocationStrategy.MacroImplementations.liftImpl[Environment, R, R[_], A]

  /**
   * Lifts the first accessible constructor for class `A` into a function that takes an `Environment`,
   * uses the strategy to extract arguments, invokes the constructor, and returns the new instance.
   */
  def liftConstructor[A]: Environment => R[A] =
    macro InvocationStrategy.MacroImplementations.liftConstructorImpl[Environment, R, R[_], A]

  def method[Target]: MethodLifter[Target] = null  // no need to instantiate, since everything on the lifter is made of macro magic
  final abstract class MethodLifter[Target] {
    /**
     * Builds method invokers from prototypes of the form {{{strategy.method[A]("methodOnA")}}}
     */
    def apply(methodName: String): (Target, Environment) => _ =
      macro MethodInvoker.WhiteboxMacroImplementations.deriveByName[Target, Environment, R]

    /**
     * Lifts methods denoted by eta-expansion prototypes of the form {{{strategy.method[Target](_.methodOnTarget _)}}}
     * into binary functions that take a `Target` and an `Environment` and return a value of type `A`.
     * @tparam A the method's result type
     */
    def apply[A](prototype: Target => FunctionReturning[A]): (Target, Environment) => A =
      macro MethodInvoker.MacroImplementations.liftMethodImplFromFunctionReturningWrappedEtaExpansion[Target, Environment, R]

    /**
     * Syntactic shortcut for Nothing in contravariant positions,
     * used to mean "we don't care about this type and will accept anything" when _ doesn't cut it.
     */
    private type * = Nothing

    /**
     * These methods lift methods denoted by prototypes of the form {{{strategy.method[Target](_.methodOnTarget(_, _))}}}
     * into binary functions that take a `Target` and an `Environment` and return a value of type `A`.
     * @tparam A the method's result type
     */
    def apply[A](prototype: (Target, *)                                                             => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *)                                                          => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *)                                                       => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *)                                                    => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *)                                                 => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *)                                              => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *)                                           => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *)                                        => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *)                                     => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *)                                  => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *)                               => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *)                            => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *)                         => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                      => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                   => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)             => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)          => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)       => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)    => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => A): (Target, Environment) => A = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment, R]
  }
}
