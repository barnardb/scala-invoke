package io.github.barnardb.scalainvoke

import scala.language.higherKinds
import scala.reflect.macros.blackbox

import io.github.barnardb.scalainvoke.macroutil.OwnerChainCorrector
import io.github.barnardb.scalainvoke.macroutil.WeakTypeTagCorrector._

object FunctionLifter {
  class MacroImplementations(val c: blackbox.Context) {
    import c.universe._

    protected def typecheckedExpr[T: WeakTypeTag](tree: Tree): Expr[T] =
      c.Expr[T](c.typecheck(tree, pt = weakTypeOf[T]))

    protected def findStrategy[Environment, R[_]](tree: Tree = c.prefix.tree)(implicit wttE: WeakTypeTag[Environment], wttIS: WeakTypeTag[FunctionLifter[Environment, R]]): Expr[FunctionLifter[Environment, R]] = {
      tree match {
        case _ if tree.tpe.baseType(symbolOf[FunctionLifter[Environment, R]]) != NoType =>
          c.Expr[FunctionLifter[Environment, R]](tree)
        case           Select(prefix, _)     => findStrategy(prefix)
        case TypeApply(Select(prefix, _), _) => findStrategy(prefix)
        case _ => c.abort(c.prefix.tree.pos, s"Can't find the InvocationStrategy in prefix. Prefix tree: ${showRaw(c.prefix.tree)}")
      }
    }

    protected def extractParameter[Environment, R[_]](parameter: Symbol)(implicit strategy: Expr[FunctionLifter[Environment, R]]): Tree = {
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
 * Creates functions with predictable signatures out of arbitrary functions, methods and constructors ("invocables").
 *
 * The general form of the functions generated is {{{E => R[A]}}} for functions and constructors as well as methods
 * on specific objects, or {{{(T, E) => R[A]}}} for methods. `E` is an "environment" type from which arguments for the
 * underlying invocable are extracted. `R` is type constructor for the lifted return value, and `A` is the return type
 * of the underlying invocable.
 *
 * The created functions are written at compile time, allowing the user-selectable strategies for parameter adaptation
 * and invocation staging to fail at compile time if they don't know how to do something.
 * E.g., the [[ImplicitArgumentExtractors]] strategy will fail at compile time if it can't find an implicit
 * [[Extractor]] for one the underlying invocable parameter types.
 * While some failures can only be detected at runtime (e.g., *is the required command-line argument present?*,
 * *is there a query-string parameter named "foo"?*), this allows you to detect as many problems at compile time as
 * possible (e.g., *do we know how to extract a Duration from a String?*,
 * *do we have help text for parameters named "fghj-id"?*).
 *
 * To instantiate this class, you must mix in an [[ArgumentExtractionStrategy]] and an [[InvocationStrategy]]. E.g.:
 * {{{
 *   new FunctionLifter[Map[String, String], Id]
 *       with ImplicitArgumentExtractors[Map[String, String]]
 *       with DirectInvocation
 * }}}
 *
 * This class provides a family of methods to lift functions, methods and constructors
 * into functions that take an `Environment`, extract argument values using the extraction methods described above,
 * invoke the underlying function, method or constructor, and return the result.
 *
 * The main [[lift]] method handles function literals and eta-expanded methods.
 *
 * [[liftConstructor]] lifts the first accessible constructor of a class type.
 */
class FunctionLifter[Environment, R[_]] {
  is: ArgumentExtractionStrategy[Environment] with InvocationStrategy[R] =>

  /* Conceptual methods (see class documentation above)
  def extract[A](environment: Environment, name: String): A
  def extractImplicit[A](environment: Environment, name: String): A
  */

  /**
   * Lifts a function into one that takes an `Environment`, uses the strategy to extract arguments,
   * invokes the original underlying function, and returns the result.
   * @tparam A the function's result type
   */
  def lift[A](function: FunctionReturning[A]): Environment => R[A] = macro FunctionLifter.MacroImplementations.liftImpl[Environment, R, R[_], A]

  /**
   * Lifts the first accessible constructor for class `A` into a function that takes an `Environment`,
   * uses the strategy to extract arguments, invokes the constructor, and returns the new instance.
   */
  def liftConstructor[A]: Environment => R[A] =
    macro FunctionLifter.MacroImplementations.liftConstructorImpl[Environment, R, R[_], A]

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
