package io.github.barnardb.scalainvoke

import scala.reflect.macros.blackbox

import io.github.barnardb.scalainvoke.macroutil.OwnerChainCorrector

object FunctionLifter {

  private val functionApplyMethodNamePattern = raw"scala\.Function[0-9]+\.apply".r.anchored

  class MacroImplementations(val c: blackbox.Context) {
    import c.universe._

    protected def typecheckedExpr[T: WeakTypeTag](tree: Tree): Expr[T] =
      c.Expr[T](c.typecheck(tree, pt = weakTypeOf[T]))

    protected def findStrategy(tree: Tree = c.prefix.tree): Expr[FunctionLifter[_, _]] =
      if (tree.tpe.baseType(symbolOf[FunctionLifter[_, _]]) != NoType) c.Expr(tree)
      else tree match {
        case           Select(prefix, _)     => findStrategy(prefix)
        case TypeApply(Select(prefix, _), _) => findStrategy(prefix)
        case _ => c.abort(c.prefix.tree.pos, s"Can't find the InvocationStrategy in prefix. Prefix tree: ${showRaw(c.prefix.tree)}")
      }

    protected def extractParameter(parameter: Symbol)(implicit strategy: Expr[FunctionLifter[_, _]]): Tree =
      extractParameter(c.Expr(q"$strategy.argumentExtractionStrategy"), parameter)

    protected def extractParameter(aes: Expr[ArgumentExtractionStrategy], parameter: Symbol): Tree =
      if (functionApplyMethodNamePattern.findFirstIn(parameter.owner.fullName).isDefined)
        extractUnnamed(aes, parameter.typeSignature)
      else {
        val extractionMethod =
          if (parameter.isImplicit) TermName("extractImplicit")
          else TermName("extract")
        q"$aes.$extractionMethod[${parameter.typeSignature}](environment, ${parameter.name.toString})"
      }

    protected def extractUnnamed(aes: Expr[ArgumentExtractionStrategy], tpe: Type): Tree =
      q"$aes.extract[$tpe](environment)"

    protected def extractFunctionParamLists(tree: Tree): List[List[Symbol]] = tree match {
      case Function(params, _) => List(params.map(_.symbol))
      case Block(_, expr)      => extractFunctionParamLists(expr)
      case Typed(expr, _)      => extractFunctionParamLists(expr)
      case Ident(_)            =>
        val typeSignature = tree.symbol.typeSignature
        typeSignature.member(TermName("apply")).typeSignatureIn(typeSignature).paramLists
    }

    protected def createLiftedFunction[AES <: ArgumentExtractionStrategy : WeakTypeTag, IS <: InvocationStrategy : WeakTypeTag, A: WeakTypeTag](function: Tree, parameterLists: List[List[Symbol]]): Expr[AES#Environment => IS#Wrapped[A]] = c.Expr {
      implicit val wttEnvironment: WeakTypeTag[AES#Environment] = c.WeakTypeTag[AES#Environment](weakTypeOf[AES].member(TypeName("Environment")).asType.typeSignatureIn(weakTypeOf[AES]))
      implicit val wttra: WeakTypeTag[IS#Wrapped[A]] = c.WeakTypeTag[IS#Wrapped[A]](appliedType(weakTypeOf[IS].member(TypeName("Wrapped")).asType.typeSignatureIn(weakTypeOf[IS]), List(weakTypeOf[A])))
      implicit val strategy = findStrategy()
      val namedParameterLists = parameterLists.map(_.map(p => c.freshName(p.name.asInstanceOf[TermName]) -> p))
      val invocation = q"$function(...${namedParameterLists.map(_.map{case (name, _) => q"$name"})})"
      val wrappedInvocation = q"$strategy.invocationStrategy.wrapInvocation[${weakTypeOf[A]}]($invocation)"
      c.typecheck(
        tree =
//          $function(...${namedParameterLists.map(_.map{case (name, _) => q"$strategy.unwrap($name)"})})
          q"""(environment: ${weakTypeOf[AES#Environment]}) => {
              ..${namedParameterLists.flatten.map {case (name, sym) => q"val $name = ${extractParameter(sym)}"}}
              $wrappedInvocation
          }""",
        pt = weakTypeOf[AES#Environment => IS#Wrapped[A]]
      )
    }

    protected def createLiftedFunction[IS <: InvocationStrategy : WeakTypeTag, A: WeakTypeTag](aes: Expr[ArgumentExtractionStrategy], function: Tree, parameterLists: List[List[Symbol]]): Expr[aes.value.Environment => IS#Wrapped[A]] = c.Expr {
      implicit val Environment = aes.actualType.member(TypeName("Environment")).asType.typeSignatureIn(aes.actualType)
      implicit val WrappedReturnType = appliedType(weakTypeOf[IS].member(TypeName("Wrapped")).asType.typeSignatureIn(weakTypeOf[IS]), List(weakTypeOf[A]))
      implicit val strategy = findStrategy()
      val namedParameterLists = parameterLists.map(_.map(p => c.freshName(p.name.asInstanceOf[TermName]) -> p))
      val invocation = q"$function(...${namedParameterLists.map(_.map{case (name, _) => q"$name"})})"
      val wrappedInvocation = q"$strategy.invocationStrategy.wrapInvocation[${weakTypeOf[A]}]($invocation)"
      c.typecheck(
        tree =
//          $function(...${namedParameterLists.map(_.map{case (name, _) => q"$strategy.unwrap($name)"})})
          q"""(environment: $Environment) => {
              ..${namedParameterLists.flatten.map {case (name, sym) => q"val $name = ${extractParameter(aes, sym)}"}}
              $wrappedInvocation
          }""",
        pt = appliedType(typeOf[_ => _], Environment, WrappedReturnType)
      )
    }

    def liftImpl[IS <: InvocationStrategy : WeakTypeTag, A: WeakTypeTag](function: Expr[_])(aes: Expr[ArgumentExtractionStrategy]): Expr[aes.value.Environment => IS#Wrapped[A]] = function.tree match {
      case Block(stats, expr) =>
        import c.internal._, decorators._
        val lifted = liftImpl[IS, A](c.Expr(expr))(aes)
        c.Expr(Block(stats, lifted.tree) setType lifted.actualType)
      case Apply(TypeApply(Select(Select(This(TypeName("scalainvoke")), TermName("FunctionReturning")), TermName("apply")), List(_)), List(unwrappedFunction)) =>
        createLiftedFunction[IS, A](aes, OwnerChainCorrector.splice(c)(unwrappedFunction), extractFunctionParamLists(unwrappedFunction))
      case t =>
        c.abort(t.pos, s"Don't know how to lift $t\nRaw Tree: ${showRaw(t)}")
    }

    protected def firstAccessibleConstructorIn(tpe: Type): MethodSymbol = {
      val constructors = tpe.members.sorted.filter(_.isConstructor).map(_.asMethod)
      constructors
        .find(m => c.typecheck(q"new $tpe(...${m.paramLists.map(_.map(p => q"null.asInstanceOf[${p.typeSignature}]"))})", silent = true) != EmptyTree)
        .getOrElse(c.abort(c.enclosingPosition, s"None of the ${constructors.length} constructor(s) in $tpe seem to be accessible"))
    }

    def liftConstructorImpl[IS <: InvocationStrategy : WeakTypeTag, A: WeakTypeTag](aes: Expr[ArgumentExtractionStrategy]): Expr[aes.value.Environment => IS#Wrapped[A]] = {
      val A = weakTypeOf[A]
      createLiftedFunction[IS, A](
        aes            = aes,
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
class FunctionLifter[AES <: ArgumentExtractionStrategy, IS <: InvocationStrategy](val argumentExtractionStrategy: AES, val invocationStrategy: IS) {

  /* Conceptual methods (see class documentation above)
  def extract[A](environment: Environment, name: String): A
  def extractImplicit[A](environment: Environment, name: String): A
  */

  /**
   * Syntactic shortcut for Nothing in contravariant positions,
   * used to mean "we don't care about this type and will accept anything" when _ doesn't cut it.
   */
  private type * = Nothing

  /**
   * Lifts a function into one that takes an `Environment`, uses the strategy to extract arguments,
   * invokes the original underlying function, and returns the result.
   *
   * @tparam A the function's result type
   */
  def lift[A](function: FunctionReturning[A])(implicit aes: ArgumentExtractionStrategy): aes.Environment => IS#Wrapped[A] = macro FunctionLifter.MacroImplementations.liftImpl[IS, A]

  /**
   * Lifts the first accessible constructor for class `A` into a function that takes an `Environment`,
   * uses the strategy to extract arguments, invokes the constructor, and returns the new instance.
   */
  def liftConstructor[A](implicit aes: ArgumentExtractionStrategy): aes.Environment => IS#Wrapped[A] =
    macro FunctionLifter.MacroImplementations.liftConstructorImpl[IS, A]

  def liftMethod[Target]: MethodLifter[Target] = null  // no need to instantiate, since everything on the lifter is made of macro magic
  final abstract class MethodLifter[Target] {
    /**
     * Builds method invokers from prototypes of the form {{{strategy.method[A]("methodOnA")}}}
     */
    def apply(methodName: String)(implicit aes: ArgumentExtractionStrategy): (Target, aes.Environment) => IS#Wrapped[_] =
      macro MethodLifter.WhiteboxMacroImplementations.deriveByName[Target, IS]

    /**
     * Lifts methods denoted by eta-expansion prototypes of the form {{{strategy.method[Target](_.methodOnTarget _)}}}
     * into binary functions that take a `Target` and an `Environment` and return a value of type `A`.
     *
     * @tparam A the method's result type
     */
    def apply[A](prototype: Target => FunctionReturning[A]): (Target, AES#Environment) => IS#Wrapped[A] =
      macro MethodLifter.MacroImplementations.liftMethodImplFromFunctionReturningWrappedEtaExpansion[Target, AES, IS]

    /**
     * These methods lift methods denoted by prototypes of the form {{{strategy.method[Target](_.methodOnTarget(_, _))}}}
     * into binary functions that take a `Target` and an `Environment` and return a value of type `A`.
     *
     * @tparam A the method's result type
     */
    def apply[A](prototype: (Target, *)                                                             => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *)                                                          => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *)                                                       => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *)                                                    => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *)                                                 => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *)                                              => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *)                                           => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *)                                        => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *)                                     => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *)                                  => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *)                               => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *)                            => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *)                         => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                      => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                   => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)             => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)          => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)       => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)    => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => A): (Target, AES#Environment) => IS#Wrapped[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
  }

  def liftMethodAndTarget[Target]: MethodAndTargetLifter[Target] = null  // no need to instantiate, since everything on the lifter is made of macro magic
  final abstract class MethodAndTargetLifter[Target] {
    /**
     * Builds method invokers from prototypes of the form {{{strategy.method[A]("methodOnA")}}}
     */
    def apply(methodName: String)(implicit aes: ArgumentExtractionStrategy): aes.Environment => IS#Wrapped[_] =
      macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveByName[Target, IS]

    /**
     * Lifts methods denoted by eta-expansion prototypes of the form {{{strategy.method[Target](_.methodOnTarget _)}}}
     * into functions that take an `Environment` and return a value of type `A`, extracting the target from the environment.
     *
     * @tparam A the method's result type
     */
    def apply[A](prototype: Target => FunctionReturning[A]): AES#Environment => IS#Wrapped[A] =
      macro MethodAndTargetLifter.MacroImplementations.liftMethodImplFromFunctionReturningWrappedEtaExpansion[Target, AES, IS]

    /**
     * These methods lift methods denoted by prototypes of the form {{{strategy.method[Target](_.methodOnTarget(_, _))}}}
     * into functions that take an `Environment` and return a value of type `A`, extracting the target from the environment.
     *
     * @tparam A the method's result type
     */
    def apply[A](prototype: (Target, *)                                                             => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *)                                                          => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *)                                                       => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *)                                                    => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *)                                                 => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *)                                              => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *)                                           => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *)                                        => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *)                                     => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *)                                  => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *)                               => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *)                            => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *)                         => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                      => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                   => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)             => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)          => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)       => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)    => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => A): AES#Environment => IS#Wrapped[A] = macro MethodAndTargetLifter.MacroImplementations.deriveFromPrototype[Target, AES, IS]
  }
}
