package io.github.barnardb.scalainvoke

import scala.reflect.macros.blackbox

import io.github.barnardb.scalainvoke.macroutil.OwnerChainCorrector

/**
  * Creates functions with predictable signatures out of arbitrary functions, methods and constructors ("invocables").
  *
  * The general form of the generated functions is {{{Environment => Invocation[Return]}}} for functions, constructors,
  * methods on specific objects, and methods lifted as functions, and {{{(Target, Environment) => Invocation[Return]}}}
  * for general methods. `Environment` is the type of a value, such as a command line or HTTP request, from which
  * arguments for the underlying invocable are extracted using an implicit [[ArgumentExtractionStrategy]].
  * `Return` the invocable's return type, which is potentially wrapped by the implicit [[InvocationStrategy]] into an
  * [[InvocationStrategy.Invocation]] type.
  *
  * The created functions are written at compile time, allowing the user-selectable strategies for parameter adaptation
  * and invocation staging to fail at compile time if they don't know how to do something.
  * E.g., the [[ImplicitArgumentExtractors]] strategy will fail at compile time if it can't find an implicit of the
  * approriate [[Extractor]] supertype for one the underlying invocable's parameter's type.
  * While some failures can only be detected at runtime (e.g., *is the required command-line argument present?*,
  * *is there a query-string parameter named "foo"?*), this allows you to detect as many problems at compile time as
  * possible (e.g., *do we know how to extract a Duration from the request?*,
  * *do we have help text for parameters named "context"?*, etc.).
  *
  * To call the methods on [[FunctionLifter]], you must mix in an [[ArgumentExtractionStrategy]] and an
  * [[InvocationStrategy]]. E.g.:
  * {{{
  * implicit val argumentExtractionStrategy = new ImplicitArgumentExtractors[HttpRequest]
  * implicit val invocationStrategy = new DirectInvocation
  * }}}
  *
  * {{{
  * object Example {
  *   import io.github.barnardb.scalainvoke._
  *
  *   final class Path(override val toString: String) extends AnyVal
  *   case class HttpRequest(path: Path, query: Map[String, String])
  *
  *   implicit val argumentExtractionStrategy = new ImplicitArgumentExtractors[HttpRequest]
  *   implicit val invocationStrategy = new DirectInvocation
  *
  *   implicit object PathExtractor extends Extractor[HttpRequest, Path] {
  *     override def extract(request: HttpRequest): Path = request.path
  *     override def extract(request: HttpRequest, name: String): Path = request.path
  *   }
  *   implicit object StringExtractor extends NamedExtractor[HttpRequest, String] {
  *     override def extract(request: HttpRequest, name: String): String = request.query(name)
  *   }
  *   implicit object DoubleExtractor extends NamedExtractor[HttpRequest, Double] {
  *     override def extract(request: HttpRequest, name: String): Double = request.query(name).toDouble
  *   }
  *
  *   def action(path: Path, material: String, weight: Double): (String, Double) =
  *     s"Do something $$path-ish with $$weight of $$material" -> weight * 2
  *
  *   // This should happen once, somewhere in the setup of the program
  *   private[this] final val liftedAction = FunctionLifter.function(action _)
  *
  *   // Because we are using DeferredExecution, we have a 2-step process to invoke.
  *   // Passing the environment to the lifted action causes the parameters to be extracted and an invoker to be returned.
  *   val invoker: () => (String, Double) =
  *     liftedAction(HttpRequest(new Path("/metal/bars"), Map("material" -> "gold", "weight" -> "42.1337")))
  *
  *   // At this point we know we have successfully extracted the arguments, but haven't yet invoked the function.
  *   // We can invoke the function when we're ready.
  *   val (message: String, doubledWeight: Double) = invoker()
  *
  *   println(message)
  *   println(doubledWeight)
  * }
  * }}}
  */
object FunctionLifter {

  private val functionApplyMethodNamePattern = raw"scala\.Function[0-9]+\.apply".r.anchored

  class MacroImplementations(val c: blackbox.Context) {
    import c.universe._

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

    protected def createLiftedFunction[A: WeakTypeTag](aes: Expr[ArgumentExtractionStrategy], is: Expr[InvocationStrategy], function: Tree, parameterLists: List[List[Symbol]]): Expr[aes.value.Environment => is.value.Invocation[A]] = c.Expr[aes.value.Environment => is.value.Invocation[A]] {
      implicit val Environment = aes.actualType.member(symbolOf[aes.value.Environment].name).asType.typeSignatureIn(aes.actualType)
      implicit val WrappedReturnType = appliedType(is.actualType.member(symbolOf[is.value.Invocation[_]].name).asType.typeSignatureIn(is.actualType), List(weakTypeOf[A]))
      val namedParameterLists = parameterLists.map(_.map(p => c.freshName(p.name.asInstanceOf[TermName]) -> p))
      val invocation = q"$function(...${namedParameterLists.map(_.map{case (name, _) => q"$name"})})"
      val wrappedInvocation = q"$is.wrapInvocation[${weakTypeOf[A]}]($invocation)"
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

    def liftImpl[A: WeakTypeTag](function: Expr[_])(aes: Expr[ArgumentExtractionStrategy], is: Expr[InvocationStrategy]): Expr[aes.value.Environment => is.value.Invocation[A]] = function.tree match {
      case Block(stats, expr) =>
        import c.internal._, decorators._
        val lifted = liftImpl[A](c.Expr(expr))(aes, is)
        c.Expr[aes.value.Environment => is.value.Invocation[A]](Block(stats, lifted.tree) setType lifted.actualType)
      case Apply(TypeApply(Select(Select(This(TypeName("scalainvoke")), TermName("FunctionReturning")), TermName("apply")), List(_)), List(unwrappedFunction)) =>
        createLiftedFunction[A](aes, is, OwnerChainCorrector.splice(c)(unwrappedFunction), extractFunctionParamLists(unwrappedFunction))
      case t =>
        c.abort(t.pos, s"Don't know how to lift $t\nRaw Tree: ${showRaw(t)}")
    }

    protected def firstAccessibleConstructorIn(tpe: Type): MethodSymbol = {
      val constructors = tpe.members.sorted.filter(_.isConstructor).map(_.asMethod)
      constructors
        .find(m => c.typecheck(q"new $tpe(...${m.paramLists.map(_.map(p => q"null.asInstanceOf[${p.typeSignature}]"))})", silent = true) != EmptyTree)
        .getOrElse(c.abort(c.enclosingPosition, s"None of the ${constructors.length} constructor(s) in $tpe seem to be accessible"))
    }

    def liftConstructorImpl[A: WeakTypeTag](aes: Expr[ArgumentExtractionStrategy], is: Expr[InvocationStrategy]): Expr[aes.value.Environment => is.value.Invocation[A]] = {
      val A = weakTypeOf[A]
      createLiftedFunction[A](aes, is,
        function       = Select(New(TypeTree(A)), termNames.CONSTRUCTOR),
        parameterLists = firstAccessibleConstructorIn(A).paramLists
      )
    }
  }

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
  def function[A](function: FunctionReturning[A])(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro FunctionLifter.MacroImplementations.liftImpl[A]

  /**
   * Lifts the first accessible constructor for class `A` into a function that takes an `Environment`,
   * uses the strategy to extract arguments, invokes the constructor, and returns the new instance.
   */
  def constructor[A](implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] =
    macro FunctionLifter.MacroImplementations.liftConstructorImpl[A]

  def method[Target]: MethodLifter[Target] = null  // no need to instantiate, since everything on the lifter is made of macro magic
  final abstract class MethodLifter[Target] {
    /**
     * Builds method invokers from prototypes of the form {{{strategy.method[A]("methodOnA")}}}
     */
    def apply(methodName: String)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[_] =
      macro MethodLifter.WhiteboxMacroImplementations.deriveByName[Target]

    /**
     * Lifts methods denoted by eta-expansion prototypes of the form {{{strategy.method[Target](_.methodOnTarget _)}}}
     * into binary functions that take a `Target` and an `Environment` and return a value of type `A`.
     *
     * @tparam A the method's result type
     */
    def apply[A](prototype: Target => FunctionReturning[A])(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] =
      macro MethodLifter.MacroImplementations.liftMethodImplFromFunctionReturningWrappedEtaExpansion[Target]

    /**
     * These methods lift methods denoted by prototypes of the form {{{strategy.method[Target](_.methodOnTarget(_, _))}}}
     * into binary functions that take a `Target` and an `Environment` and return a value of type `A`.
     *
     * @tparam A the method's result type
     */
    def apply[A](prototype: (Target, *)                                                             => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *)                                                          => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *)                                                       => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *)                                                    => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *)                                                 => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *)                                              => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *)                                           => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *)                                        => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *)                                     => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *)                                  => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *)                               => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *)                            => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *)                         => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                      => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                   => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)             => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)          => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)       => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)    => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): (Target, aes.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.deriveFromPrototype[Target]
  }

  def methodAsFunction[Target]: MethodAsFunctionLifter[Target] = null  // no need to instantiate, since everything on the lifter is made of macro magic
  final abstract class MethodAsFunctionLifter[Target] {
    /**
     * Builds method invokers from prototypes of the form {{{strategy.method[A]("methodOnA")}}}
     */
    def apply(methodName: String)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[_] =
      macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveByName[Target]

    /**
     * Lifts methods denoted by eta-expansion prototypes of the form {{{strategy.method[Target](_.methodOnTarget _)}}}
     * into functions that take an `Environment` and return a value of type `A`, extracting the target from the environment.
     *
     * @tparam A the method's result type
     */
    def apply[A](prototype: Target => FunctionReturning[A])(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] =
      macro MethodAndTargetLifter.WhiteboxMacroImplementations.liftMethodImplFromFunctionReturningWrappedEtaExpansion[Target]

    /**
     * These methods lift methods denoted by prototypes of the form {{{strategy.method[Target](_.methodOnTarget(_, _))}}}
     * into functions that take an `Environment` and return a value of type `A`, extracting the target from the environment.
     *
     * @tparam A the method's result type
     */
    def apply[A](prototype: (Target, *)                                                             => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *)                                                          => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *)                                                       => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *)                                                    => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *)                                                 => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *)                                              => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *)                                           => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *)                                        => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *)                                     => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *)                                  => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *)                               => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *)                            => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *)                         => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                      => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                   => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)             => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)          => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)       => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)    => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => A)(implicit aes: ArgumentExtractionStrategy, is: InvocationStrategy): aes.Environment => is.Invocation[A] = macro MethodAndTargetLifter.WhiteboxMacroImplementations.deriveFromPrototype[Target]
  }
}
