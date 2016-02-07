package io.github.barnardb.scalainvoke

import scala.reflect.macros.{whitebox, blackbox}

import io.github.barnardb.scalainvoke.macroutil.OwnerChainCorrector

/**
  * Creates functions with predictable signatures out of arbitrary functions, methods and constructors ("invocables").
  *
  * The general form of the generated functions is {{{Environment => Invocation[Return]}}} for functions, constructors,
  * methods on specific objects, and methods lifted as functions, and {{{(Target, Environment) => Invocation[Return]}}}
  * for general methods. `Environment` is the type of a value, such as a command line or HTTP request, from which
  * arguments for the underlying invocable are extracted using an implicit [[ValueExtractionStrategy]].
  * `Return` the invocable's return type, which is potentially wrapped by the implicit [[InvocationStrategy]] into an
  * [[InvocationStrategy.Invocation]] type.
  *
  * The created functions are written at compile time, allowing the user-selectable strategies for parameter adaptation
  * and invocation staging to fail at compile time if they don't know how to do something.
  * E.g., the [[ImplicitlyDiscoveredValueExtractors]] strategy will fail at compile time if it can't find an implicit of the
  * approriate [[ValueExtractor]] supertype for one the underlying invocable's parameter's type.
  * While some failures can only be detected at runtime (e.g., *is the required command-line argument present?*,
  * *is there a query-string parameter named "foo"?*), this allows you to detect as many problems at compile time as
  * possible (e.g., *do we know how to extract a Duration from the request?*,
  * *do we have help text for parameters named "context"?*, etc.).
  *
  * To call the methods on [[Lift]], you must mix in an [[ValueExtractionStrategy]] and an
  * [[InvocationStrategy]]. E.g.:
  * {{{
  * implicit val valueExtractionStrategy = new ImplicitlyDiscoveredValueExtractors[HttpRequest]
  * implicit val invocationStrategy = new DirectInvocation
  * }}}
  *
  * Here's an example that gives an idea of how to use [[Lift]].
  *
  * {{{
  * object Example {
  *   import io.github.barnardb.scalainvoke._
  *
  *   final class Path(override val toString: String) extends AnyVal
  *   case class HttpRequest(path: Path, query: Map[String, String])
  *
  *   implicit val valueExtractionStrategy = new ImplicitlyDiscoveredValueExtractors[HttpRequest]
  *   implicit val invocationStrategy = new DeferredInvocation
  *
  *   implicit object PathExtractor extends ValueExtractor[HttpRequest, Path] {
  *     override def extract(request: HttpRequest): Path = request.path
  *     override def extract(request: HttpRequest, name: String): Path = request.path
  *   }
  *   implicit object StringExtractor extends NamedValueExtractor[HttpRequest, String] {
  *     override def extract(request: HttpRequest, name: String): String = request.query(name)
  *   }
  *   implicit object DoubleExtractor extends NamedValueExtractor[HttpRequest, Double] {
  *     override def extract(request: HttpRequest, name: String): Double = request.query(name).toDouble
  *   }
  *
  *   def action(path: Path, material: String, weight: Double): (String, Double) =
  *     "Do something with " + path.group + " and " + material -> weight * 2
  *
  *   // This should happen once, somewhere in the setup of the program
  *   private[this] final val liftedAction = Lift.function(action _)
  *
  *   // Because we are using DeferredExecution, we have a 2-step process to invoke.
  *
  *   // Passing the environment to the lifted action causes the parameters to be extracted and an invoker to be returned.
  *   val invoker: () => (String, Double) =
  *     liftedAction(HttpRequest(new Path("/metal/bars"), Map("material" -> "gold", "weight" -> "42.1337")))
  *
  *   // At this point we know we have successfully extracted the arguments, but haven't yet invoked the function.
  *
  *   // We can invoke the function when we're ready.
  *   val (message: String, doubledWeight: Double) = invoker()
  * }
  * }}}
  */
object Lift {

  private val functionApplyMethodNamePattern = raw"scala\.Function[0-9]+\.apply".r.anchored

  class MacroImplementations(val c: blackbox.Context) {
    import c.universe._

    private final def extractUnnamedValue(ves: Expr[ValueExtractionStrategy], tpe: Type): Tree =
      q"$ves.extract[$tpe](environment)"

    protected final def extractArgument(ves: Expr[ValueExtractionStrategy], parameter: Symbol): Tree =
      if (functionApplyMethodNamePattern.findFirstIn(parameter.owner.fullName).isDefined)
        extractUnnamedValue(ves, parameter.typeSignature)
      else {
        val extractionMethod =
          if (parameter.isImplicit) TermName("extractImplicit")
          else TermName("extract")
        q"$ves.$extractionMethod[${parameter.typeSignature}](environment, ${parameter.name.toString})"
      }

    private final def extractFunctionParamLists(tree: Tree): List[List[Symbol]] = tree match {
      case Function(params, _) => List(params.map(_.symbol))
      case Block(_, expr)      => extractFunctionParamLists(expr)
      case Typed(expr, _)      => extractFunctionParamLists(expr)
      case Ident(_)            =>
        val typeSignature = tree.symbol.typeSignature
        typeSignature.member(TermName("apply")).typeSignatureIn(typeSignature).paramLists
    }

    private final def createLiftedFunction[A: WeakTypeTag](ves: Expr[ValueExtractionStrategy], is: Expr[InvocationStrategy], function: Tree, parameterLists: List[List[Symbol]]): Expr[ves.value.Environment => is.value.Invocation[A]] = c.Expr[ves.value.Environment => is.value.Invocation[A]] {
      implicit val Environment = ves.actualType.member(symbolOf[ves.value.Environment].name).asType.typeSignatureIn(ves.actualType)
      implicit val WrappedReturnType = appliedType(is.actualType.member(symbolOf[is.value.Invocation[_]].name).asType.typeSignatureIn(is.actualType), List(weakTypeOf[A]))
      val namedParameterLists = parameterLists.map(_.map(p => c.freshName(p.name.asInstanceOf[TermName]) -> p))
      val invocation = q"$function(...${namedParameterLists.map(_.map{case (name, _) => q"$name"})})"
      val wrappedInvocation = q"$is.wrapInvocation[${weakTypeOf[A]}]($invocation)"
      c.typecheck(
        tree =
//          $function(...${namedParameterLists.map(_.map{case (name, _) => q"$strategy.unwrap($name)"})})
          q"""(environment: $Environment) => {
              ..${namedParameterLists.flatten.map {case (name, sym) => q"val $name = ${extractArgument(ves, sym)}"}}
              $wrappedInvocation
          }""",
        pt = appliedType(typeOf[_ => _], Environment, WrappedReturnType)
      )
    }

    final def liftFunction[F: WeakTypeTag, A: WeakTypeTag](function: Expr[F])(evidence: Expr[FunctionReturning[F, A]], ves: Expr[ValueExtractionStrategy], is: Expr[InvocationStrategy]): Expr[ves.value.Environment => is.value.Invocation[A]] = function.tree match {
      case Block(stats, expr) =>
        import c.internal._, decorators._
        val lifted = liftFunction[F, A](c.Expr(expr))(evidence, ves, is)
        c.Expr[ves.value.Environment => is.value.Invocation[A]](Block(stats, lifted.tree) setType lifted.actualType)
      case Function(_, _) | Typed(Function(_, _), _) | Ident(TermName(_)) =>
        createLiftedFunction[A](ves, is, OwnerChainCorrector.splice(c)(function.tree), extractFunctionParamLists(function.tree))
      case t =>
        c.abort(t.pos, s"Don't know how to lift $t\nRaw Tree: ${showRaw(t)}")
    }

    private final def firstAccessibleConstructor(tpe: Type): MethodSymbol = {
      val constructors = tpe.members.sorted.filter(_.isConstructor).map(_.asMethod)
      constructors
        .find(m => c.typecheck(q"new $tpe(...${m.paramLists.map(_.map(p => q"null.asInstanceOf[${p.typeSignature}]"))})", silent = true) != EmptyTree)
        .getOrElse(c.abort(c.enclosingPosition, s"None of the ${constructors.length} constructor(s) in $tpe seem to be accessible"))
    }

    final def liftConstructor[A: WeakTypeTag](ves: Expr[ValueExtractionStrategy], is: Expr[InvocationStrategy]): Expr[ves.value.Environment => is.value.Invocation[A]] = {
      val A = weakTypeOf[A]
      createLiftedFunction[A](ves, is,
        function       = Select(New(TypeTree(A)), termNames.CONSTRUCTOR),
        parameterLists = firstAccessibleConstructor(A).paramLists
      )
    }
  }

  /**
    * Syntactic shortcut for [[Nothing]] in contravariant positions,
    * used to mean "we don't care about this type and will accept anything".
    * Note that {{{_}}} doesn't cut it, as it creates nested existentials.
    */
  private type * = Nothing

  /**
    * Lifts a function into one that takes an `Environment`, uses the strategy to extract arguments,
    * invokes the original underlying function, and returns the result.
    *
    * @tparam F the function type
    * @tparam A the function's result type
    */
  def function[F, A](function: F)(implicit evidence: FunctionReturning[F, A], ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro Lift.MacroImplementations.liftFunction[F, A]

  /**
   * Lifts the first accessible constructor for class `A` into a function that takes an `Environment`,
   * uses the strategy to extract arguments, invokes the constructor, and returns the new instance.
   */
  def constructor[A](implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] =
    macro Lift.MacroImplementations.liftConstructor[A]

  def method[Target]: MethodLifter[Target] = null  // no need to instantiate, since everything on the lifter is made of macro magic

  final abstract class MethodLifter[Target] {
    /**
     * Builds method invokers from prototypes of the form {{{strategy.method[A]("methodOnA")}}}
     */
    def apply(methodName: String)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[_] =
      macro MethodLifter.WhiteboxMacroImplementations.liftFromName[Target]

    /**
      * Lifts methods denoted by eta-expansion prototypes of the form {{{strategy.method[Target](_.methodOnTarget _)}}}
      * into binary functions that take a `Target` and an `Environment` and return a value of type `A`.
      *
      * @tparam F the type of the methods's eta expansion
      * @tparam A the method's result type
      */
    def apply[F, A](prototype: Target => F)(implicit evidence: FunctionReturning[F, A], ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] =
      macro MethodLifter.MacroImplementations.liftFromEtaExpansion[Target, F, A]

    /**
     * These methods lift methods denoted by prototypes of the form {{{strategy.method[Target](_.methodOnTarget(_, _))}}}
     * into binary functions that take a `Target` and an `Environment` and return a value of type `A`.
     *
     * @tparam A the method's result type
     */
    def apply[A](prototype: (Target, *)                                                             => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *)                                                          => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *)                                                       => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *)                                                    => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *)                                                 => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *)                                              => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *)                                           => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *)                                        => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *)                                     => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *)                                  => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *)                               => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *)                            => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *)                         => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                      => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                   => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)             => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)          => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)       => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)    => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): (Target, ves.Environment) => is.Invocation[A] = macro MethodLifter.MacroImplementations.liftFromPrototype[Target]
  }

  object MethodLifter {

    class MacroImplementations(override val c: blackbox.Context) extends Lift.MacroImplementations(c) {
      import c.universe._

      protected def liftedFunctionType[Target: WeakTypeTag](Environment: Type, method: MethodSymbol): c.universe.Type =
        appliedType(symbolOf[(_, _) => _], weakTypeOf[Target], Environment, method.returnType)

      protected def createMethodInvoker(Target: Type, Environment: Type, ves: Expr[ValueExtractionStrategy], invokeOnTarget: Tree => Tree) =
        q"""(target: $Target, environment: $Environment) => ${invokeOnTarget(q"target")}"""

      protected final def createLiftedMethod[Target: WeakTypeTag](ves: Expr[ValueExtractionStrategy], is: Expr[InvocationStrategy], method: MethodSymbol): Tree = {
        require(method.owner == symbolOf[Target], s"Expected method owner type ${method.owner} == ${symbolOf[Target]}")
        val Environment = ves.actualType.member(TypeName("Environment")).asType.typeSignatureIn(ves.actualType)
        val invoker = createMethodInvoker(weakTypeOf[Target], Environment, ves,
          target => q"""$target.$method(...${method.paramLists.map(_.map(extractArgument(ves, _)))})"""
        )
        c.typecheck(invoker, pt = liftedFunctionType[Target](Environment, method))
      }

      final def liftFromPrototype[Target: WeakTypeTag](prototype: Tree)(ves: Expr[ValueExtractionStrategy], is: Expr[InvocationStrategy]): Tree = {
        val Function(_, Apply(methodSelection, _)) = prototype
        createLiftedMethod[Target](ves, is, methodSelection.symbol.asMethod)
      }

      final def liftFromEtaExpansion[Target: WeakTypeTag, F: WeakTypeTag, A: WeakTypeTag](prototype: Tree)(evidence: Expr[FunctionReturning[F, A]], ves: Expr[ValueExtractionStrategy], is: Expr[InvocationStrategy]): Tree = {
        val Function(List(_), Block(List(), Function(_/*params*/, Apply(methodSelection, _/*args*/)))) = prototype
        createLiftedMethod[Target](ves, is, methodSelection.symbol.asMethod)
      }
    }

    class WhiteboxMacroImplementations(override val c: whitebox.Context) extends MacroImplementations(c) {
      import c.universe._

      final def liftFromName[Target: WeakTypeTag](methodName: Expr[String])(ves: Expr[ValueExtractionStrategy], is: Expr[InvocationStrategy]): Tree = {
        val Literal(Constant(name: String)) = methodName.tree
        createLiftedMethod[Target](ves, is, weakTypeOf[Target].member(TermName(name)).asMethod)
      }
    }
  }

  def methodAsFunction[Target]: MethodAsFunctionLifter[Target] = null  // no need to instantiate, since everything on the lifter is made of macro magic

  final abstract class MethodAsFunctionLifter[Target] {
    /**
     * Builds method invokers from prototypes of the form {{{strategy.method[A]("methodOnA")}}}
     */
    def apply(methodName: String)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[_] =
      macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromName[Target]

    /**
     * Lifts methods denoted by eta-expansion prototypes of the form {{{strategy.method[Target](_.methodOnTarget _)}}}
     * into functions that take an `Environment` and return a value of type `A`, extracting the target from the environment.
     *
     * @tparam A the method's result type
     */
    def apply[F, A](prototype: Target => F)(implicit evidence: FunctionReturning[F, A], ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] =
      macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromEtaExpansion[Target, F, A]

    /**
     * These methods lift methods denoted by prototypes of the form {{{strategy.method[Target](_.methodOnTarget(_, _))}}}
     * into functions that take an `Environment` and return a value of type `A`, extracting the target from the environment.
     *
     * @tparam A the method's result type
     */
    def apply[A](prototype: (Target, *)                                                             => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *)                                                          => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *)                                                       => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *)                                                    => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *)                                                 => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *)                                              => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *)                                           => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *)                                        => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *)                                     => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *)                                  => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *)                               => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *)                            => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *)                         => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                      => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                   => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)                => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)             => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)          => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)       => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *)    => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
    def apply[A](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => A)(implicit ves: ValueExtractionStrategy, is: InvocationStrategy): ves.Environment => is.Invocation[A] = macro MethodAsFunctionLifter.WhiteboxMacroImplementations.liftFromPrototype[Target]
  }

  object MethodAsFunctionLifter {

    class WhiteboxMacroImplementations(override val c: whitebox.Context) extends MethodLifter.WhiteboxMacroImplementations(c) {
      import c.universe._

      override def createMethodInvoker(Target: Type, Environment: Type, ves: Expr[ValueExtractionStrategy], invokeOnTarget: Tree => Tree) =
        q"""(environment: $Environment) => ${invokeOnTarget(q"$ves.extract[$Target](environment)")}"""

      override def liftedFunctionType[Target: WeakTypeTag](Environment: Type, method: MethodSymbol): c.universe.Type =
        appliedType(symbolOf[_ => _], Environment, method.returnType)

    }

  }

}
