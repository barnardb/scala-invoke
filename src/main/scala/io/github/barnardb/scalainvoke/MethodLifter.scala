package io.github.barnardb.scalainvoke

import scala.reflect.macros.{blackbox, whitebox}

object MethodLifter {

  class MacroImplementations(override val c: blackbox.Context) extends FunctionLifter.MacroImplementations(c) {
    import c.universe._

    protected def liftedParameters[Target: WeakTypeTag](Environment: Type): Seq[Tree] =
      Seq(q"target: ${weakTypeOf[Target]}", q"environment: $Environment")

    protected def liftedInvocationTarget[Target: WeakTypeTag](aes: Expr[ArgumentExtractionStrategy]): Tree =
      q"target"

    protected def liftedFunctionType[Target: WeakTypeTag](Environment: Type, method: MethodSymbol): c.universe.Type =
      appliedType(symbolOf[(_, _) => _], weakTypeOf[Target], Environment, method.returnType)

    protected def createLiftedMethod[Target: WeakTypeTag](aes: Expr[ArgumentExtractionStrategy], is: Expr[InvocationStrategy], method: MethodSymbol): Tree = {
      implicit val Environment = aes.actualType.member(TypeName("Environment")).asType.typeSignatureIn(aes.actualType)
      require(method.owner == symbolOf[Target], s"Expected method owner type ${method.owner} == ${symbolOf[Target]}")
      c.typecheck(
        tree = q"""(..${liftedParameters[Target](Environment)}) => ${q"""${liftedInvocationTarget[Target](aes)}.$method(...${method.paramLists.map(_.map(extractParameter(aes, _)))})"""}""",
        pt = liftedFunctionType[Target](Environment, method)
      )
    }



    def deriveFromPrototype[Target: WeakTypeTag](prototype: Tree)(aes: Expr[ArgumentExtractionStrategy], is: Expr[InvocationStrategy]): Tree = {
      val Function(_, Apply(methodSelection, _)) = prototype
      createLiftedMethod[Target](aes, is, methodSelection.symbol.asMethod)
    }

    def liftMethodImplFromFunctionReturningWrappedEtaExpansion[Target: WeakTypeTag](prototype: Tree)(aes: Expr[ArgumentExtractionStrategy], is: Expr[InvocationStrategy]): Tree = {
      val Function(List(_), Apply(_, List(Block(List(), Function(_, Apply(methodSelection, _)))))) = prototype
      createLiftedMethod[Target](aes, is, methodSelection.symbol.asMethod)
    }
  }

  class WhiteboxMacroImplementations(override val c: whitebox.Context) extends MacroImplementations(c) {
    import c.universe._

    def deriveByName[Target: WeakTypeTag](methodName: Expr[String])(aes: Expr[ArgumentExtractionStrategy], is: Expr[InvocationStrategy]): Tree = {
      val Literal(Constant(name: String)) = methodName.tree
      createLiftedMethod[Target](aes, is, weakTypeOf[Target].member(TermName(name)).asMethod)
    }
  }
}
