package io.github.barnardb.scalainvoke

import scala.reflect.macros.{blackbox, whitebox}

object MethodLifter {

  class MacroImplementations(override val c: blackbox.Context) extends FunctionLifter.MacroImplementations(c) {
    import c.universe._

    protected def liftedParameters[Target: WeakTypeTag](Environment: Type): Seq[Tree] =
      Seq(q"target: ${weakTypeOf[Target]}", q"environment: $Environment")

    protected def liftedInvocationTarget[Target: WeakTypeTag](implicit strategy: Expr[FunctionLifter[_, _]]): Tree =
      q"target"

    protected def liftedFunctionType[Target: WeakTypeTag](Environment: Type, method: MethodSymbol): c.universe.Type =
      appliedType(symbolOf[(_, _) => _], weakTypeOf[Target], Environment, method.returnType)

    protected def createLiftedMethod[Target: WeakTypeTag, AES <: ArgumentExtractionStrategy : WeakTypeTag, IS <: InvocationStrategy](method: MethodSymbol): Tree = {
      val Environment = weakTypeOf[AES].member(TypeName("Environment")).asType.typeSignatureIn(weakTypeOf[AES])
      implicit val strategy = findStrategy()
      require(method.owner == symbolOf[Target], s"Expected method owner type ${method.owner} == ${symbolOf[Target]}")
      c.typecheck(
        tree = q"""(..${liftedParameters[Target](Environment)}) => ${q"""${liftedInvocationTarget[Target]}.$method(...${method.paramLists.map(_.map(extractParameter))})"""}""",
        pt = liftedFunctionType[Target](Environment, method)
      )
    }

    protected def createLiftedMethod[Target: WeakTypeTag, IS <: InvocationStrategy : WeakTypeTag](aes: Expr[ArgumentExtractionStrategy], method: MethodSymbol): Tree = {
      implicit val Environment = aes.actualType.member(TypeName("Environment")).asType.typeSignatureIn(aes.actualType)
      implicit val strategy = findStrategy()
      require(method.owner == symbolOf[Target], s"Expected method owner type ${method.owner} == ${symbolOf[Target]}")
      c.typecheck(
        tree = q"""(..${liftedParameters[Target](Environment)}) => ${q"""${liftedInvocationTarget[Target]}.$method(...${method.paramLists.map(_.map(extractParameter))})"""}""",
        pt = liftedFunctionType[Target](Environment, method)
      )
    }



    def deriveFromPrototype[Target: WeakTypeTag, AES <: ArgumentExtractionStrategy : WeakTypeTag, IS <: InvocationStrategy](prototype: Tree): Tree = {
      val Function(_, Apply(methodSelection, _)) = prototype
      createLiftedMethod[Target, AES, IS](methodSelection.symbol.asMethod)
    }

    def liftMethodImplFromFunctionReturningWrappedEtaExpansion[Target: WeakTypeTag, AES <: ArgumentExtractionStrategy : WeakTypeTag, IS <: InvocationStrategy](prototype: Tree): Tree = {
      val Function(List(_), Apply(_, List(Block(List(), Function(_, Apply(methodSelection, _)))))) = prototype
      createLiftedMethod[Target, AES, IS](methodSelection.symbol.asMethod)
    }
  }

  class WhiteboxMacroImplementations(override val c: whitebox.Context) extends MacroImplementations(c) {
    import c.universe._

    def deriveByName[Target: WeakTypeTag, IS <: InvocationStrategy](methodName: Expr[String])(aes: Expr[ArgumentExtractionStrategy]): Tree = {
      val Literal(Constant(name: String)) = methodName.tree
      createLiftedMethod[Target, IS](aes, weakTypeOf[Target].member(TermName(name)).asMethod)
    }
  }
}
