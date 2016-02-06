package io.github.barnardb.scalainvoke

import scala.reflect.macros.{blackbox, whitebox}

object MethodLifter {

  class MacroImplementations(override val c: blackbox.Context) extends FunctionLifter.MacroImplementations(c) {
    import c.universe._

    protected def liftedParameters[Target: WeakTypeTag, Environment: WeakTypeTag]: Seq[Tree] =
      Seq(q"target: ${weakTypeOf[Target]}", q"environment: ${weakTypeOf[Environment]}")

    protected def liftedInvocationTarget[Target: WeakTypeTag](implicit strategy: Expr[FunctionLifter[_, _]]): Tree =
      q"target"

    protected def liftedFunctionType[Target: WeakTypeTag, Environment: WeakTypeTag](method: MethodSymbol): c.universe.Type =
      appliedType(symbolOf[(_, _) => _], weakTypeOf[Target], weakTypeOf[Environment], method.returnType)

    protected def createLiftedMethod[Target: WeakTypeTag, AES <: ArgumentExtractionStrategy : WeakTypeTag, IS <: InvocationStrategy](method: MethodSymbol): Tree = {
      implicit val wttEnvironment: WeakTypeTag[AES#Environment] = c.WeakTypeTag[AES#Environment](weakTypeOf[AES].member(TypeName("Environment")).asType.typeSignatureIn(weakTypeOf[AES]))
      implicit val strategy = findStrategy()
      require(method.owner == symbolOf[Target], s"Expected method owner type ${method.owner} == ${symbolOf[Target]}")
      c.typecheck(
        tree = q"""(..${liftedParameters[Target, AES#Environment]}) => ${q"""${liftedInvocationTarget[Target]}.$method(...${method.paramLists.map(_.map(extractParameter))})"""}""",
        pt = liftedFunctionType[Target, AES#Environment](method)
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

    def deriveByName[Target: WeakTypeTag, AES <: ArgumentExtractionStrategy : WeakTypeTag, IS <: InvocationStrategy](methodName: Expr[String]): Tree = {
      val Literal(Constant(name: String)) = methodName.tree
      createLiftedMethod[Target, AES, IS](weakTypeOf[Target].member(TermName(name)).asMethod)
    }
  }
}
