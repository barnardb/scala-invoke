package io.github.barnardb.scalainvoke

import scala.reflect.macros.{blackbox, whitebox}

object MethodLifter {

  class MacroImplementations(override val c: blackbox.Context) extends FunctionLifter.MacroImplementations(c) {
    import c.universe._

    protected def liftedParameters[Target: WeakTypeTag, Environment: WeakTypeTag]: Seq[Tree] =
      Seq(q"target: ${weakTypeOf[Target]}", q"environment: ${weakTypeOf[Environment]}")

    protected def liftedInvocationTarget[Target: WeakTypeTag, Environment: WeakTypeTag, IS <: InvocationStrategy](implicit strategy: Expr[FunctionLifter[Environment, IS]]): Tree =
      q"target"

    protected def liftedFunctionType[Target: WeakTypeTag, Environment: WeakTypeTag, IS <: InvocationStrategy](method: MethodSymbol): c.universe.Type =
      appliedType(symbolOf[(_, _) => _], weakTypeOf[Target], weakTypeOf[Environment], method.returnType)

    protected def createLiftedMethod[Target: WeakTypeTag, Environment: WeakTypeTag, IS <: InvocationStrategy](method: MethodSymbol): Tree = {
      implicit val strategy: Expr[FunctionLifter[Environment, IS]] = findStrategy[Environment, IS]()
      require(method.owner == symbolOf[Target], s"Expected method owner type ${method.owner} == ${symbolOf[Target]}")
      c.typecheck(
        tree = q"""(..${liftedParameters[Target, Environment]}) => ${q"""${liftedInvocationTarget[Target, Environment, IS]}.$method(...${method.paramLists.map(_.map(extractParameter[Environment, IS]))})"""}""",
        pt = liftedFunctionType[Target, Environment, IS](method)
      )
    }

    def deriveFromPrototype[Target: WeakTypeTag, Environment: WeakTypeTag, IS <: InvocationStrategy](prototype: Tree): Tree = {
      val Function(_, Apply(methodSelection, _)) = prototype
      createLiftedMethod[Target, Environment, IS](methodSelection.symbol.asMethod)
    }

    def liftMethodImplFromFunctionReturningWrappedEtaExpansion[Target: WeakTypeTag, Environment: WeakTypeTag, IS <: InvocationStrategy](prototype: Tree): Tree = {
      val Function(List(_), Apply(_, List(Block(List(), Function(_, Apply(methodSelection, _)))))) = prototype
      createLiftedMethod[Target, Environment, IS](methodSelection.symbol.asMethod)
    }
  }

  class WhiteboxMacroImplementations(override val c: whitebox.Context) extends MacroImplementations(c) {
    import c.universe._

    def deriveByName[Target: WeakTypeTag, Environment: WeakTypeTag, IS <: InvocationStrategy](methodName: Expr[String]): Tree = {
      val Literal(Constant(name: String)) = methodName.tree
      createLiftedMethod[Target, Environment, IS](weakTypeOf[Target].member(TermName(name)).asMethod)
    }
  }
}
