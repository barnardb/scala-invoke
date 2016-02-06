package io.github.barnardb.scalainvoke

import scala.language.higherKinds
import scala.reflect.macros.{blackbox, whitebox}

object MethodLifter {

  class MacroImplementations(override val c: blackbox.Context) extends FunctionLifter.MacroImplementations(c) {
    import c.universe._

    protected def liftedParameters[Target: WeakTypeTag, Environment: WeakTypeTag]: Seq[Tree] =
      Seq(q"target: ${weakTypeOf[Target]}", q"environment: ${weakTypeOf[Environment]}")

    protected def liftedInvocationTarget[Target: WeakTypeTag, Environment: WeakTypeTag, R[_]](implicit strategy: Expr[FunctionLifter[Environment, R]]): Tree =
      q"target"

    protected def liftedFunctionType[Target: WeakTypeTag, Environment: WeakTypeTag, R[_]](method: MethodSymbol): c.universe.Type =
      appliedType(symbolOf[(_, _) => _], weakTypeOf[Target], weakTypeOf[Environment], method.returnType)

    protected def createLiftedMethod[Target: WeakTypeTag, Environment: WeakTypeTag, R[_]](method: MethodSymbol): Tree = {
      implicit val strategy: Expr[FunctionLifter[Environment, R]] = findStrategy[Environment, R]()
      require(method.owner == symbolOf[Target], s"Expected method owner type ${method.owner} == ${symbolOf[Target]}")
      c.typecheck(
        tree = q"""(..${liftedParameters[Target, Environment]}) => ${q"""${liftedInvocationTarget[Target, Environment, R]}.$method(...${method.paramLists.map(_.map(extractParameter[Environment, R]))})"""}""",
        pt = liftedFunctionType[Target, Environment, R](method)
      )
    }

    def deriveFromPrototype[Target: WeakTypeTag, Environment: WeakTypeTag, R[_]](prototype: Tree): Tree = {
      val Function(_, Apply(methodSelection, _)) = prototype
      createLiftedMethod[Target, Environment, R](methodSelection.symbol.asMethod)
    }

    def liftMethodImplFromFunctionReturningWrappedEtaExpansion[Target: WeakTypeTag, Environment: WeakTypeTag, R[_]](prototype: Tree): Tree = {
      val Function(List(_), Apply(_, List(Block(List(), Function(_, Apply(methodSelection, _)))))) = prototype
      createLiftedMethod[Target, Environment, R](methodSelection.symbol.asMethod)
    }
  }

  class WhiteboxMacroImplementations(override val c: whitebox.Context) extends MacroImplementations(c) {
    import c.universe._

    def deriveByName[Target: WeakTypeTag, Environment: WeakTypeTag, R[_]](methodName: Expr[String]): Tree = {
      val Literal(Constant(name: String)) = methodName.tree
      createLiftedMethod[Target, Environment, R](weakTypeOf[Target].member(TermName(name)).asMethod)
    }
  }
}
