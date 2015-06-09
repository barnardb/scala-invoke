package io.github.barnardb.scalainvoke

import scala.language.higherKinds
import scala.reflect.macros.{blackbox, whitebox}

object MethodInvoker {

  class MacroImplementations(override val c: blackbox.Context) extends InvocationStrategy.MacroImplementations(c) {
    import c.universe._

    protected def createLiftedMethod[Target: WeakTypeTag, Environment: WeakTypeTag, R[_]](method: MethodSymbol): Tree = {
      implicit val strategy: Expr[InvocationStrategy[Environment, R]] = findStrategy[Environment, R]()
      require(method.owner == symbolOf[Target], s"Expected method owner type ${method.owner} == ${symbolOf[Target]}")
      val Target = weakTypeOf[Target]
      val Environment = weakTypeOf[Environment]
      c.typecheck(
        tree = q"""(target: $Target, environment: $Environment) => target.$method(...${method.paramLists.map(_.map(extractParameter[Environment, R]))})""",
        pt = appliedType(symbolOf[(_, _) => _], Target, Environment, method.returnType)
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

    def deriveByName[Target: WeakTypeTag, Environment: WeakTypeTag, Result[_]](methodName: Expr[String]): Tree = {
      val Literal(Constant(name: String)) = methodName.tree
      createLiftedMethod[Target, Environment, Result](weakTypeOf[Target].member(TermName(name)).asMethod)
    }
  }
}
