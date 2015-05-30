package io.github.barnardb.scalainvoke

import scala.reflect.macros.{blackbox, whitebox}

object MethodInvoker {

  class MacroImplementations(override val c: blackbox.Context) extends InvocationStrategy.MacroImplementations(c) {
    import c.universe._

    protected def instantiateInvoker[Target: WeakTypeTag, Environment: WeakTypeTag](method: MethodSymbol)(implicit strategy: Expr[InvocationStrategy[Environment]] = findStrategy[Environment]): Tree = {
      require(method.owner == symbolOf[Target], s"Expected method owner type ${method.owner} == ${symbolOf[Target]}")
      val Target = weakTypeOf[Target]
      val Environment = weakTypeOf[Environment]
      q"""
        new ${appliedType(symbolOf[(_, _) => _], Target, Environment, method.returnType)} {
          override def apply(target: $Target, environment: $Environment): ${method.returnType} =
            target.$method(...${method.paramLists.map(_.map(extractParameter[Environment]))})
        }
      """
    }

    def deriveFromPrototype[Target: WeakTypeTag, Environment: WeakTypeTag](prototype: Tree): Tree = {
      val Function (_, Apply(methodSelection, _)) = prototype
      instantiateInvoker[Target, Environment](methodSelection.symbol.asMethod)
    }

    def liftMethodImplFromFunctionReturningWrappedEtaExpansion[Target: WeakTypeTag, Environment: WeakTypeTag](prototype: Tree): Tree = {
      val Function(List(_), Apply(_, List(Block(List(), Function(_, Apply(methodSelection, _)))))) = prototype
      instantiateInvoker[Target, Environment](methodSelection.symbol.asMethod)
    }
  }

  class WhiteboxMacroImplementations(override val c: whitebox.Context) extends MacroImplementations(c) {
    import c.universe._

    def deriveByName[Target: WeakTypeTag, Environment: WeakTypeTag](methodName: Expr[String]): Tree = {
      val Literal(Constant(name: String)) = methodName.tree
      instantiateInvoker[Target, Environment](weakTypeOf[Target].member(TermName(name)).asMethod)
    }
  }
}
