package io.github.barnardb.scalainvoke

import scala.reflect.macros.{blackbox, whitebox}

object MethodAndTargetLifter {

  class MacroImplementations(override val c: blackbox.Context) extends MethodLifter.MacroImplementations(c) {
    import c.universe._

    override protected def liftedParameters[Target: WeakTypeTag, Environment: WeakTypeTag]: Seq[Tree] =
      Seq(q"environment: ${weakTypeOf[Environment]}")

    override protected def liftedInvocationTarget[Target: WeakTypeTag](implicit strategy: Expr[FunctionLifter[_, _]]): Tree =
      extractUnnamed(weakTypeOf[Target])

    override protected def liftedFunctionType[Target: WeakTypeTag, Environment: WeakTypeTag](method: MethodSymbol): c.universe.Type =
      appliedType(symbolOf[_ => _], weakTypeOf[Environment], method.returnType)
  }

  class WhiteboxMacroImplementations(override val c: whitebox.Context) extends MethodLifter.WhiteboxMacroImplementations(c) {
    import c.universe._

    override protected def liftedParameters[Target: WeakTypeTag, Environment: WeakTypeTag]: Seq[Tree] =
      Seq(q"environment: ${weakTypeOf[Environment]}")

    override protected def liftedInvocationTarget[Target: WeakTypeTag](implicit strategy: Expr[FunctionLifter[_, _]]): Tree =
      extractUnnamed(weakTypeOf[Target])

    override protected def liftedFunctionType[Target: WeakTypeTag, Environment: WeakTypeTag](method: MethodSymbol): c.universe.Type =
      appliedType(symbolOf[_ => _], weakTypeOf[Environment], method.returnType)
  }
}
