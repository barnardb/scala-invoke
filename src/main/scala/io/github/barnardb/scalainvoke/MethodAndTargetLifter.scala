package io.github.barnardb.scalainvoke

import scala.reflect.macros.{blackbox, whitebox}

object MethodAndTargetLifter {

  class MacroImplementations(override val c: blackbox.Context) extends MethodLifter.MacroImplementations(c) {
    import c.universe._

    override protected def liftedParameters[Target: WeakTypeTag](Environment: Type): Seq[Tree] =
      Seq(q"environment: $Environment")

    override protected def liftedInvocationTarget[Target: WeakTypeTag](aes: Expr[ArgumentExtractionStrategy]): Tree =
      extractUnnamed(aes, weakTypeOf[Target])

    override protected def liftedFunctionType[Target: WeakTypeTag](Environment: Type, method: MethodSymbol): c.universe.Type =
      appliedType(symbolOf[_ => _], Environment, method.returnType)
  }

  class WhiteboxMacroImplementations(override val c: whitebox.Context) extends MethodLifter.WhiteboxMacroImplementations(c) {
    import c.universe._

    override protected def liftedParameters[Target: WeakTypeTag](Environment: Type): Seq[Tree] =
      Seq(q"environment: $Environment")

    override protected def liftedInvocationTarget[Target: WeakTypeTag](aes: Expr[ArgumentExtractionStrategy]): Tree =
      extractUnnamed(aes, weakTypeOf[Target])

    override protected def liftedFunctionType[Target: WeakTypeTag](Environment: Type, method: MethodSymbol): c.universe.Type =
      appliedType(symbolOf[_ => _], Environment, method.returnType)
  }
}
