package io.github.barnardb.scalainvoke

import scala.reflect.macros.whitebox

object MethodAndTargetLifter {

  class WhiteboxMacroImplementations(override val c: whitebox.Context) extends MethodLifter.WhiteboxMacroImplementations(c) {
    import c.universe._

    override def createMethodInvoker(Target: Type, Environment: Type, aes: Expr[ArgumentExtractionStrategy], invokeOnTarget: Tree => Tree) =
      q"""(environment: $Environment) => ${invokeOnTarget(q"$aes.extract[$Target](environment)")}"""

    override def liftedFunctionType[Target: WeakTypeTag](Environment: Type, method: MethodSymbol): c.universe.Type =
      appliedType(symbolOf[_ => _], Environment, method.returnType)

  }

}
