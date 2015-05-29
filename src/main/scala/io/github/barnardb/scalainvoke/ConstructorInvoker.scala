package io.github.barnardb.scalainvoke

import scala.reflect.macros.blackbox

object ConstructorInvoker {
  class MacroImplementations(override val c: blackbox.Context) extends FunctionInvoker.MacroImplementations(c) {
    import c.universe._

    def derive[Environment: WeakTypeTag, A: WeakTypeTag]: Expr[Environment => A] = c.Expr {
      val A = weakTypeOf[A]
      createStrategicFunction[Environment](
        returnType     = A,
        function       = Select(New(TypeTree(A)), termNames.CONSTRUCTOR),
        parameterLists = firstAccessibleConstructorIn(A).paramLists
      )
    }

    def firstAccessibleConstructorIn(tpe: Type): MethodSymbol = {
      val constructors = tpe.members.sorted.filter(_.isConstructor).map(_.asMethod)
      constructors
        .find(m => c.typecheck(q"new $tpe(...${m.paramLists.map(_.map(p => q"null.asInstanceOf[${p.typeSignature}]"))})", silent = true) != EmptyTree)
        .getOrElse(c.abort(c.enclosingPosition, s"None of the ${constructors.length} constructor(s) in $tpe seem to be accessible"))
    }
  }
}
