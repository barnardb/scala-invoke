package io.github.barnardb.scalainvoke

import scala.reflect.macros.blackbox

import io.github.barnardb.scalainvoke.macroutil.OwnerChainCorrector

object FunctionInvoker {

  class MacroImplementations(override val c: blackbox.Context) extends InvocationStrategy.MacroImplementations(c) {
    import c.universe._

    protected def unwrapFunction(tree: Tree): Option[Function] = tree match {
      case f: Function    => Some(f)
      case Block(_, expr) => unwrapFunction(expr)
      case Typed(expr, _) => unwrapFunction(expr)
      case Ident(_)       => None
    }

    def deriveInvoker[Environment: WeakTypeTag](function: Tree): Tree = {
      unwrapFunction(function) match {
        case Some(Function(params, body)) =>
          createStrategicFunction[Environment](function.tpe.typeArgs.last, OwnerChainCorrector.splice(c)(function), List(params.map(_.symbol)))
        case None =>
          c.abort(function.pos, s"Looks like you're trying to create a function invoker for a function value that was defined somewhere else. Unfortunately, this isn't currently supported.")
//          val applyMethodType = function.tpe.member(TermName("apply")).typeSignatureIn(function.tpe).asInstanceOf[MethodType]
//          c.info(function.pos, s"Cannot find function definition, falling back to names from the apply method definition. Param lists: ${applyMethodType.paramLists}, and body type: ${applyMethodType.resultType}", force = true)
//          instantiateFunctionInvoker[Environment](applyMethodType.resultType, OwnerChainCorrector.splice(c)(function), applyMethodType.paramLists)
      }
    }

    def createStrategicFunction[Environment: WeakTypeTag](returnType: Type, function: Tree, parameterLists: List[List[Symbol]]): Tree = {
      implicit val strategy = findStrategy[Environment]
      val Environment = weakTypeOf[Environment]
      q"""
        new ${appliedType(symbolOf[_ => _], Environment, returnType)} {
          override def apply(environment: $Environment): $returnType =
            $function(...${parameterLists.map(_.map(extractParameter[Environment]))})
        }
      """
    }
  }

}
