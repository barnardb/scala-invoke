package io.github.barnardb.scalainvoke

import scala.reflect.macros.blackbox

object InvocationStrategy {
  class MacroImplementations(val c: blackbox.Context) {
    import c.universe._

    def findStrategy[Environment]: Expr[InvocationStrategy[Environment]] =
      if (c.prefix.actualType <:< typeOf[InvocationStrategy[_]]) c.Expr(c.prefix.tree)
      else c.prefix.tree.symbol.typeSignature.member(TermName("strategy")) match {
        case NoSymbol => c.abort(c.enclosingPosition, s"Can't find the InvocationStrategy to use (prefix symbol ${c.prefix.tree.symbol}, sig: ${c.prefix.tree.symbol.typeSignature}})")
        case strategy => c.Expr(q"${c.prefix}.$strategy")
      }

    def extractParameter[Environment](parameter: Symbol)(implicit strategy: Expr[InvocationStrategy[Environment]]): Tree = {
      val extractionMethod =
        if (parameter.isImplicit) TermName("extractImplicit")
        else TermName("extract")
      q"$strategy.$extractionMethod[${parameter.typeSignature}](environment, ${parameter.name.toString})"
    }
  }
}

/**
 * A strategy for invoking functions given an object of type [[Environment]].
 *
 * Implementers of this class must implement methods for extracting argument values from an [[Environment]].
 * The methods accept an environment and a parameter name,
 * and must have signatures such that invoking code could appear to be calling methods declared as:
 * {{{
 *   def extract[A](environment: Environment, name: String): A
 *   def extractImplicit[A](environment: Environment, name: String): A
 * }}}
 * These methods aren't explicitly declared on this class so that implementers are free to
 * add implicit parameters, use macros, etc.
 */
abstract class InvocationStrategy[Environment] { is =>

  /* Conceptual methods (see class documentation above)
  def extract[A](environment: Environment, name: String): A
  def extractImplicit[A](environment: Environment, name: String): A
  */

  /**
   * These methods build function invokers for the given function
   * @tparam Result the function's result type
   */
  def functionInvoker[Result](function: (_) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]
  def functionInvoker[Result](function: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => Result): Environment => Result = macro FunctionInvoker.MacroImplementations.deriveInvoker[Environment]

  /**
   * Builds a function invoker than calls the first accessible constructor of type [[A]]
   */
  def constructorInvoker[A]: Environment => A =
    macro ConstructorInvoker.MacroImplementations.derive[Environment, A]

  def method[Target]: MethodInvocationStrategy[Target] = new MethodInvocationStrategy[Target](this)
  final class MethodInvocationStrategy[Target](val strategy: is.type) {
    /**
     * Builds method invokers from prototypes of the form {{{strategy.method[A]("methodOnA")}}}
     */
    def apply(methodName: String): (Target, Environment) => _ =
      macro MethodInvoker.WhiteboxMacroImplementations.deriveByName[Target, Environment]

    /**
     * Builds method invokers from prototypes of the form {{{strategy.method[A](_.methodOnA(_, _))}}}
     * @tparam Result the method's result type
     */
    def apply[Result](prototype: Target => FunctionReturning[Result]): (Target, Environment) => Result =
      macro MethodInvoker.MacroImplementations.deriveFromEtaExpansionWithFunctionReturning[Target, Environment]

    /**
     * Syntactic shortcut for Nothing in invariant positions,
     * used to mean "we don't care about this type and will accept anything" when _ doesn't cut it.
     */
    private type * = Nothing

    /**
     * These methods build method invokers from eta-expansion prototypes of the form {{{strategy.method[A](_.methodOnA _)}}}
     * @tparam Result the method's result type
     */
    def apply[Result](prototype: (Target, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
    def apply[Result](prototype: (Target, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => Result): (Target, Environment) => Result = macro MethodInvoker.MacroImplementations.deriveFromPrototype[Target, Environment]
  }
}
