package io.github.barnardb.scalainvoke

object FunctionReturning {
  import scala.language.implicitConversions
  private type * = Nothing

  implicit def apply[R](function: () => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
  implicit def apply[R](function: (*, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *, *) => R): FunctionReturning[R] = new FunctionReturning[R](function)
}

/**
 * A wrapper class for FunctionN values that return values of type [[R]].
 *
 * This allows types to be expressed that accept functions of any arity while constraining their return types,
 * which allows us to work around some typing quirks that I couldn't get around with overloaded methods.
 */
final class FunctionReturning[R] private (val function: AnyRef) extends AnyVal
