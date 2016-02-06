package io.github.barnardb.scalainvoke

import scala.language.higherKinds

/**
 * A strategy for invoking code.
 *
 * Implementers of this class must implement a method for wrapping expressions .
 * The method must have a signature such that invoking code could appear to be calling methods declared as:
 * {{{
 *   def wrapInvocation[A](invocation: A): R[A]
 * }}}
 * This method isn't explicitly declared on this class so that implementers are free to
 * add implicit parameters, use macros, etc.
 *
 * @tparam R type constructor for the wrapped return type
 */
trait InvocationStrategy {
  type Wrapped[A]
}
