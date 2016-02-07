package io.github.barnardb.scalainvoke

import scala.language.higherKinds

/**
 * A strategy for invoking invocables.
 *
 * Implementers of this class must implement a method for wrapping expressions .
 * The method must have a signature such that invoking code could appear to be calling methods declared as:
 * {{{
 *   def wrapInvocation[A](invocation: A): Invocation[A]
 * }}}
 * This method isn't explicitly declared on this class so that implementers are free to
 * add implicit parameters, use macros, etc.
 */
trait InvocationStrategy {
  /**
    * Type constructor for the value returned by the parameter extraction step.
    *
    * @tparam A the return type of the invocable that is being lifted
    */
  type Invocation[A]
}
