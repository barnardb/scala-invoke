package io.github.barnardb.scalainvoke

final class DirectInvocation extends InvocationStrategy {

  override type Invocation[A] = A

  @inline
  def wrapInvocation[T](invocation: T): T = invocation

}
