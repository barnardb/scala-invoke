package io.github.barnardb.scalainvoke

final class DirectInvocation extends InvocationStrategy {

  override type Wrapped[A] = A

  def wrapInvocation[T](invocation: T): T = invocation

}
