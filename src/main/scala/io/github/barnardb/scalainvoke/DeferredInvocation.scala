package io.github.barnardb.scalainvoke

final class DeferredInvocation extends InvocationStrategy {

  override type Invocation[A] = () => A

  @inline
  def wrapInvocation[A](invocation: => A): () => A = () => invocation

}
