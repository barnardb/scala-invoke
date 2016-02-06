package io.github.barnardb.scalainvoke

final class DeferredInvocation extends InvocationStrategy {

  override type Wrapped[A] = () => A

  def wrapInvocation[A](invocation: => A): () => A = () => invocation

}
