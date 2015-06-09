package io.github.barnardb.scalainvoke

final class DeferredInvocationStrategy[E] extends ImplicitExtractorInvocationStrategy[E, Function0] {

  def wrapInvocation[T](invocation: => T): () => T = () => invocation

}
