package io.github.barnardb.scalainvoke

trait DeferredInvocation extends InvocationStrategy[Function0] {

  def wrapInvocation[T](invocation: => T): () => T = () => invocation

}
