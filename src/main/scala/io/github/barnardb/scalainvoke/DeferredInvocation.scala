package io.github.barnardb.scalainvoke

trait DeferredInvocation extends ReturnValueStrategy[Function0] {

  def wrapInvocation[T](invocation: => T): () => T = () => invocation

}
