package io.github.barnardb.scalainvoke

final class DeferredInvocation extends ReturnValueStrategy[Function0] {

  def wrapInvocation[T](invocation: => T): () => T = () => invocation

}
