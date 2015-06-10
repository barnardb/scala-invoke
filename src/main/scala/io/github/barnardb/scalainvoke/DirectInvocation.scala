package io.github.barnardb.scalainvoke

trait DirectInvocation extends ReturnValueStrategy[Id] {

  def wrapInvocation[T](invocation: T): T = invocation
  def unwrap[A](value: A): A = value
}
