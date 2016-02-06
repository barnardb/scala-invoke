package io.github.barnardb.foo

import io.github.barnardb.scalainvoke._

class Foo {

  final val strategy = new FunctionLifter[ImplicitArgumentExtractors[Map[String, String]], DirectInvocation](new ImplicitArgumentExtractors[Map[String, String]], new DirectInvocation)

  implicit object StringExtractor extends Extractor[Map[String, String], String] {
    override def extract(a: Map[String, String], name: String): String = a(name)
  }

  private[this] final val haveFun = strategy.liftConstructor[Bar]

  def haveMoreFun(m: Map[String, String]) = haveFun(m)

  def haveOhSoMuchFun() = haveMoreFun(Map("material" -> "gold"))
}
