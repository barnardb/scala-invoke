package io.github.barnardb.foo

import io.github.barnardb.scalainvoke._

class Foo {

  final val strategy = new FunctionLifter[Map[String, String], DirectInvocation](new DirectInvocation)
    with ImplicitArgumentExtractors[Map[String, String]]

  implicit object StringExtractor extends Extractor[Map[String, String], String] {
    override def extract(a: Map[String, String], name: String): String = a(name)
  }

  private[this] final val haveFun = strategy.liftConstructor[Bar]

  def haveMoreFun(m: Map[String, String]) = haveFun(m)

  def haveOhSoMuchFun() = haveMoreFun(Map("material" -> "gold"))
}
