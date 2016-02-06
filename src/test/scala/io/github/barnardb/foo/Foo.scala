package io.github.barnardb.foo

import io.github.barnardb.scalainvoke._

class Foo {

  implicit val argumentExtractionStrategy = new ImplicitArgumentExtractors[Map[String, String]]
  final val strategy = new FunctionLifter[DirectInvocation](new DirectInvocation)

  implicit object StringExtractor extends Extractor[Map[String, String], String] {
    override def extract(a: Map[String, String], name: String): String = a(name)
  }

  private[this] final val haveFun = strategy.liftConstructor[Bar]

  def haveMoreFun(m: Map[String, String]) = haveFun(m)

  def haveOhSoMuchFun() = haveMoreFun(Map("material" -> "gold"))
}
