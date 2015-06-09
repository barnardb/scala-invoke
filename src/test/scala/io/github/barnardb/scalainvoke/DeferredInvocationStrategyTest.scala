package io.github.barnardb.scalainvoke

import org.scalatest.FunSuite

class DeferredInvocationStrategyTest extends FunSuite {

  class Box(init: String) {
    var value = init
  }
  val strategy: DeferredInvocationStrategy[Map[String, Box]] = new DeferredInvocationStrategy[Map[String, Box]]

  implicit object BoxExtractor extends Extractor[Map[String, Box], String] {
    override def extract(a: Map[String, Box], name: String): String = {
      println(s"extracting $name from $a")
      a(name).value
    }
  }

  test("extracts parameters and returns deferred execution") {
    var stage = "init"

    val lifted = strategy.lift((boxed: String) => s"param: $boxed, exec: $stage")

    stage = "eager"
    val box = new Box("eager")

    val deferred = lifted(Map("boxed" -> box))

    stage = "deferred"
    box.value = "deferred"

    assertResult("param: eager, exec: deferred") {
      deferred()
    }
  }

}
