package io.github.barnardb.scalainvoke

import org.scalatest.FunSuite

class DeferredInvocationStrategyTest extends FunSuite {

  class Box(init: String) {
    var value = init
  }
  val strategy = new InvocationStrategy[Map[String, Box], Function0] with ImplicitExtractors[Map[String, Box]] with DeferredInvocation

  implicit object BoxExtractor extends Extractor[Map[String, Box], String] {
    override def extract(a: Map[String, Box], name: String): String = {
      println(s"extracting $name from $a")
      a(name).value
    }
  }

  test("lifted method application extracts parameters immediate but defers execution") {
    var stage = "init"

    val lifted = strategy.lift((boxed: String) => s"param: $boxed, exec: $stage")

    stage = "eager"
    val box = new Box("eager")

    val deferred: () => String = lifted(Map("boxed" -> box))

    stage = "deferred"
    box.value = "deferred"

    assertResult("param: eager, exec: deferred") {
      deferred()
    }
  }

}
