package io.github.barnardb.scalainvoke

import org.scalatest.FunSuite

class DeferredInvocationTest extends FunSuite {

  class Var(init: String) {
    var value = init
  }

  val strategy = new FunctionLifter[Map[String, Var], DeferredInvocation](new DeferredInvocation)
    with ImplicitArgumentExtractors[Map[String, Var]]

  implicit object VarExtractor extends Extractor[Map[String, Var], String] {
    override def extract(a: Map[String, Var], name: String): String = {
      println(s"extracting $name from $a")
      a(name).value
    }
  }

  test("lifted method application extracts parameters immediately but defers execution") {
    var stage = "lifting stage"
    val lifted = strategy.lift((boxed: String) => s"param extracted during $boxed, evaluated during $stage")

    stage = "primary invocation stage"
    val box = new Var("primary invocation")
    val deferred: () => String = lifted(Map("boxed" -> box))

    stage = "deferred invocation stage"
    box.value = "deferred invocation"
    assertResult("param extracted during primary invocation, evaluated during deferred invocation stage") {
      deferred()
    }
  }

}
