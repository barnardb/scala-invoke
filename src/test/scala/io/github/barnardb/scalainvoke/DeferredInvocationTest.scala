package io.github.barnardb.scalainvoke

import org.scalatest.FunSuite

class DeferredInvocationTest extends FunSuite {

  class Var(init: String) {
    var value = init
    def wrapAt(index: Int): String = init.substring(index) + init.substring(0, index)
  }

  implicit val invocationStrategy = new DeferredInvocation

  implicit object VarExtractor extends NamedValueExtractor[Map[String, Var], String] {
    override def extract(a: Map[String, Var], name: String): String = {
      println(s"extracting $name from $a")
      a(name).value
    }
  }

  test("lifted function application extracts parameters immediately but defers execution") {
    implicit val valueExtractionStrategy = new ImplicitlyDiscoveredValueExtractors[Map[String, Var]]

    var stage = "lifting stage"
    val lifted = Lift.function((boxed: String) => s"param extracted during $boxed, evaluated during $stage")

    stage = "primary invocation stage"
    val box = new Var("primary invocation")
    val deferred: () => String = lifted(Map("boxed" -> box))

    stage = "deferred invocation stage"
    box.value = "deferred invocation"
    assertResult("param extracted during primary invocation, evaluated during deferred invocation stage") {
      deferred()
    }
  }

  test("lifted constructor application extracts parameters immediately but defers execution") {
    implicit val valueStrategy = new ImplicitlyDiscoveredValueExtractors[Var]
    implicit object StringExtractor extends NamedValueExtractor[Var, String] {
      override def extract(a: Var, name: String): String = s"$name: ${a.value}"
    }

    val lifted = Lift.constructor[Var]

    val box = new Var("primary invocation")
    val deferred: () => Var = lifted(box)

    box.value = "deferred invocation"
    assertResult("init: primary invocation") {
      deferred().value
    }
  }

  test("eta-expansion-lifted method application extracts parameters immediately but defers execution") {
    implicit val valueExtractionStrategy = new ImplicitlyDiscoveredValueExtractors[Map[String, Var]]
    implicit object IntExtractor extends NamedValueExtractor[Map[String, Var], Int] {
      override def extract(a: Map[String, Var], name: String): Int = a(name).value.toInt
    }

    val lifted = Lift.method[Var](_.wrapAt _)

    val box = new Var("1")
    val deferred: () => String = lifted(new Var("funny"), Map("index" -> box))

    box.value = "2"
    assertResult("unnyf") {
      deferred()
    }
  }

}
