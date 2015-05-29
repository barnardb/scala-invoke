package io.github.barnardb.scalainvoke

import org.scalatest.FunSuite

class InvocationStrategyTest extends FunSuite {

  class DemoClass(id: String) {
    def foo(first: String, second: Int): String = s"[$id] First: $first, second: $second"
  }

  object TestExtractors {
    implicit object StringExtractor extends Extractor[Map[String, String], String] {
      override def extract(a: Map[String, String], name: String): String = a(name)
    }
    implicit object IntExtractor extends Extractor[Map[String, String], Int] {
      override def extract(a: Map[String, String], name: String): Int = a(name).toInt
    }
  }

  val strategy = new ImplicitExtractorInvocationStrategy[Map[String, String]]


  test("invokes inline function literals") {
    import TestExtractors._
    val invoker = strategy.functionInvoker((first: String, second: Int) => s"First: $first, second: $second")

    val context = Map("first" -> "a", "second" -> "42")

    assertResult("First: a, second: 42") {
      invoker(context): String
    }
  }

//  test("invokes function values defined elsewhere") {
//    val foo = (first: String, second: Int) => s"First: $first, second: $second"
//
//    import Extractors._
//    val invoker = strategy.functionInvoker(foo)
//
//    assertResult("First: a, second: 42") {
//      invoker(Map("first" -> "a", "second" -> "42")): String
//    }
//  }

  test("invokes eta-expanded local methods") {
    def localMethod(first: String, second: Int): String = s"First: $first, second: $second"

    import TestExtractors._
    val invoker = strategy.functionInvoker(localMethod _)

    assertResult("First: a, second: 42") {
      invoker(Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes eta-expanded methods on class instances") {
    import TestExtractors._
    val instance = new DemoClass("hi")
    val invoker = strategy.functionInvoker(instance.foo _)

    assertResult("[hi] First: a, second: 42") {
      invoker(Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes eta-expanded methods on Scala objects") {
    object bar {
      def foo(first: String, second: Int): String = s"First: $first, second: $second"
    }

    import TestExtractors._
    val invoker = strategy.functionInvoker(bar.foo _)

    assertResult("First: a, second: 42") {
      invoker(Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes eta-expanded methods on inline class instances") {
    import TestExtractors._
    val invoker = strategy.functionInvoker(new DemoClass("hi").foo _)

    assertResult("[hi] First: a, second: 42") {
      invoker(Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes eta-expanded methods on class instances returned from method calls") {
    import TestExtractors._
    def instance = new DemoClass("hi")
    val invoker = strategy.functionInvoker(instance.foo _)

    assertResult("[hi] First: a, second: 42") {
      invoker(Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes function literals with type ascriptions") {
    import TestExtractors._
    val invoker = strategy.functionInvoker(((x: String) => x): String => String)

    assertResult("X") {
      invoker(Map("x" -> "X")): String
    }
  }

  test("invokes function literals defined in a block that reference a lazy val in the block (i.e., prevents owner chain corruption without taking untypecheck shortcuts)") {
    import TestExtractors._
    val invoker = strategy.functionInvoker({
      lazy val tricksy: String = "I'm forcing you to deal with owner chain corruption without resorting to untypecheck"
      (x: String) => x + tricksy
    })

    assertResult("look, I'm forcing you to deal with owner chain corruption without resorting to untypecheck") {
      invoker(Map("x" -> "look, ")): String
    }
  }

  test("constructs objects") {
    import TestExtractors._
    val invoker = strategy.constructorInvoker[DemoClass]

    assertResult("[yays] First: a, second: 1") {
      invoker(Map("id" -> "yays")).foo("a", 1)
    }
  }

  test("constructs objects from primary constructor in the presence of multiple constructors") {
    class MultiConstruct(val a: String) {
      def this(b: String, c: String) = this(s"$b $c")
    }

    import TestExtractors._
    val invoker = strategy.constructorInvoker[MultiConstruct]

    assertResult("A") {
      invoker(Map("a" -> "A", "b" -> "B", "c" -> "C")).a
    }
  }

  test("constructs objects from primary constructor in the presence of private primary constructor") {
    class MultiConstruct private (val a: String) {
      def this(b: String, c: String) = this(s"$b $c")
    }

    import TestExtractors._
    val invoker = strategy.constructorInvoker[MultiConstruct]

    assertResult("B C") {
      invoker(Map("a" -> "A", "b" -> "B", "c" -> "C")).a
    }
  }

  test("invokes methods denoted by partial application") {
    import TestExtractors._
    val invoker = strategy.method[DemoClass](_.foo _)

    assertResult("[ha!] First: a, second: 42") {
      invoker(new DemoClass("ha!"), Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes methods denoted by prototype lambda") {
    import TestExtractors._
    val invoker = strategy.method[DemoClass](_.foo(_, _))

    assertResult("[ha!] First: a, second: 42") {
      invoker(new DemoClass("ha!"), Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes methods denoted by name") {
    import TestExtractors._
    val invoker = strategy.method[DemoClass]("foo")

    assertResult("[ha!] First: a, second: 42") {
      invoker(new DemoClass("ha!"), Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("can disambiguate overloaded methods using type ascriptions in prototype lambda's") {
    class MultiFoo {
      def foo(value: String): String = value + 2
      def foo(value: Int): String = (value + 2).toString
    }

    import TestExtractors._
    val invoker = strategy.method[MultiFoo](_.foo(_: Int))

    assertResult("3") {
      invoker(new MultiFoo, Map("value" -> "1")): String
    }
  }

  test("invokes methods with multiple argument lists") {
    class MultiArg(id: String) {
      def foo(first: String)(second: Int): String = s"[$id] First: $first, second: $second"
    }

    import TestExtractors._
    val invoker = strategy.method[MultiArg](_.foo(_)(_))

    assertResult("[ha!] First: a, second: 42") {
      invoker(new MultiArg("ha!"), Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes methods with implicit argument lists") {
    class DifferentPlicities(id: String) {
      def foo(first: String)(implicit second: String): String = s"[$id] First: $first, second: $second"
    }

    implicit object ExplicitStringExtractor extends ExplicitExtractor[Map[String, String], String] {
      override def extract(a: Map[String, String], name: String): String = "explicit " + a(name)
    }
    implicit object ImplicitStringExtractor extends ImplicitExtractor[Map[String, String], String] {
      override def extract(a: Map[String, String], name: String): String = "implicit " + a(name)
    }

    val invoker = strategy.method[DifferentPlicities](_.foo(_)(_))

    assertResult("[ha!] First: explicit a, second: implicit b") {
      implicit val implicitString: String = "c"
      s"$implicitString should be ignored by the invoker, so we need to use it here to make the compiler happy"
      invoker(new DifferentPlicities("ha!"), Map("first" -> "a", "second" -> "b")): String
    }
  }

}
