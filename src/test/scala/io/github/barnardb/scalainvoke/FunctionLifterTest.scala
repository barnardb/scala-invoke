package io.github.barnardb.scalainvoke

import org.scalatest.FunSuite

class FunctionLifterTest extends FunSuite {

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
    implicit object DemoClassExtractor extends UnnamedExtractor[Map[String, String], DemoClass] {
      override def extract(a: Map[String, String]): DemoClass = new DemoClass(a("demo class name"))
    }
  }

  val strategy = new FunctionLifter[ImplicitArgumentExtractors[Map[String, String]], DirectInvocation](new ImplicitArgumentExtractors[Map[String, String]], new DirectInvocation)

  test("lifts inline function literals") {
    import TestExtractors._
    val invoker = strategy.lift((first: String, second: Int) => s"First: $first, second: $second")

    val context = Map("first" -> "a", "second" -> "42")

    assertResult("First: a, second: 42") {
      invoker(context): String
    }
  }

  test("invokes function values defined elsewhere, resorting to extracting arguments without parameter names") {
    val foo = (first: String, second: Int) => s"First: $first, second: $second"

    implicit object UnnamedStringExtractor extends UnnamedExtractor[Map[String, String], String] {
      override def extract(a: Map[String, String]): String = a("defaultString")
    }
    implicit object UnnamedIntExtractor extends UnnamedExtractor[Map[String, String], Int] {
      override def extract(a: Map[String, String]): Int = a("defaultInt").toInt
    }

    val invoker = strategy.lift(foo)

    assertResult("First: unfortunate, second: 7") {
      invoker(Map("first" -> "a", "second" -> "42", "defaultString" -> "unfortunate", "defaultInt" -> "7")): String
    }
  }

  test("lifts eta-expanded local methods") {
    def localMethod(first: String, second: Int): String = s"First: $first, second: $second"

    import TestExtractors._
    val invoker = strategy.lift(localMethod _)

    assertResult("First: a, second: 42") {
      invoker(Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("lifts eta-expanded methods on class instances") {
    import TestExtractors._
    val instance = new DemoClass("hi")
    val invoker = strategy.lift(instance.foo _)

    assertResult("[hi] First: a, second: 42") {
      invoker(Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("lifts eta-expanded methods on Scala objects") {
    object bar {
      def foo(first: String, second: Int): String = s"First: $first, second: $second"
    }

    import TestExtractors._
    val invoker = strategy.lift(bar.foo _)

    assertResult("First: a, second: 42") {
      invoker(Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("lifts eta-expanded methods on inline class instances") {
    import TestExtractors._
    val invoker = strategy.lift(new DemoClass("hi").foo _)

    assertResult("[hi] First: a, second: 42") {
      invoker(Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("lifts eta-expanded methods on class instances returned from method calls") {
    import TestExtractors._
    def instance = new DemoClass("hi")
    val invoker = strategy.lift(instance.foo _)

    assertResult("[hi] First: a, second: 42") {
      invoker(Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("lifts function literals with type ascriptions") {
    import TestExtractors._
    val invoker = strategy.lift(((x: String) => x): String => String)

    assertResult("X") {
      invoker(Map("x" -> "X")): String
    }
  }

  test("lifts function literals defined in a block that reference a lazy val in the block (i.e., prevents owner chain corruption without taking untypecheck shortcuts)") {
    import TestExtractors._
    val invoker = strategy.lift({
      lazy val tricksy: String = "I'm forcing you to deal with owner chain corruption without resorting to untypecheck"
      (x: String) => x + tricksy
    })

    assertResult("look, I'm forcing you to deal with owner chain corruption without resorting to untypecheck") {
      invoker(Map("x" -> "look, ")): String
    }
  }

  test("lifts class constructors") {
    import TestExtractors._
    val invoker = strategy.liftConstructor[DemoClass]

    assertResult("[yays] First: a, second: 1") {
      invoker(Map("id" -> "yays")).foo("a", 1)
    }
  }

  test("selects the first constructor for lifting in the presence of multiple constructors") {
    class MultiConstruct(val a: String) {
      def this(b: String, c: String) = this(s"$b $c")
    }

    import TestExtractors._
    val invoker = strategy.liftConstructor[MultiConstruct]

    assertResult("A") {
      invoker(Map("a" -> "A", "b" -> "B", "c" -> "C")).a
    }
  }

  test("skips inaccessible constructors when selecting which constructor to lift") {
    class MultiConstruct private (val a: String) {
      def this(b: String, c: String) = this(s"$b $c")
    }

    import TestExtractors._
    val invoker = strategy.liftConstructor[MultiConstruct]

    assertResult("B C") {
      invoker(Map("a" -> "A", "b" -> "B", "c" -> "C")).a
    }
  }

  test("invokes methods denoted by partial application") {
    import TestExtractors._
    val invoker = strategy.liftMethod[DemoClass](_.foo _)

    assertResult("[ha!] First: a, second: 42") {
      invoker(new DemoClass("ha!"), Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes methods denoted by partial application, with target extraction") {
    import TestExtractors._
    val invoker = strategy.liftMethodAndTarget[DemoClass](_.foo _)

    assertResult("[Linley] First: a, second: 42") {
      invoker(Map("demo class name" -> "Linley", "first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes methods denoted by prototype lambda") {
    import TestExtractors._
    val invoker = strategy.liftMethod[DemoClass](_.foo(_, _))

    assertResult("[ha!] First: a, second: 42") {
      invoker(new DemoClass("ha!"), Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes methods denoted by prototype lambda, with target extraction") {
    import TestExtractors._
    val invoker = strategy.liftMethodAndTarget[DemoClass](_.foo(_, _))

    assertResult("[Linley] First: a, second: 42") {
      invoker(Map("demo class name" -> "Linley", "first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes methods denoted by name") {
    import TestExtractors._
    val invoker = strategy.liftMethod[DemoClass]("foo")

    assertResult("[ha!] First: a, second: 42") {
      invoker(new DemoClass("ha!"), Map("first" -> "a", "second" -> "42")): String
    }
  }

  test("invokes methods denoted by name, with target extraction") {
    import TestExtractors._
    val invoker = strategy.liftMethodAndTarget[DemoClass]("foo")

    assertResult("[Linley] First: a, second: 42") {
      invoker(Map("demo class name" -> "Linley", "first" -> "a", "second" -> "42")): String
    }
  }

  test("can disambiguate overloaded methods using type ascriptions in prototype lambda's") {
    class MultiFoo {
      def foo(value: String): String = value + 2
      def foo(value: Int): String = (value + 2).toString
    }

    import TestExtractors._
    val invoker = strategy.liftMethod[MultiFoo](_.foo(_: Int))

    assertResult("3") {
      invoker(new MultiFoo, Map("value" -> "1")): String
    }
  }

  test("invokes methods with multiple argument lists") {
    class MultiArg(id: String) {
      def foo(first: String)(second: Int): String = s"[$id] First: $first, second: $second"
    }

    import TestExtractors._
    val invoker = strategy.liftMethod[MultiArg](_.foo(_)(_))

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

    val invoker = strategy.liftMethod[DifferentPlicities](_.foo(_)(_))

    assertResult("[ha!] First: explicit a, second: implicit b") {
      implicit val implicitString: String = "c"
      s"$implicitString should be ignored by the invoker, so we need to use it here to avoid a compiler warning"
      invoker(new DifferentPlicities("ha!"), Map("first" -> "a", "second" -> "b")): String
    }
  }

}
