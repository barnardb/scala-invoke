package io.github.barnardb.scalainvoke.example

object `your application's domain` {

  case class HttpRequest(path: Path, query: Map[String, String])

  final class Path(override val toString: String) extends AnyVal {
    def group: String = toString.stripPrefix("/").split("/").head
  }

  def action(path: Path, material: String, weight: Double): (String, Double) =
    "Do something with " + path.group + " and " + material -> weight * 2
}

object `extractors for use with scalainvoke` {
  import `your application's domain`._
  import io.github.barnardb.scalainvoke.{NamedValueExtractor, ValueExtractor}

  implicit object StringExtractor extends NamedValueExtractor[HttpRequest, String] {
    override def extract(request: HttpRequest, name: String): String = request.query(name)
  }

  implicit object DoubleExtractor extends NamedValueExtractor[HttpRequest, Double] {
    override def extract(request: HttpRequest, name: String): Double = request.query(name).toDouble
  }

  implicit object PathExtractor extends ValueExtractor[HttpRequest, Path] {
    override def extract(request: HttpRequest): Path = request.path
    override def extract(request: HttpRequest, name: String): Path = request.path
  }
}

object `lifting` {
  import `your application's domain`._
  import `extractors for use with scalainvoke`._
  import io.github.barnardb.scalainvoke._

  // Choose which strategies you want to use
  implicit val valueExtractionStrategy = new ImplicitlyDiscoveredValueExtractors[HttpRequest]
  implicit val invocationStrategy = new DeferredInvocation

  // Lift things
  val liftedAction = Lift.function(action _)
}

object `calling the lifted function` extends App {
  import `your application's domain`._
  import `lifting`.liftedAction

  // Because we are using DeferredExecution, we have a 2-step process to invoke.

  // Passing an environment to the lifted action causes the parameters to be extracted and an invoker to be returned.
  val environment = HttpRequest(new Path("/metal/bars"), Map("material" -> "gold", "weight" -> "42.1337"))
  val invoker: () => (String, Double) = liftedAction(environment)
  // At this point we know we have successfully extracted the arguments, but haven't yet invoked the function.

  // We can invoke the function when we're ready.
  val (message: String, doubledWeight: Double) = invoker()

  println(message)
  println(doubledWeight)
}
