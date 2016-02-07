package io.github.barnardb.scalainvoke.example

object Example extends App {
  import io.github.barnardb.scalainvoke._

  final class Path(override val toString: String) extends AnyVal {
    def group: String = toString.stripPrefix("/").split("/").head
  }
  case class HttpRequest(path: Path, query: Map[String, String])

  implicit val valueExtractionStrategy = new ImplicitlyDiscoveredValueExtractors[HttpRequest]
  implicit val invocationStrategy = new DeferredInvocation

  implicit object PathExtractor extends ValueExtractor[HttpRequest, Path] {
    override def extract(request: HttpRequest): Path = request.path
    override def extract(request: HttpRequest, name: String): Path = request.path
  }
  implicit object StringExtractor extends NamedValueExtractor[HttpRequest, String] {
    override def extract(request: HttpRequest, name: String): String = request.query(name)
  }
  implicit object DoubleExtractor extends NamedValueExtractor[HttpRequest, Double] {
    override def extract(request: HttpRequest, name: String): Double = request.query(name).toDouble
  }

  def action(path: Path, material: String, weight: Double): (String, Double) =
    "Do something with " + path.group + " and " + material -> weight * 2

  // This should happen once, somewhere in the setup of the program
  private[this] final val liftedAction = Lift.function(action _)

  // Because we are using DeferredExecution, we have a 2-step process to invoke.

  // Passing the environment to the lifted action causes the parameters to be extracted and an invoker to be returned.
  val invoker: () => (String, Double) =
    liftedAction(HttpRequest(new Path("/metal/bars"), Map("material" -> "gold", "weight" -> "42.1337")))

  // At this point we know we have successfully extracted the arguments, but haven't yet invoked the function.

  // We can invoke the function when we're ready.
  val (message: String, doubledWeight: Double) = invoker()

  println(message)
  println(doubledWeight)
}
