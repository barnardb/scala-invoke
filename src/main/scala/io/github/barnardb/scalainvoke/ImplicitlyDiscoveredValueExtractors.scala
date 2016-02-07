package io.github.barnardb.scalainvoke

/**
  * Extracts named non-implicit values
  *
  * @tparam A environment type to extract from
  * @tparam B type of value to extract
  */
trait ExplicitValueExtractor[A, B] {
  def extract(a: A, name: String): B
}

/**
  * Extracts named implicit values
  *
  * @tparam A environment type to extract from
  * @tparam B type of value to extract
  */
trait ImplicitValueExtractor[A, B] {
  def extract(a: A, name: String): B
}

/**
  * Extracts nameless values
  *
  * @tparam A environment type to extract from
  * @tparam B type of value to extract
  */
trait TypedValueExtractor[A, B] {
  def extract(a: A): B
}

/**
  * Extracts named values
  *
  * @tparam A environment type to extract from
  * @tparam B type of value to extract
  */
trait NamedExtractor[A, B] extends ExplicitValueExtractor[A, B] with ImplicitValueExtractor[A, B]

/**
  * Extracts named and unnamed values
  *
  * @tparam A environment type to extract from
  * @tparam B type of value to extract
  */
trait ValueExtractor[A, B] extends TypedValueExtractor[A, B] with NamedExtractor[A, B]

/**
  * An extraction strategy that looks for implicit extractors to extract values from the environment.
  * Looks for [[ExplicitValueExtractor]]s to extract named non-implicit values,
  * [[ImplicitValueExtractor]]s to extract implicit values, and
  * [[TypedValueExtractor]]s to extract values for which no name is known.
  *
  * @tparam E environment type to extract from
  */
class ImplicitlyDiscoveredValueExtractors[E] extends ValueExtractionStrategy {

  override type Environment = E

  def extract[A](environment: E)(implicit extractor: TypedValueExtractor[E, A]): A =
    extractor.extract(environment)

  def extract[A](environment: E, name: String)(implicit extractor: ExplicitValueExtractor[E, A]): A =
    extractor.extract(environment, name)

  def extractImplicit[A](environment: E, name: String)(implicit extractor: ImplicitValueExtractor[E, A]): A =
    extractor.extract(environment, name)

}
