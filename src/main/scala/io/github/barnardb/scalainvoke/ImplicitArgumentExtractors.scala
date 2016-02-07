package io.github.barnardb.scalainvoke

/**
 * Extracts named values for non-implicit parameters
 *
 * @tparam A parameter value type to extract
 * @tparam B environment type
 */
trait NormalExtractor[A, B] {
  def extract(a: A, name: String): B
}

/**
 * Extracts named values for implicit parameters
 *
 * @tparam A parameter value type to extract
 * @tparam B environment type
 */
trait ImplicitExtractor[A, B] {
  def extract(a: A, name: String): B
}

/**
 * Extracts values for which names are not known
 *
 * @tparam A parameter value type to extract
 * @tparam B environment type
 */
trait UnnamedExtractor[A, B] {
  def extract(a: A): B
}

/**
 * Extracts named values
 *
 * @tparam A parameter value type to extract
 * @tparam B environment type
 */
trait NamedExtractor[A, B] extends NormalExtractor[A, B] with ImplicitExtractor[A, B]

/**
  * Extracts values of one type from another
  *
  * @tparam A parameter value type to extract
  * @tparam B environment type
  */
trait Extractor[A, B] extends UnnamedExtractor[A, B] with NamedExtractor[A, B]

/**
 * An extraction strategy that looks for implicit [[NormalExtractor]]s to extract non-implicit parameters,
 * and implicit [[ImplicitExtractor]]s to extract implicit parameters.
 *
 * @tparam E environment type
 */
class ImplicitArgumentExtractors[E] extends ArgumentExtractionStrategy {

  override type Environment = E

  def extract[A](environment: E)(implicit extractor: UnnamedExtractor[E, A]): A =
    extractor.extract(environment)

  def extract[A](environment: E, name: String)(implicit extractor: NormalExtractor[E, A]): A =
    extractor.extract(environment, name)

  def extractImplicit[A](environment: E, name: String)(implicit extractor: ImplicitExtractor[E, A]): A =
    extractor.extract(environment, name)

}
