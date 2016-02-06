package io.github.barnardb.scalainvoke

/**
 * A strategy for extracting argument values for parameters of a given name and type, given an "environment" of type `E`.
 *
 * Implementers of this class must define methods that have signatures such that
 * code invoking them could appear to be calling methods declared as:
 * {{{
 *   def extract[A](environment: Environment): A
 *   def extract[A](environment: Environment, name: String): A
 *   def extractImplicit[A](environment: Environment, name: String): A
 * }}}
 * These methods aren't explicitly declared on this class so that implementers are free to
 * add implicit parameters, use macros, etc.
 *
 * @tparam E "environment" type from which values can be extracted
 */
trait ArgumentExtractionStrategy {
  type Environment
}
