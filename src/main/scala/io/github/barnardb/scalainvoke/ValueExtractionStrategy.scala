package io.github.barnardb.scalainvoke

/**
  * A strategy for extracting values the values needed to for an invocation, given an
  * [[ValueExtractionStrategy#Environment]].
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
  */
trait ValueExtractionStrategy {

  /**
    * The type from which values can be extracted
    */
  type Environment

  // See class description for information about these phantom methods
  //
  // def extract[A](environment: Environment): A
  // def extract[A](environment: Environment, name: String): A
  // def extractImplicit[A](environment: Environment, name: String): A

}
