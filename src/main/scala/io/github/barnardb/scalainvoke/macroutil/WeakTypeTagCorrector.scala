package io.github.barnardb.scalainvoke.macroutil

import scala.language.higherKinds
import scala.reflect.macros.blackbox

/**
 * This is meant to work around my
 * [[https://github.com/barnardb/scala-lab/commit/f46bb5a4742c28658d8d27556847fcff87284a5e
 * problem with WeakTypeTags for types applied in macro definitions]].
 */
object WeakTypeTagCorrector {

  def weakTypeTagForAppliedType[C[_], CApplied <: C[_]: c.WeakTypeTag, A: c.WeakTypeTag](c: blackbox.Context): c.WeakTypeTag[C[A]] =
    c.WeakTypeTag[C[A]](c.universe.appliedType(c.weakTypeOf[CApplied].typeConstructor, List(c.weakTypeOf[A])))
}
