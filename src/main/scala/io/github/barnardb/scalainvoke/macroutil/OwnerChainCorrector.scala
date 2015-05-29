package io.github.barnardb.scalainvoke.macroutil

import scala.reflect.macros.blackbox

/**
 * A utility for preventing owner chain corruption when manipulating Trees in macro implementations.
 *
 * To learn about owner chain corruption, see Eugene Burmako's excellent
 * [[https://github.com/scalamacros/macrology201/commits/part1 "Macrology 201" Part 1]].
 * The owner chain corruption sequence starts from Step 19.
 *
 * The code here is derived from the solution presented in
 * [[https://github.com/scalamacros/macrology201/commit/722854c9504e65e45663597c9b6baf93dca7d87f Step 24]] of that material,
 * which was in turn based on
 * [[https://gist.github.com/retronym/10640845#file-macro2-scala some work by Jason Zaugg]].
 * I just had a look at the latter, and now see that it has excellent comments.
 */
object OwnerChainCorrector {

  /**
   * Wrap a tree in a call to this method when splicing it into code where it will have a different owner than it had
   * when it was built.
   *
   * This method stores the tree's current owner,
   * and wraps the tree in a call to a macro that will later discover what the new owner should be and update the tree.
   *
   * Note that this will not work for arbitrary trees; I think it might only work for valid expression trees.
   */
  def splice(c: blackbox.Context)(tree: c.Tree): c.Tree = {
    import c.universe._
    import c.internal._, decorators._

    tree.updateAttachment(Internal.OriginalOwner(enclosingOwner))
    q"${symbolOf[Internal.type]}.changeOwner($tree)"
  }

  object Internal {
    import scala.language.existentials
    final case class OriginalOwner(symbol: blackbox.Context#Symbol)

    def changeOwner[A](tree: A): A = macro changeOwnerImpl
    def changeOwnerImpl(c: blackbox.Context)(tree: c.Tree): c.Tree = {
      import c.universe._
      import c.internal._, decorators._

      val OriginalOwner(originalOwner: Symbol) =
        tree.attachments.get[OriginalOwner].getOrElse {
          c.abort(tree.pos, s"OwnerChainCorrector error: Can't find the OriginalOwner that was attached by `splice`. This may mean that your tree got mangled by the splicing, possibly because it wasn't a valid expression. The tree currently looks like this: ${showRaw(tree)}")
        }
      c.internal.changeOwner(tree, originalOwner, c.internal.enclosingOwner)
    }
  }
}
