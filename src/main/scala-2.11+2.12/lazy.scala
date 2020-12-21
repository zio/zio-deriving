package shapely

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait LazyCompat { this: Lazy.type =>
  implicit def gen[A]: Lazy[A] = macro LazyMacros.gen[A]
}
object LazyMacros {
  def gen[A: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val A = c.weakTypeOf[A]
    q"""_root_.shapely.Lazy(_root_.scala.Predef.implicitly[$A])"""
  }
}
