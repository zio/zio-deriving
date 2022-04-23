package zio.deriving

import scala.annotation.Annotation

trait Meta[A] {
  def name: String
  def annotations: List[Annotation]

  // field* metadata only exists for case classes
  def fieldNames: Array[String]
  def fieldAnnotations: Array[List[Annotation]]
}
object Meta extends MetaCompat {
  def apply[A](implicit A: Meta[A]): Meta[A] = A
}
