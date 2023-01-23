package zio.derivingtest

import scala.annotation._
import zio.deriving.Meta

object MetaTestExamples {
  final case class field(name: String) extends Annotation

  @field("sealed trait Gaz")
  sealed trait Gaz
  object Gaz {
    @field("case class Foo")
    case class Foo(@field("case class field s") s: String)                                                 extends Gaz
    @field("case class Bar")
    case class Bar(@field("case class field txt") txt: String, @field("case class field <+>") `<+>`: Long) extends Gaz
    @field("case class |++|")
    case class |++|()                                                                                      extends Gaz
    @field("case object Car")
    case object Car                                                                                        extends Gaz
  }
}
import MetaTestExamples._
import Gaz._

class MetaTest extends junit.framework.TestCase {

  def assertEquals[A](expected: A, got: A): Unit =
    assert(expected == got, s"$got != $expected")

  def testName(): Unit = {
    assertEquals("Gaz", Meta[Gaz].name)
    assertEquals("Foo", Meta[Foo].name)
    assertEquals("Bar", Meta[Bar].name)
    assertEquals("|++|", Meta[|++|].name)
    assertEquals("Car", Meta[Car.type].name)
  }

  def testAnnotations(): Unit = {
    assertEquals(List(field("sealed trait Gaz")), Meta[Gaz].annotations)
    assertEquals(List(field("case class Foo")), Meta[Foo].annotations)
    assertEquals(List(field("case class Bar")), Meta[Bar].annotations)
    assertEquals(List(field("case class |++|")), Meta[|++|].annotations)
    assertEquals(List(field("case object Car")), Meta[Car.type].annotations)
  }

  def testFieldNames(): Unit = {
    assertEquals(List(), Meta[Gaz].fieldNames.toList)
    assertEquals(List("s"), Meta[Foo].fieldNames.toList)
    assertEquals(List("txt", "<+>"), Meta[Bar].fieldNames.toList)
    assertEquals(List(), Meta[|++|].fieldNames.toList)
    assertEquals(List(), Meta[Car.type].fieldNames.toList)
  }

  def testFieldAnnotations(): Unit = {
    assertEquals(Nil, Meta[Gaz].fieldAnnotations.toList)
    assertEquals(List(List(field("case class field s"))), Meta[Foo].fieldAnnotations.toList)
    assertEquals(
      List(List(field("case class field txt")), List(field("case class field <+>"))),
      Meta[Bar].fieldAnnotations.toList
    )
    assertEquals(Nil, Meta[|++|].fieldAnnotations.toList)
    assertEquals(Nil, Meta[Car.type].fieldAnnotations.toList)
  }

}
