package zio.deriving

import scala.annotation._
import zio.test._
object MetaSpec extends ZIOSpecDefault {
  import Gaz._

  def spec: ZSpec[Environment, Any] = suite("MetaSpec")(
    test("name") {
      assertTrue(
        Meta[Gaz].name == "Gaz",
        Meta[Foo].name == "Foo",
        Meta[Bar].name == "Bar",
        Meta[|++|].name == "|++|",
        Meta[Car.type].name == "Car"
      )
    },
    test("annotations") {
      assertTrue(
        Meta[Gaz].annotations == List(field("sealed trait Gaz")),
        Meta[Foo].annotations == List(field("case class Foo")),
        Meta[Bar].annotations == List(field("case class Bar")),
        Meta[|++|].annotations == List(field("case class |++|")),
        Meta[Car.type].annotations == List(field("case object Car"))
      )
    },
    test("fieldNames") {
      assertTrue(
        Meta[Gaz].fieldNames.toList == List(),
        Meta[Foo].fieldNames.toList == List("s"),
        Meta[Bar].fieldNames.toList == List("txt", "<+>"),
        Meta[|++|].fieldNames.toList == List(),
        Meta[Car.type].fieldNames.toList == List()
      )
    },
    test("fieldAnnotations") {
      assertTrue(
        Meta[Gaz].fieldAnnotations.toList == List(),
        Meta[Foo].fieldAnnotations.toList == List(List(field("case class field s"))),
        Meta[Bar].fieldAnnotations.toList == List(
          List(field("case class field txt")),
          List(field("case class field <+>"))
        ),
        Meta[|++|].fieldAnnotations.toList == List(),
        Meta[Car.type].fieldAnnotations.toList == List()
      )
    }
  )

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
