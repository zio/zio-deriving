package wheels.enums

object Examples {
  sealed trait Foo
  object Foo {
    case object Bar extends Foo
    case object Baz extends Foo

    val values: List[Foo] = Enum.derived[Foo].values
  }
}
import Examples._

class EnumTests extends junit.framework.TestCase {

  def assertEquals[A](expected: A, got: A): Unit = assert(expected == got, s"$got != $expected")

  def testValues: Unit =
    assertEquals(List(Foo.Bar, Foo.Baz), Foo.values)

}
