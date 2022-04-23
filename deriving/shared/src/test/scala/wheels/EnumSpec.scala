package wheels.enums
import zio.test._
object EnumSpec extends ZIOSpecDefault {

  sealed trait Foo
  object Foo {
    case object Bar extends Foo
    case object Baz extends Foo

    val values: List[Foo] = Enum.derived[Foo].values
  }

  override def spec: ZSpec[Environment, Any] = suite("EnumSpec")(
    test("values") {
      assertTrue(Foo.values == List(Foo.Bar, Foo.Baz))
    }
  )
}
