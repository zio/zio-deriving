package wheels.enums
import zio.test._
object EnumSpec extends ZIOSpecDefault {
  import Examples._

  override def spec: ZSpec[Environment, Any] = suite("EnumSpec")(
    test("values") {
      val actual = Foo.values
      // println(s"actual: $actual")
      assert(Foo.values)(Assertion.equalTo(List(Foo.Bar, Foo.Baz)))
    }
  )
}

object Examples {
  sealed trait Foo
  object Foo {
    case object Bar extends Foo
    case object Baz extends Foo

    val values: List[Foo] = Enum.derived[Foo].values
  }
}
