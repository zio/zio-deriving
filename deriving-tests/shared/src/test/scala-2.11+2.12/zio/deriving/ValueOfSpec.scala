package zio.deriving

import zio.test._

object ValueOfSpec extends ZIOSpecDefault {
  def spec: ZSpec[Environment, Any] = suite("ValueOfSpec")(
    test("Getting value of a case object") {

      val actual = summonValueOf[ACaseObject]
      assert(actual.value)(Assertion.equalTo(ACaseObject))
    }
  )

  type ACaseObject = ACaseObject.type
  case object ACaseObject

  def summonValueOf[T](implicit ev: ValueOf[T]): ValueOf[T] = ev
}
