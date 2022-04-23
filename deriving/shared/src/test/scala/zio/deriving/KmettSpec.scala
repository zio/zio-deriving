package zio.deriving

import scala.math.Ordering
import scala.annotation.tailrec
import zio.test._
import scala.annotation.nowarn

object KmettSpec extends ZIOSpecDefault {
  import KmettExamples._

  private def assertEquals[A](expected: A, got: A) =
    assertTrue(got == expected)

  def spec: ZSpec[Environment, Any] = suite("KmettSpec")(
    test("Semigroup") {
      assertEquals(3, 1 |+| 2) &&
      assertEquals(Foo(4, 6), Foo(1, 2) |+| Foo(3, 4))
    },
    test("Equal") {
      assertEquals(true, 1 === 1) &&
      assertEquals(false, 1 === 2) &&
      assertEquals(true, Foo(1, 2) === Foo(1, 2)) &&
      assertEquals(false, Foo(1, 2) === Foo(1, 1)) &&
      assertEquals(true, Bar() === Bar()) &&
      assertEquals(true, Car(1, 2, 3, 4, 5, 6) === Car(1, 2, 3, 4, 5, 6)) &&
      assertEquals(false, Car(1, 2, 3, 4, 5, 6) === Car(1, 2, 3, 4, 5, 7)) &&
      assertEquals(true, (Bar(): Parent) === (Bar(): Parent)) &&
      assertEquals(false, (Foo(1, 2): Parent) === (Bar(): Parent)) &&
      assertEquals(true, (Foo(1, 2): Parent) === (Foo(1, 2): Parent)) &&
      assertEquals(false, (Foo(1, 1): Parent) === (Foo(1, 2): Parent)) &&
      assertEquals(true, (Leaf(1): ATree) === (Leaf(1): ATree)) &&
      assertEquals(false, (Leaf(1): ATree) === (Leaf(2): ATree)) &&
      assertEquals(true, (Branch(List(Leaf(1))): ATree) === (Branch(List(Leaf(1))): ATree))

    },
    test("Default") {
      assertEquals(Right(Bar()), Default.value[Bar]) &&
      assertEquals(Right(Foo(0, 0)), Default.value[Parent]) &&
      assertEquals(Right(Tuple1(0)), Default.value[Tuple1[Int]]) &&
      assertEquals(Right((0, 0)), Default.value[(Int, Int)]) &&
      assertEquals(Right((0, 0, 0)), Default.value[(Int, Int, Int)]) &&
      assertEquals(Right(Left(0)), Default.value[Either[Int, Int]])
    }
  )

}

object KmettExamples {

  object OrderingOrphans extends Derivable[Ordering] {
    implicit def caseclass0[A]: Ordering[CaseClass0[A]] = new Ordering[CaseClass0[A]] {
      def compare(a1: CaseClass0[A], a2: CaseClass0[A]) = 0
    }

    implicit val xfunctor: Contravariant[Ordering] = new Contravariant[Ordering] {
      def contramap[A, B](fa_ : => Ordering[A])(f: B => A): Ordering[B] = new Ordering[B] {
        lazy val fa                  = fa_
        def compare(x: B, y: B): Int = fa.compare(f(x), f(y))
      }
    }

    implicit val align: Align[Ordering] = new Align[Ordering] {
      def align[A, B](fa_ : => Ordering[A], fb_ : => Ordering[B]): Ordering[(A, B)] = new Ordering[(A, B)] {
        lazy val fa = fa_
        lazy val fb = fb_

        def compare(x: (A, B), y: (A, B)): Int = {
          val xs = fa.compare(x._1, y._1)
          if (xs != 0) xs
          else fb.compare(x._2, y._2)
        }
      }
    }

    implicit val decide: Decide[Ordering] = new Decide[Ordering] {
      def decide[A, B](fa_ : => Ordering[A], fb_ : => Ordering[B]): Ordering[Either[A, B]] =
        new Ordering[Either[A, B]] {
          lazy val fa = fa_
          lazy val fb = fb_

          def compare(x: Either[A, B], y: Either[A, B]): Int = (x, y) match {
            case (Left(xa), Left(ya))   => fa.compare(xa, ya)
            case (Right(xb), Right(yb)) => fb.compare(xb, yb)
            case (Left(_), Right(_))    => -1
            case (Right(_), Left(_))    => 1
          }
        }
    }
  }

  trait Equal[A] {
    // type parameter is in contravariant (parameter) position
    def equal(a1: A, a2: A): Boolean
  }
  object Equal                                  extends Derivable[Equal] {
    implicit val int: Equal[Int] = new Equal[Int] {
      override def equal(a1: Int, a2: Int) = a1 == a2
    }

    implicit def list[A](implicit A: Equal[A]): Equal[List[A]] = new Equal[List[A]] {
      @tailrec override def equal(as1: List[A], as2: List[A]) = (as1, as2) match {
        case (Nil, Nil)                     => true
        case (a1 :: a1_tail, a2 :: a2_tail) =>
          if (!A.equal(a1, a2)) false
          else equal(a1_tail, a2_tail)
        case _                              => false
      }
    }

    implicit def caseclass0[A]: Equal[CaseClass0[A]] = new Equal[CaseClass0[A]] {
      override def equal(a1: CaseClass0[A], a2: CaseClass0[A]) = true
    }

    implicit val xfunctor: Contravariant[Equal] = new Contravariant[Equal] {
      override def contramap[A, B](fa_ : => Equal[A])(f: B => A) = new Equal[B] {
        lazy val fa                      = fa_
        override def equal(b1: B, b2: B) = fa.equal(f(b1), f(b2))
      }
    }

    implicit val align: Align[Equal] = new Align[Equal] {
      override def align[A, B](fa_ : => Equal[A], fb_ : => Equal[B]) = new Equal[(A, B)] {
        lazy val fa                                  = fa_
        lazy val fb                                  = fb_
        override def equal(ab1: (A, B), ab2: (A, B)) = fa.equal(ab1._1, ab2._1) && fb.equal(ab1._2, ab2._2)
      }
    }

    implicit val decide: Decide[Equal] = new Decide[Equal] {
      def decide[A, B](fa_ : => Equal[A], fb_ : => Equal[B]): Equal[Either[A, B]] = new Equal[Either[A, B]] {
        lazy val fa                                              = fa_
        lazy val fb                                              = fb_
        override def equal(ab1: Either[A, B], ab2: Either[A, B]) = (ab1, ab2) match {
          case (Left(a1), Left(a2))   => fa.equal(a1, a2)
          case (Right(b1), Right(b2)) => fb.equal(b1, b2)
          case _                      => false
        }
      }
    }
  }
  implicit class EqualOps[A](private val a1: A) extends AnyVal           {
    def ===(a2: A)(implicit S: Equal[A]): Boolean = S.equal(a1, a2)
  }

  // why use Default instead of Arbitrary as the example?
  // Firstly, Arbitrary shouldn't be a typeclass.
  // Secondly, Decide[Arbitrary] doesn't work as you'd expect.
  trait Default[A] {
    // type parameter is in covariant (return) position
    def default: Either[String, A]
  }
  object Default extends Derivable[Default] {
    def value[A](implicit A: Default[A]): Either[String, A] = A.default

    implicit val int: Default[Int] = new Default[Int] {
      override def default = Right(0)
    }

    implicit def caseclass0[A]: Default[CaseClass0[A]] = new Default[CaseClass0[A]] {
      override def default = Right(CaseClass0())
    }

    implicit val xfunctor: XFunctor[Default] = new Covariant[Default] {
      override def fmap[A, B](fa_ : => Default[A])(f: A => B) = new Default[B] {
        lazy val fa          = fa_
        override def default = fa.default match {
          case Left(err) => Left(err)
          case Right(a)  => Right(f(a))
        }
      }
    }
    implicit val align: Align[Default]       = new Align[Default] {
      override def align[A, B](fa_ : => Default[A], fb_ : => Default[B]) = new Default[(A, B)] {
        lazy val fa          = fa_
        lazy val fb          = fb_
        override def default = (fa.default, fb.default) match {
          case (Right(a), Right(b)) => Right((a, b))
          case (Left(err), _)       => Left(err)
          case (_, Left(err))       => Left(err)
        }
      }
    }
    implicit val decide: Decide[Default]     = new Decide[Default] {
      override def decide[A, B](fa_ : => Default[A], fb_ : => Default[B]) = new Default[Either[A, B]] {
        lazy val fa          = fa_
        override def default = fa.default match {
          case Left(err) => Left(err)
          case Right(a)  => Right(Left(a))
        }
      }
    }

  }

  trait Semigroup[A] {
    // type parameter is in both covariant and contravariant position (invariant)
    def add(a1: A, a2: A): A
  }
  object Semigroup                                  extends Derivable[Semigroup] {
    implicit val int: Semigroup[Int] = new Semigroup[Int] {
      override def add(a1: Int, a2: Int) = a1 + a2
    }

    implicit val xfunctor: XFunctor[Semigroup] = new XFunctor[Semigroup] {
      override def xmap[A, B](fa_ : => Semigroup[A])(f: A => B, g: B => A) = new Semigroup[B] {
        lazy val fa                    = fa_
        override def add(b1: B, b2: B) = f(fa.add(g(b1), g(b2)))
      }
    }

    implicit val align: Align[Semigroup] = new Align[Semigroup] {
      override def align[A, B](fa_ : => Semigroup[A], fb_ : => Semigroup[B]) = new Semigroup[(A, B)] {
        lazy val fa                                = fa_
        lazy val fb                                = fb_
        override def add(ab1: (A, B), ab2: (A, B)) = (fa.add(ab1._1, ab2._1), fb.add(ab1._2, ab2._2))
      }
    }
  }
  implicit class SemigroupOps[A](private val a1: A) extends AnyVal               {
    def |+|(a2: A)(implicit S: Semigroup[A]): A = S.add(a1, a2)
  }

  sealed trait Parent
  case class Foo(a: Int, b: Int)                                 extends Parent
  case class Bar()                                               extends Parent
  case class Car(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int) extends Parent
  case class Dar()                                               extends Parent
  case class Ear()                                               extends Parent
  case object Faz                                                extends Parent {
    implicit val equal: Equal[Faz.type]     = Equal.derived
    implicit val default: Default[Faz.type] = Default.derived
  }

  object Parent {
    implicit val equal: Equal[Parent]     = Equal.derived
    implicit val default: Default[Parent] = Default.derived
  }
  object Foo    {
    implicit val equal: Equal[Foo]         = Equal.derived
    implicit val semigroup: Semigroup[Foo] = Semigroup.derived
    implicit val default: Default[Foo]     = Default.derived
  }
  object Bar    {
    implicit val equal: Equal[Bar]     = Equal.derived
    implicit val default: Default[Bar] = Default.derived
  }
  object Car    {
    implicit val equal: Equal[Car]     = Equal.derived
    implicit val default: Default[Car] = Default.derived
  }
  object Dar    {
    implicit val equal: Equal[Dar]     = Equal.derived
    implicit val default: Default[Dar] = Default.derived
  }
  object Ear    {
    implicit val equal: Equal[Ear]     = Equal.derived
    implicit val default: Default[Ear] = Default.derived
  }

  sealed abstract class ATree
  final case class Leaf(value: Int)           extends ATree
  final case class Branch(roots: List[ATree]) extends ATree

  // object ATree {
  //   implicit lazy val equal: Equal[ATree] = Equal.derived
  // }
  // object Leaf {
  //   implicit def equal: Equal[Leaf] = Equal.derived
  // }
  // object Branch {
  //   implicit def equal: Equal[Branch] = Equal.derived
  // }
  object ATree {
    implicit lazy val equal: Equal[ATree] = {
      implicit def leaf: Equal[Leaf]     = Equal.derived
      implicit def branch: Equal[Branch] = Equal.derived
      Equal.derived
    }
  }

  case class Complex[A](r: A, i: A)
  object Complex {
    implicit def ordering[A: Ordering]: Ordering[Complex[A]] = {
      import OrderingOrphans._
      derived
    }
  }

  sealed trait Dimension
  case class Cube(x: Double, y: Double, z: Double)                 extends Dimension
  case class Tesseract(x: Double, y: Double, z: Double, t: Double) extends Dimension

  object Dimension {
    @nowarn
    implicit val ordering: Ordering[Dimension] = {
      import OrderingOrphans._
      derived
    }
  }
  object Cube      {
    implicit val ordering: Ordering[Cube] = {
      import OrderingOrphans._
      derived
    }
  }
  object Tesseract {
    implicit val ordering: Ordering[Tesseract] = {
      import OrderingOrphans._
      derived
    }
  }

}
