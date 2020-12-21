Shapely is a Scala library for typeclass derivation with the design goals of:

- **compatibility** source compatible with both Scala 2 and Scala 3.
- **simple** the implementation has minimal macros and avoids type-level programming.
- **fast compilations** never slower than manual instances.
- **fast runtime** minimal overhead compared to manual instances.

The remainder of this document may be considered a standalone companion to [Functional Programming for Mortalz](https://leanpub.com/fpmortals).

## What is a Typeclass?

Typeclasses are a way to encode polymorphism, i.e. functions that work for a variety of different types.

A typeclass is a `trait` that:

- holds no state
- has a type parameter
- has at least one abstract method
- has laws
- may contain generalised methods
- may extend other typeclasses
- has one implementation for each concrete type

### Use in the Standard Library

The most visible example of a typeclass in the Scala standard library is the abstraction over numbers:

```scala
package scala.math

trait Ordering[T] {
  def compare(x: T, y: T): Int

  def lt(x: T, y: T): Boolean = compare(x, y) < 0
  def gt(x: T, y: T): Boolean = compare(x, y) > 0
}

trait Numeric[T] extends Ordering[T] {
  def plus(x: T, y: T): T
  def times(x: T, y: T): T
  def negate(x: T): T
  def zero: T

  def abs(x: T): T = if (lt(x, zero)) negate(x) else x
}
```

We can see all the key features of a typeclass in action:

- there is no state
- `Ordering` and `Numeric` have type parameter `T`
- `Ordering` has abstract `compare` and `Numeric` has abstract `plus`, `times`, `negate` and `zero`
- `Ordering` defines generalised `lt` and `gt` based on `compare`,
  `Numeric` defines `abs` in terms of `lt`, `negate` and `zero`
- `Numeric` extends `Ordering`
- there is only one `Numeric[Int]`

We can now write functions for types that "have a" `Numeric` typeclass:

```scala
def signOfTheTimes[T](t: T)(implicit N: Numeric[T]): T = {
  import N._
  times(negate(abs(t)), t)
}
```

We are no longer dependent on the OOP hierarchy of our input types, i.e. we don't demand that our input "is a" `Numeric`, which is vitally important if we want to support a third party class that we cannot redefine.

But the syntax for `signOfTheTimes` is clunky, there are some things we can do to clean it up. Introducing `ops` on the typeclass companion:

```scala
object Numeric {
  object ops {
    implicit class NumericOps[T](t: T)(implicit N: Numeric[T]) {
      def +(o: T): T = N.plus(t, o)
      def *(o: T): T = N.times(t, o)
      def unary_-: T = N.negate(t)
      def abs: T = N.abs(t)

      // duplicated from Ordering.ops
      def <(o: T): T = N.lt(t, o)
      def >(o: T): T = N.gt(t, o)
    }
  }
}
```

By also using `implicit` *context bounds* we can now write:

```scala
import Numeric.ops._

def signOfTheTimes[T: Numeric](t: T): T = -(t.abs) * t
```

### Typeclass Derivation

Typeclasses are *wired up* using the `implicit` language features.

An *instance* of `Ordering` is defined as an `implicit val` that implements the typeclass, and can provide faster implementations for the generalised methods (but they still box primitives, so aren't optimal):

```scala
implicit val OrderingDouble: Ordering[Double] = new Ordering[Double] {
  def compare(x: Double, y: Double): Int = java.lang.Double.compare(x, y)
  override def lt(x: Double, y: Double): Boolean = x < y
  override def gt(x: Double, y: Double): Boolean = x > y
}
```

with `implicit def` provided for most stdlib collections

```scala
implicit def seqOrdering[CC[X] <: Seq[X], T: Ordering]: Ordering[CC[T]] = ...
```

The process of generating typeclass instances from `implicit def` rules is what we mean when we say *typeclass derivation*.

To keep things nice and simple, and to avoid going into the *implicit scope* rules of the compiler, these derivations typically live on the companion object of the typeclass or data type (to do otherwise is to create an "orphan" and they are very difficult to reason about but are useful for library interop).

Say we create a simple data type like

```scala
case class Complex[A](r: A, i: A)
```

there is no way to automatically generate an `Ordering` instance, we must write one explicitly

```scala
object Complex {
  implicit def ordering[A: Ordering]: Ordering[Complex[A]] = new Ordering[Complex[A]] {
    def compare(x: Complex[A], y: Complex[A]): Int = ...
  }
}
```

That's where libraries such as `shapeless`, `magnolia` and `scalaz-deriving` come in. They allow users to write something closer to

```scala
object Complex {
  implicit def ordering[A: Ordering]: Ordering[Complex[A]] = Ordering.derived
}
```

`shapely` is an alternative approach; the remainder of this document will explain how to create derivation rules for typeclasses using shapely.

## Divide and Conquer

`shapely` defines some lawful user-facing typeclasses that abstract over typeclasses 🤯

```scala
trait XFunctor[F[_]] {
  def xmap[A, B](fa: F[A])(f: A => B, g: B => A): F[B]
}

trait Align[F[_]] {
  def align[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

trait Decide[F[_]] {
  def decide[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
}
```

Let's take a moment to read those type signatures. Your typeclass is the `F[_]`. So they read like:

- if you give me a typeclass for an `A`, and a way to convert an `A` into a `B` and a `B` into an `A`, then I can give you a typeclass for a `B`.
- if you give me a typeclass for an `A` and a typeclass for a `B`, then I can give you a typeclass for a tuple of `A` and `B`.
- if you give me a typeclass for an `A` and a typeclass for a `B`, then I can give you a typeclass for either `A` or `B`.

The laws are:

- identity: `fa == xmap(fa)(id, id)`
- composition: `xmap(xmap(fa, f1, g1), f2, g2) == xmap(fa, f2 . f1, g1 . g2)`
- associativity (align): `align(align(fa, fb), fc) == align(fa, align(fb, fc))`
- associativity (decide): `decide(decide(fa, fb), fc) == decide(fa, decide(fb, fc))`

`shapely` provides conveniences, to help implementing `XFunctor`

```scala
trait Covariant[F[_]] extends XFunctor[F] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

trait Contravariant[F[_]] extends XFunctor[F] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}
```

- if you give me a typeclass for an `A`, and a way to convert an `A` into a `B`, then I can give you a typeclass for a `B`.
- if you give me a typeclass for an `A`, and a way to convert a `B` into an `A`, then I can give you a typeclass for a `B`.

We can refer to these as the AC/DC typeclasses.

To get automatic derivation for any `case class` the typeclass author implements `Align`. For `sealed trait` they implement `Decide`, then mix `Derivable` into the companion. Here's the implementation for `Ordering`

```scala
implicit val xfunctor: Contravariant[Ordering] = new Contravariant[Ordering] {
  def contramap[A, B](fa: Ordering[A])(f: B => A): Ordering[B] = new Ordering[B] {
    def compare(x: B, y: B): Int = fa.compare(f(x), f(y))
  }
}
```

```scala
implicit val align: Align[Ordering] = new Align[Ordering] {
  def align[A, B](fa: Ordering[A], fb: Ordering[B]): Ordering[(A, B)] = new Ordering[(A, B)] {
    def compare(x: (A, B), y: (A, B)): Int = {
      val xs = fa.compare(x._1, y._1)
      if (xs != 0) xs
      else fb.compare(x._2, y._2)
    }
  }
}
```

```scala
implicit val decide: Decide[Ordering] = new Decide[Ordering] {
  def decide[A, B](fa: Ordering[A], fb: Ordering[B]): Ordering[Either[A, B]] = new Ordering[Either[A, B]] {
    def compare(x: Either[A, B], y: Either[A, B]): Int = (x, y) match {
      case (Left(xa), Left(ya)) => fa.compare(xa, ya)
      case (Right(xb), Right(yb)) => fb.compare(xb, yb)
      case (Left(_), Right(_)) => -1
      case (Right(_), Left(_)) => 1
    }
  }
}
```

Downstream users just need to type

```scala
case class Complex[A](r: A, i: A)
object Complex {
  implicit def ordering[A: Ordering]: Ordering[Complex[A]] = Ordering.derived
}
```

which can be simplified further in Scala 3 to

```scala
case class Complex[A](r: A, i: A) derives Ordering
```

And it's not just limited to case classes of 2 parameters, it works for all arities and sealed traits.

```scala
sealed trait Dimension
case class Cube(x: Double, y: Double, z: Double) extends Dimension
case class Tesseract(x: Double, y: Double, z: Double, t: Double) extends Dimension
```

The tests include a few more examples, which are good exercises. Try implementing `XFunctor`, `Align` and `Decide` for

```scala
  trait Equal[A]  {
    // type parameter is in contravariant (parameter) position
    def equal(a1: A, a2: A): Boolean
  }

  trait Default[A] {
    // type parameter is in covariant (return) position
    def default: Either[String, A]
  }

  trait Semigroup[A] {
    // type parameter is in both covariant and contravariant position (invariant)
    def add(a1: A, a2: A): A
  }
```

Homework:

- We can't implement `Decide[Semigroup]`, why not ?
- why should we not implement `Decide[Arbitrary]` (as in ScalaCheck / ScalaProps) ?
- what about case classes with no parameters, and case objects ?

## Lower Level

`shapely` is just a bunch of generated code. Typeclass authors can use that mechanism directly if they can't write lawful AC/DC instances.

`shapely` data types mirror cases classes and sealed traits of all shapes (hence the name!)

```scala
sealed trait Shape[A]
sealed trait CaseClass[A] extends Shape[A] { def value(i: Int): Any }
sealed trait SealedTrait[A] extends Shape[A] { def value: A ; def index: Int }
```

```
case class CaseClass0[A]() extends CaseClass[A]
case class CaseClass1[A, A1](_1: A1) extends CaseClass[A]
case class CaseClass2[A, A1, A2](_1: A1, _2: A2) extends CaseClass[A]
...
case class CaseClass64[A, A1, A2, ...](_1: A1, _2: A2, ...) extends CaseClass[A]
```

```scala
sealed trait SealedTrait1[A, A1 <: A] extends SealedTrait[A]
sealed trait SealedTrait2[A, A1 <: A, A2 <: A] extends SealedTrait[A]
...
sealed trait SealedTrait64[A, A1 <: A, A2 <: A, ...] extends SealedTrait[A]

object SealedTrait {
  case class _1[A, ...](value: A1) extends ...
  case class _2[A, ...](value: A2) extends ...
  ...
  case class _64[A, ...](value: A64) extends ...
}
```

The conversion between regular data types and the shapely shapes is handled by a typeclass that has a macro that automatically creates instances of a

```scala
trait Shapely[A, B <: Shape[A]] {
  def to(a: A): B
  def from(b: B): A
}
```

where `A` is your own case classes and sealed traits, `B` is a `shapely.Shape`.

The typeclass law is that roundtripping recovers an equal value

- identity: `to(from(b)) == b` AND `from(to(a)) == a`

The `Derivable` trait provides all the boilerplate to apply a "divide and conquer" approach for all arities, into the tuple and `Either` that were implemented in `Align` and `Decide`.

Typeclass authors can skip AC/DC and write codegen directly in their build tool, it's easy!

Two reasons to do this are: if the typeclass can't be expressed as a lawful AC/DC, or maximal performance is required.

Let's write a typeclass and codegen the derivation rules for it in sbt. We want a way to get all the `case object` values that extend a `sealed trait`, a fairly standard enumeration encoding in Scala 2. Our typeclass is

```scala
trait Enum[A] { self =>
  def values: List[A]

  final def map[B](f: A => B): Enum[B] = new Enum[B] {
    def values: List[B] = self.values.map(f)
  }
}
```

and we would want to implement it for `SealedTrait` something like this, hardcoded for sealed traits of 2 case objects

```scala
implicit def sealedtrait2[A, A1 <: A, A2 <: A](
  implicit A1: ValueOf[A1], A2: ValueOf[A2]
) = new Enum[SealedTrait2[A, A1, A2]] {
  def values = _1(A1.value) :: _2(A2.value) :: Nil
}
```

which we then need to convert into codegen rules. `project/ExamplesCodeGen.scala` contains the full code, like

```scala
    val enums = (1 to sum_arity).map { i =>
      val tparams = (1 to i).map(p => s"A$p <: A").mkString(", ")
      val tparams_ = (1 to i).map(p => s"A$p").mkString(", ")
      val implicits = (1 to i).map(p => s"A$p: ValueOf[A$p]").mkString(", ")
      val tycons = s"SealedTrait$i[A, $tparams_]"
      val work = (1 to i).map { p => s"_$p(A$p.value)" }.mkString("", " :: ", " :: Nil")
      s"""  implicit def sealedtrait$i[A, $tparams](implicit $implicits): Enum[$tycons] = new Enum[$tycons] {
         |    def values: List[$tycons] = $work
         |  }""".stripMargin
    }
    s"""package wheels.enums
       |
       |import shapely._
       |
       |private[enums] trait GeneratedEnums {
       |${enums.mkString("\n\n")}
       |}""".stripMargin
```

There's not much more than `.map` and `.mkString` going on here.

The way I create codegen rules is to start by copy/pasting another example and changing the strings to match the template that I wrote by hand. It would be possible to create a custom DSL for the templates, much like the Haskell [`boilerplate`](https://hackage.haskell.org/package/boilerplate). But that is left as an exercise to the reader.

With all the `Enum` specific stuff stripped out, that template looks like

```scala
    val sealedtraits = (1 to 64).map { i =>
      val tparams = (1 to i).map(p => s"").mkString("")
      val implicits = (1 to i).map(p => s"").mkString("")
      s""
    }
    s""
```

Homework:

- implement `Arbitrary` derivation rules for sealed traits

## Meta

Sometimes our typeclasses might need access to more information than just the types.

That's where `Meta` helps:

```scala
trait Meta[A] {
  def name: String
  def annotations: List[Annotation]
  def fieldNames: Array[String]
  def fieldAnnotations: Array[List[Annotation]]
}
```

which is provided by a macro. A typical usecase for this is to implement an encoder or decoder.

I rewrote `zio-json` to use shapely and this is how an `Encoder` is able to get the field names for case classes with annotations providing overrides

```scala
abstract class CaseClassEncoder[A, CC <: shapely.CaseClass[A]](M: shapely.Meta[A]) extends Encoder[CC] {
  val names: Array[String] = M.fieldAnnotations
    .zip(M.fieldNames)
    .map {
      case (a, n) => a.collectFirst { case field(name) => name }.getOrElse(n)
    }
    .toArray
  ...
}
```

similarly, we can pick a different encoder based on some annotations

```scala
  implicit def sealedtrait2[A, A1 <: A, A2 <: A](
    implicit M: Meta[A], M1: Meta[A1], M2: Meta[A2],
             A1: Encoder[A1], A2: Encoder[A2]
  ): Encoder[SealedTrait2[A, A1, A2]] = {
    M.annotations.collectFirst { case discriminator(n) => n } match {
      case None => ...
      case Some(hintfield) => ...
    }
  }
```

It is really easy to encode user-specified customisations with annotations instead of complex `implicit` machinery. Here we can see 3 annotations being used by a user to customise the form of their JSON

```scala
@discriminator("hint")
sealed abstract class Parent

@hint("Cain")
case class Child1() extends Parent

@hint("Abel")
case class Child2(@field("lamb") sheep: Int) extends Parent
```

Note here that we're able to get the `Meta` for every value in the sealed trait. That's not something that is easy to get hold of with shapeless or Magnolia and it opens up lots of possibilities for typeclass authors. In the case of `zio-json` it allows additional performance optimisations and security protection because we can skip over fields that are not going to be relevant to any of the subtypes.

## Lazy

This covers an advanced topic and you might never need to know about this.

In Scala 2.13 and newer it is possible to use by-name implicit parameters

```scala
def foo(implicit =>bar: Bar): Baz = ...
```

meaning that the `bar` is only evaluated when it is needed. It is a compiler warning in earlier versions of Scala and all implicit parameters are strictly evaluated. Normally that is not a problem, but think about what happens when deriving a typeclass for a data model such as

```scala
sealed trait ATree
case class Leaf(value: String) extends ATree
case class Branch(roots: List[ATree]) extends ATree
```

If we were to ask for implicit evidence for a typeclass called `Nuthin` like this

```scala
object ATree {
  implicit val nuthin: Nuthin[ATree] = Nuthin.derived
}
object Leaf {
  implicit val nuthin: Nuthin[Leaf] = Nuthin.derived
}
object Branch {
  implicit val nuthin: Nuthin[Branch] = Nuthin.derived
}
```

then we'd get into a problem because `Branch.nuthin` depends on `ATree.nuthin`, and `ATree.nuthin` depends on `Branch.nuthin` : a cyclic dependency. The Scala compiler accepts cycles and at runtime we get a `NullPointerException`.

You may think that a possible fix would be to use `lazy val` instead of `val`, but for reasons beyond my understanding, that results in infinite recursion. Sometimes.

The safest workaround is to use a `lazy val` on the `sealed trait` and `def` on the subtypes that refer to the parent

```scala
object ATree {
  implicit lazy val nuthin: Nuthin[ATree] = Nuthin.derived
}
object Leaf {
  implicit val nuthin: Nuthin[Leaf] = Nuthin.derived
}
object Branch {
  implicit def nuthin: Nuthin[Branch] = Nuthin.derived
}
```

an alternative encoding of the same idea (but hiding the subtype instances) is

```scala
object ATree {
  implicit lazy val nuthin: Nuthin[ATree] = {
    implicit def leaf: Nuthin[Leaf] = Nuthin.derived
    implicit def branch: Nuthin[Branch] = Nuthin.derived
    Nuthin.derived
  }
}
```

But that can still cause a problem if the `derived` rule is evaluating all its dependencies. And since implicit parameters are strictly evaluated, that's exactly what would happen.

A simple fix is to use by-name implicit parameters in your generated code. However, that doesn't work for Scala 2.12 and earlier.

A workaround is provided by

```scala
final class Lazy[A] private (private[this] var eval: () => A) {
  lazy val value: A = {
    val value0 = eval()
    eval = null
    value0
  }
}
object Lazy extends LazyCompat {
  def apply[A](a: =>A): Lazy[A] = new Lazy[A](() => a)
}
```

which can be used to turn any parameter into a by-name one, but adding the extra benefit of caching and not holding a reference to the calculation.

When you ask for an implicit `Lazy[Foo]` on Scala 2.13 or later, it will automatically use a by-name implicit. On Scala 2.12 or earlier, a macro ensures that the value of the implicit is calculated lazily, converting the call site into `Lazy(implicitly[Foo])` instead of `implicitly[Lazy[Foo]]`.

Unlike `shapeless.Lazy`, this one doesn't do anything funky with the compiler.

So when generating typeclasses, make sure to wrap the dependencies with `Lazy`. The JSON example above is more like

```scala
  implicit def sealedtrait2[A, A1 <: A, A2 <: A](
    implicit M: Meta[A], M1: Meta[A1], M2: Meta[A2],
             A1: Lazy[Encoder[A1]], A2: Lazy[Encoder[A2]]
  ) = ...
```

If you look at the actual definitions of the AC/DC typeclasses, you'll see that they are all by-name, to accomodate for this. Implementators may want to cache the values in a `lazy val`. Another design choice for shapely would be to use `Lazy` in the AC/DC API but it felt like pollution.
