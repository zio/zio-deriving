package zio.deriving

trait LazyCompat { this: Lazy.type =>
  implicit def gen[A](implicit A: => A): Lazy[A] = Lazy(A)
}
