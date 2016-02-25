package io.getquill.monad

import language.higherKinds

import java.util.concurrent.{ CountDownLatch, TimeUnit }
import java.util.concurrent.atomic.AtomicInteger

import util.control.NonFatal
import util.{ Try, Success, Failure }
import collection.generic.CanBuildFrom
import reflect.ClassTag

trait IOMonad {

  def sequence[A, M[X] <: TraversableOnce[X], E <: Effect](in: M[IO[A, E]])(implicit cbf: CanBuildFrom[M[IO[A, E]], A, M[A]]): IO[M[A], E]

  def unit: IO[Unit, Effect]

  def failed[T](exception: Throwable): IO[T, Effect] = apply(throw exception)

  def successful[T](result: T): IO[T, Effect] = apply(result)

  def fromTry[T](result: Try[T]): IO[T, Effect] = unit.transform(_ => result)

  def apply[T](body: => T): IO[T, Effect] = unit.map(_ => body)

  def find[T, E <: Effect](ios: collection.immutable.Iterable[IO[T, E]])(p: T => Boolean): IO[Option[T], E] =
    sequence(ios).map(_.find(p))

  def foldLeft[T, R, E <: Effect](ios: collection.immutable.Iterable[IO[T, E]])(zero: R)(op: (R, T) => R): IO[R, E] =
    sequence(ios).map(_.foldLeft(zero)(op))

  def reduceLeft[T, R >: T, E <: Effect](ios: collection.immutable.Iterable[IO[T, E]])(op: (R, T) => R): IO[R, E] =
    sequence(ios).map(_.reduceLeft(op))

  def traverse[A, B, M[X] <: TraversableOnce[X], E <: Effect](in: M[A])(fn: A => IO[B, E])(implicit cbf: CanBuildFrom[M[A], B, M[B]]): IO[M[B], E] =
    sequence(in.map(fn)).map(r => cbf().++=(r).result)

  trait IO[+T, -E <: Effect] {

    def zip[U, E2 <: Effect](that: IO[U, E2]): IO[(T, U), E with E2]
    def transformWith[S, E2 <: Effect](f: Try[T] => IO[S, E2]): IO[S, E with E2]

    def failed: IO[Throwable, E] =
      transform {
        case Failure(t) => Success(t)
        case Success(v) => Failure(new NoSuchElementException("IO.failed not completed with a throwable."))
      }

    def transform[S](s: T => S, f: Throwable => Throwable): IO[S, E] =
      transform {
        case Success(r) => Try(s(r))
        case Failure(t) => Try(throw f(t)) // will throw fatal errors!
      }

    def transform[S](f: Try[T] => Try[S]): IO[S, E] =
      transformWith(x => fromTry(f(x)))

    def map[S](f: T => S): IO[S, E] = transform(_.map(f))

    def flatMap[S, E2 <: Effect](f: T => IO[S, E2]): IO[S, E with E2] = transformWith {
      case Success(s) => f(s)
      case Failure(_) => this.asInstanceOf[IO[S, E with E2]]
    }

    def filter(p: T => Boolean): IO[T, E] =
      map { r => if (p(r)) r else throw new NoSuchElementException("IO.filter predicate is not satisfied") }

    final def withFilter(p: T => Boolean): IO[T, E] = filter(p)

    def collect[S](pf: PartialFunction[T, S]): IO[S, E] =
      map {
        r => pf.applyOrElse(r, (t: T) => throw new NoSuchElementException("IO.collect partial function is not defined at: " + t))
      }

    def recover[U >: T](pf: PartialFunction[Throwable, U]): IO[U, E] =
      transform { _ recover pf }

    def recoverWith[U >: T, E2 <: Effect](pf: PartialFunction[Throwable, IO[U, E2]]): IO[U, E with E2] =
      transformWith {
        case Failure(t) => pf.applyOrElse(t, (_: Throwable) => this)
        case Success(_) => this
      }

    def zipWith[U, R, E2 <: Effect](that: IO[U, E2])(f: (T, U) => R): IO[R, E with E2] =
      zip(that).map(f.tupled)

    def fallbackTo[U >: T, E2 <: Effect](that: IO[U, E2]): IO[U, E with E2] =
      if (this eq that) this
      else recoverWith { case _ => that } recoverWith { case _ => this }

    def andThen[U](pf: PartialFunction[Try[T], U]): IO[T, E] =
      transform {
        result =>
          pf.applyOrElse[Try[T], Any](result, Predef.identity[Try[T]])
          result
      }
  }
}
