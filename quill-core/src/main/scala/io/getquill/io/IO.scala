package io.getquill.io

import scala.util.Success
import scala.util.Failure
import scala.util.Try
import scala.reflect.ClassTag
import scala.collection.generic.CanBuildFrom
import language.higherKinds

// fusion
// effect
// auto flatMap => zip if no data dep
trait IO[+T] {

  def onSuccess[U](pf: PartialFunction[T, U]): Unit =
    onComplete {
      case Success(v) => pf.lift(v)
      case _          =>
    }

  def onFailure[U](pf: PartialFunction[Throwable, U]): Unit =
    onComplete {
      case Failure(t) => pf.lift(t)
      case _          =>
    }

  def onComplete[U](f: Try[T] => U): Unit

  def failed: IO[Throwable] = {
    //    val p = Promise[Throwable]()
    //    onComplete {
    //      case Failure(t) => p success t
    //      case Success(v) => p failure (new NoSuchElementException("IO.failed not completed with a throwable."))
    //    }
    //    p.IO
    ???
  }

  def foreach[U](f: T => U): Unit = onComplete { _ foreach f }

  def transform[S](s: T => S, f: Throwable => Throwable): IO[S] = {
    //    val p = Promise[S]()
    //    onComplete {
    //      case Success(r) => p complete Try(s(r))
    //      case Failure(t) => p complete Try(throw f(t)) // will throw fatal errors!
    //    }
    //    p.IO
    ???
  }

  def map[S](f: T => S): IO[S] = {
    //    val p = Promise[S]()
    //    onComplete { v => p complete (v map f) }
    //    p.IO
    ???
  }

  def flatMap[S](f: T => IO[S]): IO[S] = {
    //    import impl.Promise.DefaultPromise
    //    val p = new DefaultPromise[S]()
    //    onComplete {
    //      case f: Failure[_] => p complete f.asInstanceOf[Failure[S]]
    //      case Success(v) => try f(v) match {
    //        // If possible, link DefaultPromises to avoid space leaks
    //        case dp: DefaultPromise[_] => dp.asInstanceOf[DefaultPromise[S]].linkRootOf(p)
    //        case fut => fut.onComplete(p.complete)(internalExecutor)
    //      } catch { case NonFatal(t) => p failure t }
    //    }
    //    p.IO
    ???
  }

  def filter(p: T => Boolean): IO[T] =
    map {
      r => if (p(r)) r else throw new NoSuchElementException("IO.filter predicate is not satisfied")
    }

  final def withFilter(p: T => Boolean): IO[T] = filter(p)

  def collect[S](pf: PartialFunction[T, S]): IO[S] =
    map {
      r => pf.applyOrElse(r, (t: T) => throw new NoSuchElementException("IO.collect partial function is not defined at: " + t))
    }

  def recover[U >: T](pf: PartialFunction[Throwable, U]): IO[U] = {
    //    val p = Promise[U]()
    //    onComplete { v => p complete (v recover pf) }
    //    p.IO
    ???
  }

  /**
   * Creates a new IO that will handle any matching throwable that this
   *  IO might contain by assigning it a value of another IO.
   *
   *  If there is no match, or if this IO contains
   *  a valid result then the new IO will contain the same result.
   *
   *  Example:
   *
   *  {{{
   *  val f = IO { Int.MaxValue }
   *  IO (6 / 0) recoverWith { case e: ArithmeticException => f } // result: Int.MaxValue
   *  }}}
   */
  def recoverWith[U >: T](pf: PartialFunction[Throwable, IO[U]]): IO[U] = {
    //    val p = Promise[U]()
    //    onComplete {
    //      case Failure(t) => try pf.applyOrElse(t, (_: Throwable) => this).onComplete(p.complete)(internalExecutor) catch { case NonFatal(t) => p failure t }
    //      case other => p complete other
    //    }
    //    p.IO
    ???
  }

  def zip[U](that: IO[U]): IO[(T, U)] = {
    //    val p = Promise[(T, U)]()
    //    onComplete {
    //      case f: Failure[_] => p complete f.asInstanceOf[Failure[(T, U)]]
    //      case Success(s) => that onComplete { c => p.complete(c map { s2 => (s, s2) }) }
    //    }
    //    p.IO
    ???
  }

  def fallbackTo[U >: T](that: IO[U]): IO[U] = {
    //    val p = Promise[U]()
    //    onComplete {
    //      case s @ Success(_) => p complete s
    //      case f @ Failure(_) => that onComplete {
    //        case s2 @ Success(_) => p complete s2
    //        case _ => p complete f // Use the first failure as the failure
    //      }
    //    }
    //    p.IO
    ???
  }

  def mapTo[S](implicit tag: ClassTag[S]): IO[S] = {
    val boxedClass = {
      val c = tag.runtimeClass
      if (c.isPrimitive) IO.toBoxed(c) else c
    }
    map(s => boxedClass.cast(s).asInstanceOf[S])
  }

  def andThen[U](pf: PartialFunction[Try[T], U]): IO[T] = {
    //    val p = Promise[T]()
    //    onComplete {
    //      case r => try pf.applyOrElse[Try[T], Any](r, Predef.conforms[Try[T]]) finally p complete r
    //    }
    //    p.IO
    ???
  }

}

object IO {

  private[io] val toBoxed = Map[Class[_], Class[_]](
    classOf[Boolean] -> classOf[java.lang.Boolean],
    classOf[Byte] -> classOf[java.lang.Byte],
    classOf[Char] -> classOf[java.lang.Character],
    classOf[Short] -> classOf[java.lang.Short],
    classOf[Int] -> classOf[java.lang.Integer],
    classOf[Long] -> classOf[java.lang.Long],
    classOf[Float] -> classOf[java.lang.Float],
    classOf[Double] -> classOf[java.lang.Double],
    classOf[Unit] -> classOf[scala.runtime.BoxedUnit])

  def failed[T](exception: Throwable): IO[T] = ???
  def successful[T](result: T): IO[T] = ???
  def fromTry[T](result: Try[T]): IO[T] = ???
  def apply[T](body: => T): IO[T] = ???
  def sequence[A, M[X] <: TraversableOnce[X]](in: M[IO[A]])(implicit cbf: CanBuildFrom[M[IO[A]], A, M[A]]): IO[M[A]] = {
    in.foldLeft(successful(cbf(in))) {
      (fr, fa) => for (r <- fr; a <- fa) yield (r += a)
    } map (_.result())
  }

  def firstCompletedOf[T](ios: TraversableOnce[IO[T]]): IO[T] = {
    //    val p = Promise[T]()
    //    val completeFirst: Try[T] => Unit = p tryComplete _
    //    ios foreach { _ onComplete completeFirst }
    //    p.IO
    ???
  }

  def find[T](ios: TraversableOnce[IO[T]])(p: T => Boolean): IO[Option[T]] = {
    ???
  }

  def fold[T, R](ios: TraversableOnce[IO[T]])(zero: R)(op: (R, T) => R): IO[R] = {
    if (ios.isEmpty) successful(zero)
    else sequence(ios).map(_.foldLeft(zero)(op))
  }

  def reduce[T, R >: T](ios: TraversableOnce[IO[T]])(op: (R, T) => R): IO[R] = {
    if (ios.isEmpty) failed(new NoSuchElementException("reduce attempted on empty collection"))
    else sequence(ios).map(_ reduceLeft op)
  }

  def traverse[A, B, M[X] <: TraversableOnce[X]](in: M[A])(fn: A => IO[B])(implicit cbf: CanBuildFrom[M[A], B, M[B]]): IO[M[B]] =
    in.foldLeft(successful(cbf(in))) { (fr, a) =>
      val fb = fn(a)
      for (r <- fr; b <- fb) yield (r += b)
    }.map(_.result())
}
