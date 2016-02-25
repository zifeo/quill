package io.getquill.monad

//import scala.util.Try

//object IO extends FreeMonad {
//  
//  type Instance[E <: Effect, R] = IO[E, R]
//  
//  def unit: Free[Unit] = ???
//  def bind[T, S](free: Free[T])(f: Try[T] => Free[S]): Free[S] = ???
//  
  sealed trait Effect
  sealed trait Read extends Effect
  sealed trait Write extends Effect
//  
//  sealed trait Action[E <: Effect, R]
//  case class Query[R](f: () => R) extends Action[Write, R]
//  case class Execute[R](f: () => R) extends Action[Write, R]
//  
//  class IO[E <: Effect, R] extends Instance[R]
//  
//}
