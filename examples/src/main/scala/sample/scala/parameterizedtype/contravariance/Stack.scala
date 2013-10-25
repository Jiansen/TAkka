package sample.scala.parameterizedtype.contravariance

trait Stack[+E] {
  def empty():Boolean
  def push[T >: E](elt: T): Stack[T]
  def pop():(E, Stack[E])
}