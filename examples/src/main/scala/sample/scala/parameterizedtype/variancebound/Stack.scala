package sample.scala.parameterizedtype.variancebound

trait Stack[+E] {
  def empty():Boolean
  def push[T >: E](elt: T): Stack[T]
  def pop():(E, Stack[E])
}