package sample.scala.parameterizedtype.typebound

trait Stack[E] {
  def empty():Boolean
  def push[T <: E](elt:T):Unit
  def pop():E
}