package sample.scala.generic

trait Stack[E] {
  def empty():Boolean
  def push(elt:E):Unit
  def pop():E
}