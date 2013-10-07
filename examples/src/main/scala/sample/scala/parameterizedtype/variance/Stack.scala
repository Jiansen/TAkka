package sample.scala.parameterizedtype.variance

trait Stack[+E] {
  def empty():Boolean
//  def push(elt: E): Stack[E] // covariant type E occurs in contravariant position in type E of value  elt
  def pop():E
}