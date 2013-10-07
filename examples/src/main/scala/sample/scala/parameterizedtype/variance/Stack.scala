package sample.scala.parameterizedtype.variance

//trait Stack[+E] {
//  def empty():Boolean
////  def push(elt: E): Unit // covariant type E occurs in contravariant position in type E of value  elt
////  def push[T<:E](elt: T): Unit // covariant type E occurs in contravariant position in type <: E of type T
//  def push[T>:E](elt: T): Unit
//  def pop():E
//}


trait Stack[-E] {
  def empty():Boolean
  def push(elt: E): Unit
//  def pop():E // contravariant type E occurs in covariant position in type ()E of method  pop
}