package sample.scala.parameterizedtype.covariance

//trait Stack[+E] {
//  def empty():Boolean
//  def push(elt:E):Unit  // covariant type E occurs in contravariant position in type E of value  elt
//  def pop():E
//}

//trait Stack[-E] {
//  def empty():Boolean
//  def push(elt:E):Unit  
//  def pop():E // contravariant type E occurs in covariant position in type ()E of method pop
//}

trait Stack[+E] {
  def empty():Boolean
  def push[T >: E](elt: T): Stack[T]
  def pop():(E, Stack[E])
}