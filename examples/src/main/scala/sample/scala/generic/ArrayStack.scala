package sample.scala.generic

import scala.collection.immutable.List
class ArrayStack[E] extends Stack[E]{
  private var list:List[E] = Nil
  
  def empty():Boolean = {
    return list.size == 0
  }
  
  def push(elt:E):Unit = {
    this.list = list.:+(elt)
  }
  def pop():E = {
    val elt:E = list(list.size-1)
    this.list = list.take(list.size-1);
    return elt
  }
  
  override def toString():String = {
    return "stack"+list.toString
  }
}