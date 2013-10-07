package sample.scala.parameterizedtype.generic

import scala.collection.mutable.ArrayBuffer
class ArrayStack[E] extends Stack[E]{
  private val list:ArrayBuffer[E] = new ArrayBuffer[E]()
  
  def empty():Boolean = {
    return list.size == 0
  }
  
  def push(elt:E):Unit = {
    list += elt
  }
  def pop():E = {
    val elt:E = list.remove(list.size-1)
    return elt
  }
  
  override def toString():String = {
    return "stack"+list.toString.drop(11)
  }
}