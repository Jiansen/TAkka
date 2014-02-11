package sample.scala.parameterizedtype.typebound

class ArrayStack[E] extends Stack[E]{
  private var list:List[E] = Nil
  
  def empty():Boolean = {
    return list.size == 0
  }
  
  def push[T <: E](elt:T):Unit = {
    list = elt :: list
  }
  def pop():E = {
    val elt:E = list.head
    list = list.tail
    return elt
  }
  
  override def toString():String = {
    return "stack"+list.toString.drop(4)
  }
}