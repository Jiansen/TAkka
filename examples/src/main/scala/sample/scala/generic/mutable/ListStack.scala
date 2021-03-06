package sample.scala.generic.mutable

class ListStack[E] extends Stack[E]{
  private var list:List[E] = Nil
  
  def empty():Boolean = {
    return list.size == 0
  }
  
  def push(elt:E):Unit = {
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