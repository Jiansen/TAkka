package sample.scala.parameterizedtype.covariance
class ListStack[+E](protected val list:List[E]) extends Stack[E]{
  def empty():Boolean = {
    return list.size == 0
  }
  
  def push[T >: E](elt: T): Stack[T] = {
    new ListStack(elt :: list)
  }
  def pop():(E, Stack[E]) = {
    if (!empty) (list.head, new ListStack(list.tail))
    else throw new NoSuchElementException("pop of empty stack")
  }
  
  override def toString():String = {
    return "stack"+list.toString.drop(4)
  }
}