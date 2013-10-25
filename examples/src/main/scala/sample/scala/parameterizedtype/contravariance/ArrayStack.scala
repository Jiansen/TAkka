package sample.scala.parameterizedtype.contravariance
class ArrayStack[+E](protected val list:List[E]) extends Stack[E]{  
  def empty():Boolean = {
    return list.size == 0
  }
  
  def push[T >: E](elt: T): Stack[T] = {
    new ArrayStack(elt :: list)
  }
  def pop():(E, Stack[E]) = {
    if (!empty) (list.head, new ArrayStack(list.tail))
    else throw new NoSuchElementException("pop of empty stack")
  }
  
  override def toString():String = {
    return "stack"+list.toString.drop(4)
  }
}