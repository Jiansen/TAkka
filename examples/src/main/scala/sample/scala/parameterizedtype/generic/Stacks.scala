package sample.scala.parameterizedtype.generic

object Stacks {
  def reverse[T](in:Stack[T]):Stack[T] = {
    val out = new ArrayStack[T]
    while(!in.empty){
      val elt = in.pop
      out.push(elt)
    }
    return out
  }
}