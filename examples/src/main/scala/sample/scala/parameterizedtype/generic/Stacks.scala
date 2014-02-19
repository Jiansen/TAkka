package sample.scala.parameterizedtype.generic

object Stacks {
  def reverse[T](in:Stack[T]):Stack[T] = {
    val out = new ListStack[T]
    while(!in.empty){
      val elt = in.pop
      out.push(elt)
    }
    return out
  }
}