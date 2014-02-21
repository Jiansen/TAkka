package sample.scala.parameterizedtype.covariance

object Stacks {
  def reverse[T](in:Stack[T]):Stack[T] = {
    var temp = in
    var out:Stack[T] = new ListStack[T](Nil)
    while(!temp.empty){
      val eltStack = temp.pop
      temp = eltStack._2
      out = out.push(eltStack._1)
    }
    return out
  }
  
  def move[E, T>:E, S<:E](des:Stack[T], src:Stack[S]):Unit = {
    
    
//    for( i <- 0 until src.length){
//      des(i) = src(i)
//    }
  }
}