package sample.scala.parameterizedtype.variance

object ArrayCopy extends App{
  def copy[E, T>:E, S<:E](des:Array[T], src:Array[S]):Unit = {
    for( i <- 0 until src.length){
      des(i) = src(i)
    }
  }
  
  val objs:Array[Any] = Array(new Integer(2), 3.14, "four");
  val ints:Array[Integer] = Array(new Integer(5), new Integer(6));
  
  copy[Object, Any, Integer](objs, ints)
  
  println(objs.deep.mkString(", "))
  
}