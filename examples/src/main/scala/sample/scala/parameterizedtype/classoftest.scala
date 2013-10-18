package sample.scala.parameterizedtype

object classtest extends App{
  def inspect[T](l:List[T])(implicit manifest : scala.reflect.Manifest[T]) = println(manifest.toString)
  
  inspect(List(1,2,3,4))
  inspect(List(List(1,2),List(3,4)))
}