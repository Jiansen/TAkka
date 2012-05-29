package takka.nameserver

case class TSymbol[T](val symbol:Symbol)(implicit val t:Manifest[T]) {
  override def hashCode():Int = symbol.hashCode()
}

case class TValue[T](val value:T)(implicit val t:Manifest[T])

object tsymboltest extends App{
  class Top
  class Super extends Top
  class Middle extends Super
  class Sub extends Middle
  class Bot extends Sub
  
  
  val s1 = TSymbol[Top]('top)
  val s2 = TSymbol[Sub]('top)
  
  println(s1 == s2) // true
}
