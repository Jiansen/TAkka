package takka.nameserver

/**
 * A typed symbol is a pair of a symbol and a type descriptor.
 * The type descriptor (Manifest) can be used at run-time for type comparison.
 */
case class TSymbol[T](val symbol:Symbol)(implicit val t:Manifest[T]) {
  /**
   * To support subtyping enquiry in typed name server,
   * the hash of a '''TSymbol''' only considers the symbol representation.
   */
  override def hashCode():Int = symbol.hashCode()
}

/**
 * A typed value contains a type descriptor (Manifest) which can be used at run-time.
 */
case class TValue[T](val value:T)(implicit val t:Manifest[T])

/*
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
*/