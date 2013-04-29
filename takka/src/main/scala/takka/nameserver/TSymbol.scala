package takka.nameserver

// import scala.reflect.runtime.universe.{TypeTag, Type, typeOf}
// import scala.reflect.runtime.universe._
import scala.reflect.Manifest
import scala.Symbol

/**
 * A typed symbol is a pair of a symbol and a type descriptor.
 * The type descriptor (TypeTag) can be used at run-time for type comparison.
 */
@SerialVersionUID( 1L )
case class TSymbol[-T:Manifest](val symbol:Symbol) {
  private [takka] val t:Manifest[_] = manifest[T]
  /**
   * To support subtyping enquiry in typed name server,
   * the hash of a '''TSymbol''' only considers the symbol representation.
   */
  override def hashCode():Int = symbol.hashCode()
}

/**
 * A typed value contains a type descriptor (TypeTag) which can be used at run-time.
 */
case class TValue[T:Manifest](val value:T){
  private [takka] val t:Manifest[_] = manifest[T]
}

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