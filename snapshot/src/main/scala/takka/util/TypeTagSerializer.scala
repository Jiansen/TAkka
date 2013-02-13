package takka.util
import scala.reflect.runtime.universe._
import takka.nameserver.TSymbol
import scala.Symbol

object TypeTagSerializer{
  import scala.collection.mutable.HashMap
  
  val typestore = new HashMap[String, TypeTag[_]]
  
  def register[T](t:TypeTag[T]){
    typestore += ((t.toString, t))
  }
  
  def serialize[T](t:TypeTag[T]):String = {
    t.toString()
  }
  
  def deserialize(s:String):TypeTag[_] = {
    typestore(s)
  }
}


@SerialVersionUID( 1L )
case class SeriTSymbol(val tagString:String)(val symbol:Symbol)

object TSymbolSerializer{  
  def serialize[T:TypeTag](t:TSymbol[T]):SeriTSymbol = {
    SeriTSymbol(TypeTagSerializer.serialize(typeTag[T]))(t.symbol)
  }
  
  def deserialize(s:SeriTSymbol):TSymbol[_] = {
    TSymbol(s.symbol)(TypeTagSerializer.deserialize(s.tagString))
  }
}
