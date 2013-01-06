package takka.nameserver

import scala.reflect.runtime.universe.{TypeTag}
import scala.language.existentials

/**
  * A local typed name server.  
  * 
  * 
  * {{{
  * object NameServerTest extends App {
  *   import NameServer._
  *   
  *   set(TSymbol[Int]('Int), 1)
  *  val i = get(TSymbol[Int]('Int))
  *  println(i)// Some(1)
  *  val j = get(TSymbol[Int]('Intn))
  *  println(j)// None
  *  val k = get(TSymbol[Double]('Int))
  *  println(k)// None  
  *  
  *  
  *  class Top
  *  class Super extends Top
  *  class Middle extends Super
  *  class Sub extends Middle
  *  class Bot extends Sub
  *  
  *  
  *  set[Middle](TSymbol[Middle]('symbol), new Middle)
  *  // OR  set(TSymbol[Middle]('symbol), new Middle)
  *  val b = get(TSymbol[Bot]('symbol))
  *  val t = get(TSymbol[Top]('symbol))
  *  println("bottom "+b) // bottom None
  *  println("top "+ t)  // top Some(nameserver.NameServer2Test$Middle@785f8172)
  * }
  * }}}
  */  
object NameServer {
  private val nameMap = new scala.collection.mutable.HashMap[TSymbol[_], TValue[_]]
  
  /**
   * Register a typed symbol with a value of corresponding type.
   * 
   * Throws a 'NamesHasBeenRegisteredException' if
   * the name has been used by the name server.
   */
  @throws(classOf[NamesHasBeenRegisteredException])
  def set[T:TypeTag](name:TSymbol[T], value:T):Unit = synchronized {
    val tValue = TValue[T](value)
    if (nameMap.contains(name)){
      throw new NamesHasBeenRegisteredException(name)
    }else{
      nameMap.+=((name, tValue))
    }
  }
  
  /**
   * Cancel the entry associated with 'name'. if
   * (i)  'name'.symbol is registered, and
   * (ii) 'name'.type (intention type) :> registered type
   * Otherwise, do noting.
   */
  def unset[T](name:TSymbol[T]):Unit = synchronized {
    if (nameMap.contains(name)  && nameMap(name).t <:< name.t  ){// intention type is a super type of registered type
      nameMap -= name
    }else{ // nameMap.contains(name) && (nameMap(name).t <:< name.t) && (!(nameMap(name).t =:= name.t))
      //DO nothing
    }
  }
  
  /**
   * Return Some if 'name' is associated with a value, and '''T''' is a supertype of the registered type; Otherwise return None
   */
  def get[T](name:TSymbol[T]):Option[T] = synchronized {
    if (!nameMap.contains(name)) {return None}
    else { 
      val tValue = nameMap(name)
      if (tValue.t <:< name.t) {
        return Some(tValue.value.asInstanceOf[T])
      }else{
        return None
      }
    }
  }
}

case class NamesHasBeenRegisteredException(name:TSymbol[_]) extends Exception("Name "+name+" has been registered.")



