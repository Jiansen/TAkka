package takka.nameserver

/**
  * A local typed name server.  
  * 
  * 
  * {{{
object NameServerTest extends App {
  import NameServer._
  
  set(TSymbol[Int]('Int), 1)
  val i = get(TSymbol[Int]('Int))
  println(i)// Some(1)
  val j = get(TSymbol[Int]('Intn))
  println(j)// None
  val k = get(TSymbol[Double]('Int))
  println(k)// None  
  
  
  class Top
  class Super extends Top
  class Middle extends Super
  class Sub extends Middle
  class Bot extends Sub
  
  
  set[Middle](TSymbol[Middle]('symbol), new Middle)
  // OR  set(TSymbol[Middle]('symbol), new Middle)
  val b = get(TSymbol[Bot]('symbol))
  val t = get(TSymbol[Top]('symbol))
  println("bottom "+b) // bottom None
  println("top "+ t)  // top Some(nameserver.NameServer2Test$Middle@785f8172)
}
  * }}}
  */  
object NameServer {
  private val nameMap = new scala.collection.mutable.HashMap[TSymbol[_], TValue[_]]
  
  /**
   * Tegister a typed symbol with a value of corresponding type.
   * 
   * Throws an 'NamesHasBeenRegisteredException' if
   * the name has been used by the name server.
   */
  @throws(classOf[NamesHasBeenRegisteredException])
  def set[T:Manifest](name:TSymbol[T], value:T):Unit = {
    val tValue = TValue[T](value)
    if (nameMap.contains(name)){
      throw new NamesHasBeenRegisteredException(name)
    }else{
      nameMap.+=((name, tValue))
    }
  }
  
  /**
   * Cancel the entry associated with 'name'.
   * 
   * The type parameter does not take into account, in another word, 
   * '''T''' may not be the same as the registered type.
   */
  def unset[T](name:TSymbol[T]):Unit = {
    nameMap -= name
  }
  
  /**
   * Return Some if 'name' is associated with a value, and '''T''' is a supertype of the registered type; Otherwise return None
   */
  def get[T](name:TSymbol[T]):Option[T] = {
    if (!nameMap.contains(name)) {return None}
    else { 
      val tValue = nameMap(name)
      if (name.t >:> tValue.t) {
        return Some(tValue.value.asInstanceOf[T])
      }else{
        return None
      }
    }
  }
}

case class NamesHasBeenRegisteredException(name:TSymbol[_]) extends Exception("Name "+name+" has been registered.")



