package takka.nameserver

/**
  * A local typed name server.
  */  
// TODO: implement a global (eventually) consistent name server
object NameServer {
  private val nameMap = new scala.collection.mutable.HashMap[TSymbol[_], TValue[_]]
  
  // TODO: what if users don't handler the exception?
  @throws(classOf[NamesHasBeenRegisteredException])
  def set[T:Manifest](name:TSymbol[T], value:T) = {
    val tValue = TValue[T](value)
    if (nameMap.contains(name)){
      throw new NamesHasBeenRegisteredException(name)
    }else{
      nameMap.+=((name, tValue))
    }
  }
  
  def unset[T](name:TSymbol[T]) = {
    nameMap -= name
  }
  
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

/*
object NameServer3Test extends App {
  import NameServer3._
  
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
*/