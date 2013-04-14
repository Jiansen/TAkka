/*
package takka.util

import scala.reflect.runtime.universe._
import takka.nameserver.TSymbol

object SerializationTest extends App {
  import java.io._
  import TypeTagSerializer._
  import TSymbolSerializer._
  
  class Sup
  class Sub extends Sup
  
  val sup = typeTag[Sup]
  val sub = typeTag[Sub]

  register(sup)
  register(sub)
  
  val sups = TypeTagSerializer.serialize(sup)
  val subs = TypeTagSerializer.serialize(sub)
  var sups2:String = _
  var subs2:String = _
  
  val sym = TSymbol[Int]('mySymbol)
  TypeTagSerializer.register(typeTag[Int])
  
  try {
    val supFOut = new FileOutputStream("sup");
    val supOut = new ObjectOutputStream(supFOut);
    supOut.writeObject(sups);
    val subFOut = new FileOutputStream("sub");
    val subOut = new ObjectOutputStream(subFOut);
    subOut.writeObject(sups);
    
    supOut.close();
    supFOut.close();
    subOut.close();
    subFOut.close();
    
    
    
    val symFOut = new FileOutputStream("sym");
    val symOut = new ObjectOutputStream(symFOut);
    symOut.writeObject(TSymbolSerializer.serialize(sym));
    
    val symFileIn = new FileInputStream("sym");
    val symin = new ObjectInputStream(symFileIn);
    val sym2 = symin.readObject().asInstanceOf[SeriTSymbol]
    println(TSymbolSerializer.deserialize(sym2))
    
    println(sym2)
    
    
    val supFileIn = new FileInputStream("sup");
    val supin = new ObjectInputStream(supFileIn);
    val subFileIn = new FileInputStream("sub");
    val subin = new ObjectInputStream(subFileIn);
    sups2 = supin.readObject().asInstanceOf[String];
    supin.close();
    supFileIn.close();  
    subs2 = subin.readObject().asInstanceOf[String];
    subin.close();
    subFileIn.close();  
  }catch {
    case i:IOException => i.printStackTrace()
  }
  
  val sup2 = TypeTagSerializer.deserialize(sups2)
  val sub2 = TypeTagSerializer.deserialize(subs2)
  println(sup2)
  println(sub2)
//  println(sub2 <:< sup2)
  
}
*/