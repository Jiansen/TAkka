package takka.util

/*
 * String.hashCode in JAVA is consistent
 * However, it returns an Int rather than a Long
 */
class SerialVersionUID(uidStr:String) extends scala.SerialVersionUID( uidStr.hashCode() )

/*
@SerialVersionUID("HelloClass-v-0-1")
class Hello 

object UIDTest extends App{
  
  val h = new Hello
  println(h.getClass().asInstanceOf[Serializable])

}
*/