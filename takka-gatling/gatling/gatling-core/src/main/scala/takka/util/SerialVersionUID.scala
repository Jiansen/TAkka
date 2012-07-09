package takka.util

/**
 * Annotation for specifying the `static SerialVersionUID` field
 * of a serializable class.
 * 
 * To help version management, users provide a string representation of version UID, e.g.
 * 
 * {{{
 * @SerialVersionUID("HelloClass-v-0-1")
 * class Hello 
 * }}}
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