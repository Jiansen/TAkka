/**
 * Copyright 2011-2012 eBusiness Information, Groupe Excilys (www.excilys.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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