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