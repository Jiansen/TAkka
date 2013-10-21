package sample.other

import scala.reflect._

object ManifestExample extends App {
  assert(List(1,2.0,"3").isInstanceOf[List[String]])
  // Compiler Warning :non-variable type argument String in type List[String] is unchecked since it is eliminated by 
  // erasure
  
  case class Foo[A](a: A)
  type F = Foo[_]
  assert(classManifest[F].toString.equals("sample.other.ManifestExample$Foo[<?>]"))
  assert(NoManifest.toString.equals("<?>"))
  
  assert(manifest[List[Int]].toString.equals("scala.collection.immutable.List[Int]"))
  assert(manifest[List[Int]].erasure.toString.equals("class scala.collection.immutable.List"))
  
  def typeName[T](x: T)(implicit m: Manifest[T]): String  = {
    m.toString
  }
  assert(typeName(2).equals("Int"))
    
  def boundTypeName[T:Manifest](x: T):String = {
    manifest[T].toString
  }
  assert(boundTypeName(2).equals("Int"))
  
  def isSubType[T: Manifest, U: Manifest] = manifest[T] <:< manifest[U]
  assert(isSubType[List[String], List[AnyRef]]) // true
  assert(! isSubType[List[String], List[Int]])    // false
}