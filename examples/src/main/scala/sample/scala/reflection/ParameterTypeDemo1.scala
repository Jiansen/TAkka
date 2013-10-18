package sample.scala.reflection

class Base1[A, B]{
  def p(a:A, b:B){
    println("a: "+a+"\nb: "+b)
  }
}

class Foo1[B] extends Base1[Int, B]

class Bar1 extends Foo1[String]

object ParameterTypeDemo1 extends App{
  val bar = new Bar1()
  bar.p(3, "Hello")
//  bar.p("3", "Hello")  
//  type mismatch; found : String("3") 
//	 required: Int  
}