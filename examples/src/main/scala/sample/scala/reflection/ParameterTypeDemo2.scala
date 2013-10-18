package sample.scala.reflection

class Base2[String, B]{
  def p(a:String, b:B){
    println("a: "+a+"\nb: "+b)
  }
}

class Foo2[B] extends Base2[Int, B]

class Bar2 extends Foo2[Int]

object ParameterTypeDemo2 extends App{
  val bar = new Bar2()
  bar.p(3, 2)
//  bar.p("hello", 2)  
//  type mismatch; found : String("hello") 
//	 required: Int  
}