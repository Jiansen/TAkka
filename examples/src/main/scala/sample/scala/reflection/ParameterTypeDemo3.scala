package sample.scala.reflection

class Super
class Middle extends Super
class Sub extends Middle

class Base3[A>:Sub <:Super, B]{
  def p(a:A, b:B){
    println("a: "+a+"\nb: "+b)
  }
}

// class Foo3[B] extends Base3[Int, B]
// type arguments [Int,B] do not conform to class Base3's type parameter bounds [A >: sample.scala.Sub <: sample.scala.Super,B]

class Foo3[B] extends Base3[Sub, B]
class Bar3 extends Foo3[Int]

object ParameterTypeDemo3 extends App{
  val bar = new Bar3()
  bar.p(new Sub, 2)
//  bar.p(new Middle, 2)
//   type mismatch; found : sample.scala.Middle required: 
//	 sample.scala.Sub
}