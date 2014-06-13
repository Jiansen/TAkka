/*
scalacheck_2.9.0-1-1.9.jar could be downloaded from 
http://code.google.com/p/scalacheck/downloads/list
*/

import join._
import scala.concurrent.ops._

import org.scalacheck.Prop.forAll
import org.scalacheck._
import org.scalacheck.Prop._

object join_pattern_test extends App{

  var c1 = 0
  var c2 = 0
  var c3 = 0
  var c4 = 0
  
  
  object join extends Join{  
    object A extends AsyName[Int]  
    object B extends AsyName[Int]
    object C extends AsyName[Int]
    object D extends AsyName[Int]
    object E extends AsyName[Int]
    
    join {
     case A(1) and B(1) =>  { c1 += 1 }
     case C(2) and B(2) and D(2) =>  { c2 += 1 }
     case B(3) and D(3) and E(3) =>  { c3 += 1 }
     case E(4)=>  { c4 += 1 }
    }
    
  }
      
  
  val ch_msg_gen_1 = Gen.oneOf(
       (join.A, 1),
       (join.B, 1), (join.B, 2), (join.B, 3),
                    (join.C, 2),
                    (join.D, 2), (join.D, 3),
                                 (join.E, 3), (join.E, 4)
      )

  val ch_msg_gen_2 = Gen.oneOf(
       (join.A, 1), (join.A, 2), (join.A, 3),
       (join.B, 1), (join.B, 2), (join.B, 3),
       (join.C, 1), (join.C, 2), (join.C, 3),
       (join.D, 1), (join.D, 2), (join.D, 3),
       (join.E, 1), (join.E, 2), (join.E, 3), (join.E, 4)
      )

//  val prop = forAll (ch_msg_gen_1) { ch_msg =>      
  val prop = forAll (ch_msg_gen_2) { ch_msg =>
      ch_msg match {
         case (channel, msg) =>  {
           /*
           channel match {
             case join.A => println("join.A("+msg+")")
             case join.B => println("join.B("+msg+")")
             case join.C => println("join.C("+msg+")")
             case join.D => println("join.D("+msg+")")
             case join.E => println("join.E("+msg+")")
           }
           */
           //spawn {channel(msg)}
           channel(msg)
      }
      true
    }

  }

  def test100(){
join.A(2)
join.D(2)
join.E(3)
join.A(1)
join.A(2)
join.A(3)
join.E(1)
join.A(1)
join.D(3)
join.B(2)
join.E(2)
join.D(1)
join.B(3)
join.E(3)
join.A(1)
join.E(2)
join.A(3)
join.A(1)
join.E(3)
join.E(3)
join.D(3)
join.A(1)
join.D(1)
join.E(4)
join.D(1)
join.D(2)
join.B(1)
join.B(2)
join.E(1)
join.D(2)
join.B(3)
join.E(3)
join.E(3)
join.E(3)
join.A(1)
join.A(1)
join.B(1)
join.B(3)
join.D(3)
join.A(1)
join.D(1)
join.A(1)
join.E(4)
join.D(1)
join.D(2)
join.B(1)
join.B(1)
join.A(1)
join.B(3)
join.A(3)
join.E(3)
join.E(4)
join.A(3)
join.D(2)
join.D(2)
join.A(2)
join.E(1)
join.B(2)
join.A(1)
join.B(2)
join.E(2)
join.D(3)
join.A(2)
join.B(2)
join.B(1)
join.A(2)
join.A(1)
join.B(3)
join.B(3)
join.A(3)
join.B(2)
join.B(3)
join.A(2)
join.E(3)
join.E(2)
join.B(1)
join.A(3)
join.D(1)
join.D(2)
join.A(1)
join.E(1)
join.D(1)
join.E(1)
join.D(2)
join.E(4)
join.B(2)
join.E(2)
join.A(3)
join.E(2)
join.D(3)
join.E(4)
join.A(2)
join.A(1)
join.E(4)
join.B(3)
join.B(2)
join.E(2)
join.A(1)
join.E(3)
join.A(2)
  }
  
val start = java.lang.System.currentTimeMillis()

  var n = 0
  while (n < 10){
//    prop.check
    test100()
    n+=1
  }
//join.B(3)
//join.D(3)
//join.E(3)
//join.A(2)
//join.A(2)
//join.A(1)
//join.B(1)

val end = java.lang.System.currentTimeMillis()

println("Time: "+ (end-start))
  println("c1 = "+c1)
  println("c2 = "+c2)
  println("c3 = "+c3)
  println("c4 = "+c4)

  println("A: "+join.A.getQ)
  println("B: "+join.B.getQ)
  println("C: "+join.C.getQ)
  println("D: "+join.D.getQ)
  println("E: "+join.E.getQ)
  
  println(c1*2 + c2*3 + c3*3 + c4 + join.A.getQ.size + join.B.getQ.size + join.C.getQ.size + join.D.getQ.size + join.E.getQ.size)
  
}

/*
+ OK, passed 100 tests.
> Collected test data: 
100% Gen()
c1 = 6
c2 = 0
c3 = 5
c4 = 6
A: Queue(2, 2, 3, 3, 1, 1, 1, 1, 3, 3, 2, 1, 2, 2, 1, 3, 2, 3, 1, 3, 2, 1, 1, 2)
B: Queue(2, 2, 2, 2, 2, 3, 2, 3, 2, 3, 2)
C: Queue()
D: Queue(2, 1, 1, 1, 2, 2, 1, 1, 2, 2, 2, 1, 2, 1, 2)
E: Queue(1, 2, 2, 1, 3, 3, 3, 1, 2, 3, 2, 1, 1, 2, 2, 2, 3)
100
*/
