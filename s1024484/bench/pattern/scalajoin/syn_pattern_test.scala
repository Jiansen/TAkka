import join._
import scala.concurrent.ops._

object syn_pattern_test extends App{
  
  
  object join extends Join{  
    object A extends AsyName[Int]
    object Suc extends SynName[Int, Int]
    
    join {
     case A(1) and Suc(1) =>  Suc reply 2
     case A(2) and Suc(2) =>  Suc reply 3
    }
    
  }

  spawn {
//    join.A(1)
//    Thread.sleep(100)
    println("Suc(1): "+ join.Suc(1))
  }
//  spawn {    join.A(1) }
  spawn {
//      println("\n Before "+join.A.getQ)
    println("Suc(2): "+ join.Suc(2))
//      println("\n After "+join.A.getQ)

  }
  spawn{
    join.A(1)
    join.A(2)
    println(join.A.getQ)
  }
  
  
//  join.A(1)
//  println("Suc(1): "+ join.Suc(1))
/*  join.A(2)
  println("Suc(2): "+ join.Suc(2))
*/  
  
}