import join._
import scala.concurrent.ops._
object ScalaJoinfib extends App{
  object fibjoin extends Join{
    object fib extends SynName[Int, Int]
    join{
      case fib(n) => 
        if (n<=2) fib reply 1
        else fib reply (fib(n-1)+ fib(n-2))      
    }
  }
  spawn{
    println("Thread 1:"+fibjoin.fib(20))
  }
  spawn{
    println("Thread 2:"+fibjoin.fib(25))
  }
}
/*
Thread 2:75025
Thread 1:6765
*/