import join._
import scala.concurrent.ops._

object SuccTest extends App{
  val n = 1000
  var list:List[Boolean] = List()
  var random = new scala.util.Random()
  
  object join extends Join{  
    object A extends AsyName[Unit]
    object Suc extends SynName[Int, Int]
    
    join {
     case A(_) and Suc(n) =>  Suc reply (n+1)
    }
    
  }

  var i = 0
  while(i<n){
    spawn{synchronized{
      val k = random.nextInt(100)
      join.A()
      val sk = join.Suc(k)
      list = (sk == k+1)::list
    }}
    i+=1
  }
  Thread.sleep(2000)
//  println(list)
  println(list.size)
  println(list.forall(b => b))
  
}