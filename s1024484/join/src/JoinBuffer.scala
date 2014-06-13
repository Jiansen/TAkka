import join._
import scala.concurrent.ops._

object JoinBuffer {
  object Buffer extends Join{
    object Put extends AsyName[String] 
    object Get extends SynName[Unit, String] 
    join {
      case Put(s) and Get(_) => Get reply s
    }
  }
  
  def Producer(n:Int) {
    Console.println("Producer Started");
    var i:Int = 0
    while (i < n) {
      Buffer.Put(i.toString());
      Console.println("Produced!"+ i);
        //Thread.Sleep(10);
      i+=1
    }
  }
  
  def Consumer(n:Int) {
    Console.println("Consumer Started");
    var i:Int = 0
    while (i < n) {
      val s = Buffer.Get()
      Console.println("Consumed?"+ s);
        //Thread.Sleep(10);
      i+=1
    }
    Console.println("Done Consuming");
  }
  
  def main(args : Array[String]) : Unit = {
    spawn{Producer(10000)}
    spawn{Consumer(10000)}
  }
}
