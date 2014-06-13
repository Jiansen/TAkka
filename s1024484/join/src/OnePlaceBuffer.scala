import join._
import scala.concurrent.ops._

object OnePlaceBufferTest extends App{
  object OnePlaceBuffer extends Join{
    private object Empty extends AsyName[Unit]
    private object Contains extends AsyName[String]
    object Put extends SynName[String, Unit] 
    object Get extends SynName[Unit, String] 
    join {
      case Put(s) and Empty(_) => {Contains(s); Put reply Unit}
      case Get(_) and Contains(s) => {Empty(); Get reply s}
    }
    def init() {
      Empty(Unit)      
    }
  }
  
  def Producer(n:Int) {
    Console.println("Producer Started");
    var i:Int = 0
    while (i < n) {
      OnePlaceBuffer.Put(i.toString());
//      Console.println("Produced!"+ i);
        //Thread.Sleep(10);
      i+=1
    }
  }
  
  def Consumer(n:Int) {
    Console.println("Consumer Started");
    var i:Int = 0
    while (i < n) {
      val s = OnePlaceBuffer.Get()
//      Console.println("Consumed?"+ s);
        //Thread.Sleep(10);
      i+=1
    }
    Console.println("Done Consuming");
  }

  OnePlaceBuffer.init()
  spawn{Producer(10000)}
  spawn{Consumer(10000)}
  
}
