import scala.actors.Actor._
import scala.compat.Platform
import scala.actors.Scheduler

object CounterActorTest {
  val counter = new CounterActor

  def main(args: Array[String]) {
//    runTest(3000000)
    runTest(300000)
    Scheduler.shutdown
  }

  def runTest(msgCount: Int) {
    val start = System.currentTimeMillis
    val bytesPerMsg = 100

//    (1 to msgCount).par.foreach((x: Int) => counter ! new AddCount(bytesPerMsg))
    var n = 1
    while(n <= msgCount){
      counter ! AddCount(bytesPerMsg)  // Line Com
      n+=1
    }
    
    val finish = System.currentTimeMillis   // estimated overhead without Line Com
    val elapsedTime = (finish - start) / 1000.0
    val count = counter !? new GetAndReset
    
    printf("Count is %s%n",count)
    printf("Test took %s seconds%n", elapsedTime)
    printf("Throughput=%s per sec%n", msgCount / elapsedTime)
  }
}

/*
msgCount     Time(sec)  Throughput(msg/sec)
3000000       -         -
1000000       1.103     906618.3136899365
900000        0.74     1134930.6431273643
600000        0.683     878477.3060029282
300000        0.453     662251.655629139
100000        0.109     917431.1926605505
*/