import scala.compat.Platform

object CounterJoinTest {

  def main(args: Array[String]) {
//    runTest(3000000)
    runTest(90000000)    
  }

  def runTest(msgCount: Int) {
    val start = System.currentTimeMillis
//    val count = theTest(msgCount)
    
    val bytesPerMsg = 100
//    val updates = (1 to msgCount).par.foreach((x: Int) => CounterJoin.AddCount(bytesPerMsg))
    var n = 1
    while(n <= msgCount){
      CounterJoin.AddCount(bytesPerMsg)  // Line Com
      n+=1
    }
    
    
    val finish = System.currentTimeMillis   // estimated overhead without Line Com (mainly due to Line Syn)
    val elapsedTime = (finish - start) / 1000.0
    
    val count = CounterJoin.GetAndReset(())  // Line Syn : Too expensive, don't time this line.

    printf("Count is %s%n",count)
    printf("Test took %s seconds%n", elapsedTime)
    printf("Throughput=%s per sec%n", msgCount / elapsedTime)
  }
}

/*
msgCount   Time(sec)  Throughput(msg/sec)
300000000  14.821     2.024154915322853E7
90000000    5.098     1.7653981953707337E7
60000000    3.34      1.7964071856287427E7
30000000    1.593     1.8832391713747647E7
20000000    1.206     1.658374792703151E7
10000000    0.701     1.4265335235378033E7
*/