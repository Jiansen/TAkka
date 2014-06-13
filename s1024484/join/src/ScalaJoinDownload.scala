//package benchmark.scala

import join._

object Download extends Join {
  object Payload extends AsyName[String]
  object Stop extends AsyName[Unit]
  join{
    case Payload(payload) => {
      if(payload == "Requested 1") {Manager.decrementLatch}      
      val newPayload = payload.replaceFirst("Requested ", "Downloaded ")
      Index.Payload(newPayload)
    }
  }
}

object Index extends Join {
  object Payload extends AsyName[String]
  object Stop extends AsyName[Unit]
  join {
    case Payload(payload) => {
      if(payload == "Downloaded 1") {Manager.decrementLatch}
      val newPayload = payload.replaceFirst("Downloaded ", "Indexed ")
      Write.Payload(newPayload)
    }
  }
}

object Write extends Join {
  object Payload extends AsyName[String]
  object Stop extends AsyName[Unit]
  join {
    case Payload(payload) => {
      if(payload == "Indexed 1") {Manager.decrementLatch}
      payload.replaceFirst("Indexed ", "Wrote ")
    }
  }
}

object Manager {
  val NR_REQUESTS = 900000
  var latch = 3
  var start = 0L
  
  def decrementLatch() = synchronized {
    latch -= 1
    if (latch == 0) { 
      val elapsed = System.currentTimeMillis - start
      println("elapsed = " + elapsed) 
    }
  }

  def main(args: Array[String]){
    start = System.currentTimeMillis
    var i = NR_REQUESTS
    while (i >= 1){
      val payload = "Requested " + i
      Download.Payload(payload)
      i-=1
    }
  }
}

/*
NR_REQUESTS      Time
10000             350
100000            690
200000           1033
300000           1278
400000           1718
500000           2068
600000           2472
700000           2740
800000           3151
900000           3591
1000000          3724
2000000          6398
10000000          -
 */