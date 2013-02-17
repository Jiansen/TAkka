package sample.takka

import takka.actor._

trait PingMsg
case object Pong extends PingMsg

trait PongMsg
case object Ping extends PongMsg
case object Stop extends PongMsg

object MyTimer{
  var start:Long = 0
  var end:Long = 0
}

class Ping(count: Int, pong: ActorRef[(PongMsg, ActorRef[PingMsg])]) extends TypedActor[PingMsg] {
  var pingsLeft = count - 1  
  
  def typedReceive = {
    case Pong =>
//      if (pingsLeft % 100 == 0) {}
        // Console.println("Ping: pong")
      if (pingsLeft > 0) {
        pong ! (Ping, typedSelf)
        pingsLeft -= 1
      } else {
        Console.println("Ping: stop")
        pong ! ((Stop, typedSelf))
        sys.exit()
      }
  }
  
}

class Pong extends TypedActor[(PongMsg, ActorRef[PingMsg])] {
  
  def typedReceive = {
    case (Ping, sender) =>
        sender ! (Pong)
    case (Stop, sender) =>
      MyTimer.end = java.util.Calendar.getInstance().getTime().getTime()      
      println("Pong: stop")
      println(MyTimer.end - MyTimer.start)
      sys.exit()
  }
}

object PingPong extends App{
  val system = ActorSystem("MySystem")
  val pong = system.actorOf[(PongMsg, ActorRef[PingMsg])](Props(new Pong()), "pong")
  val ping = system.actorOf[PingMsg](Props(new Ping(100000, pong)), "ping")

  MyTimer.start = java.util.Calendar.getInstance().getTime().getTime()
  pong ! (Ping, ping)
}


/*
 * Benchmark
 * 
 * Machine: DELL Laptop
 * svn version 106
 * # of ping-pong = 100000 = 10^5
 * average time (ms): 851.5
 * 1.59x times as Akka version (535.625) 
 * records: 822, 862, 854, 701, 672, 952, 814, 1037, 770, 1055
 * 
 *  * # of ping-pong = 1000000 = 10^6
 * average time (ms): 2647.875
 * 1.55x times as Akka version (1703.625) 
 * records: 3068, 2655, 2577, 2763, 2532, 2523, 3118, 2612, 2453, 2361
 * 
 * svn version 110
 * # of ping-pong = 100000 = 10^5
 * average time (ms): 854.5
 * 1.595x times as Akka version (535.625) 
 * records: 556, 937, 984, 804, 654, 928, 855, 950, 873, 835
 * 
 *  * # of ping-pong = 1000000 = 10^6
 * average time (ms): 2568.125
 * 1.51x times as Akka version (1703.625) 
 * records: 2465, 2482, 2597, 3055, 2579, 2636, 2646, 2673, 2467, 2354
 * 
 */