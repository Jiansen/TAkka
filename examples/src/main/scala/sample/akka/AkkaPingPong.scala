package sample.akka

import akka.actor._

trait PingMsg
case object Pong extends PingMsg

trait PongMsg
case object Ping extends PongMsg
case object Stop extends PongMsg


object AkkaTimer{
  var start:Long = 0
  var end:Long = 0
}

class AkkaPing(count: Int, pong: ActorRef) extends Actor {
  var pingsLeft = count - 1  
  
  def receive = {
    case Pong =>
      if (pingsLeft % 100 == 0) {}
        //Console.println("Ping: pong")
      if (pingsLeft > 0) {
        pong ! (Ping, self)
        pingsLeft -= 1
      } else {
        Console.println("Ping: stop")
        pong ! (Stop, self)
        sys.exit()
      }
  }
  
}

class AkkaPong extends Actor {
  var pongCount = 0  
  
  def receive = {
    case (Ping, sender:ActorRef) =>
      //println("Hello "+pongCount)
      if (pongCount % 100 == 0) {}
        //Console.println("Pong: ping "+pongCount)
        sender ! Pong
        pongCount = pongCount + 1
    case (Stop, sender:ActorRef) =>
      Console.println("Pong: stop")
      AkkaTimer.end = java.util.Calendar.getInstance().getTime().getTime()
      Console.println(AkkaTimer.end - AkkaTimer.start)
      sys.exit()
  }
}

object AkkaPingPong extends App{
  val system = ActorSystem("MySystem")
  val pong = system.actorOf(Props(new AkkaPong()), "pong")
  val ping = system.actorOf(Props(new AkkaPing(100000, pong)), "ping")
  //println(pong)
  //println(ping)
//  val pong = new Pong
//  val ping = new Ping(100000, pong)
  
  AkkaTimer.start = java.util.Calendar.getInstance().getTime().getTime()
  pong ! (Ping, ping)
  //pong ! (Ping, ping)
}