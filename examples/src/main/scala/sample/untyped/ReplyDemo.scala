package sample.untyped

import akka.actor._
import akka.pattern.ask
import scala.concurrent.duration._
import scala.util._
class Echo extends Actor{
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val timeout = akka.util.Timeout(2 second)  
  def receive = {
    case m =>
      val replyTo = sender
      val actor = context.actorOf(Props[DoubleEcho])
      (actor ? m) onSuccess{
        case x => 
          println("sender: "+sender)        
          println("replyTo: "+replyTo)
          sender ! x
      }
  }
}

class DoubleEcho extends Actor{
  def receive = {
    case m => {
      sender ! m+" "+m
    }
  }
}


object ReplyDemo extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val timeout = akka.util.Timeout(2 second)  
  val system = ActorSystem("mysystem")
  val actor = system.actorOf(Props[Echo],"echo")
  actor ? "Hello" onComplete {
    case Success(x) => println(x)
    case Failure(e) => println(e)
  }
}

/*
sender: Actor[akka://mysystem/deadLetters]
replyTo: Actor[akka://mysystem/temp/$a]
akka.pattern.AskTimeoutException: Timed out
*/