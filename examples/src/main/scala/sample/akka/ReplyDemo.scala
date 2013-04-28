package sample.akka

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
      val actor = context.actorOf(Props[DoubleEcho], "double")
      (actor ? m) onSuccess{
        case x => 
          println("sender: "+sender)        
          println("double: "+actor)
          println("replyTo: "+replyTo)
          sender ! x
      }
  }
}

class DoubleEcho extends Actor{
  def receive = {
    case m => {
      println("sending from "+self+" to "+sender)
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
sending from Actor[akka://mysystem/user/echo/double] to Actor[akka://mysystem/temp/$a]
sender: Actor[akka://mysystem/deadLetters]
double: Actor[akka://mysystem/user/echo/double]
replyTo: Actor[akka://mysystem/temp/$b]
akka.pattern.AskTimeoutException: Timed out
*/