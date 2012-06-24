package sample.atm.akka

import akka.actor._
import akka.dispatch._

class ebank_sup (name:Symbol, port:Int) extends Actor{
  def receive = {
    case p: Props => sender ! context.actorOf(p)
  }
  
  override def preStart() = {
    // val strategy = OneForOneStrategy(List(classOf[Exception]), 5, 1000)
    //val system = ActorSystem("atm_sup")
  
    context.actorOf(Props[backend])
    context.actorOf(Props(new atm_sup_class('default, 8080)))
  }
}