package sample.atm.takka

import takka.actor._
import akka.dispatch._

class ebank_sup (name:Symbol, port:Int) extends Actor[Null]{
  def typedReceive = {
    case _ => 
  }
  
  override def preStart() = {
    // val strategy = OneForOneStrategy(List(classOf[Exception]), 5, 1000)
    //val system = ActorSystem("atm_sup")
  
    typedContext.actorOf(Props[BackendMsg, backend], "backend")
    typedContext.actorOf(Props(new atm_sup_class('default, 8080)), "atm_sup")
  }
}