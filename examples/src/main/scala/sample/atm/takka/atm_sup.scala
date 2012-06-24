package sample.atm.takka

import takka.actor._
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy._
import akka.util.duration._
import akka.dispatch._

class atm_sup_class(name:Symbol, port:Int) extends Actor[Unit]{
  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
    case _: Exception                => 
      Restart
  }
  
  def typedReceive = {
    case _ => 
  }
  
  override def preStart() = {
    // val strategy = OneForOneStrategy(List(classOf[Exception]), 5, 1000)
    // val system = ActorSystem("atm_sup")
  
//    typed_context.actorOf(Props().withCreator(atm.start_link(name)), "atm")
    typedContext.actorOf(Props[ATMEvent, ATM_Class], "atm")    
    typedContext.actorOf(Props(new webATM_Class(name, 8080)), "webATM")
  }
}

/*
object atm_sup_test extends App{
  backend.start_link()
  
  val system = ActorSystem("atm_sup")
  system.actorOf(Props(new atm_sup_class('default, 8080)))
  /*
  backend.start_link()
  
  val system = ActorSystem("atm_sup")
  val strategy = OneForOneStrategy(List(classOf[Exception]), 5, 1000)
  
  val superprops = Props[atm_sup_class].withFaultHandler(strategy)
  val supervisor = system.actorOf(superprops, "supervisor")
  supervisor ! Props().withCreator(atm.start_link('default))
  
  supervisor ! Props(new webATM_Class('default, 8080))
  */
}
*/