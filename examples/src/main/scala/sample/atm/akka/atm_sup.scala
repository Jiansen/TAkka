package sample.atm.akka

import akka.actor._
import akka.dispatch._

class atm_sup_class(name:Symbol, port:Int) extends Actor{
  def receive = {
    case p: Props => sender ! context.actorOf(p)
  }
  
  override def preStart() = {
    // val strategy = OneForOneStrategy(List(classOf[Exception]), 5, 1000)
    // val system = ActorSystem("atm_sup")
  
    context.actorOf(Props().withCreator(atm.start_link(name)))
    context.actorOf(Props(new webATM_Class(name, 8080)))
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