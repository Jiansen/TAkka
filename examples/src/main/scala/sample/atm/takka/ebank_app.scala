package sample.atm.takka

import takka.actor._

object ebank_app extends App{
  val system = ActorSystem("atm_sup")
  
  system.actorOf(Props(new ebank_sup('default, 8080)), "ebank_sup")
}