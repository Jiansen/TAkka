package sample.atm.takka

import takka.actor._
import akka.util.{Timeout}
import akka.dispatch.Future

object Patterns {
 def ask[M] (actor: ActorRef[M], message: M, timeout: Timeout): Future[AnyRef] = {
   akka.pattern.Patterns.ask(actor.untypedRef, message, timeout)
 }
 
 
}