package sample.barbershop.akka

import akka.actor.{ActorRef, Actor}
import akka.event.Logging // new import: for logging

/**
 * Will set addresses for, line, sign and  chairs
 */
trait PostStart {
  actor: Actor =>

  var line: ActorRef = null
  var sign: ActorRef = null
  var lounge: ActorRef = null
  var tracker: ActorRef = null


  /**
   * Called when all actors in the barbershop are started
   */
  private val log = Logging(context.system, this) //new val: for logging
  
  def postStart: Unit = {
    log.debug("Default postStarted")
  }

  override def preStart {
    // actor.become {
    context.become {
      case barbershop: Barbershop => try { {
        line = barbershop.line
        sign = barbershop.sign
        lounge = barbershop.lounge
        tracker = barbershop.tracker
        postStart
      }
      } finally //{unbecome}
      {context.unbecome}
    }
  }

}