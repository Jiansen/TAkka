package sample.barbershop.takka

import takka.actor.{ActorRef, Actor}
import akka.event.Logging // new import: for logging

/**
 * Will set addresses for, line, sign and  chairs
 */
trait PostStart {
  actor: akka.actor.Actor =>

  var line: ActorRef[LineMessage] = null
  var sign: ActorRef[SignMessages] = null
  var lounge: ActorRef[ChairsMessages] = null
  var tracker: ActorRef[TrackerMessages] = null


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