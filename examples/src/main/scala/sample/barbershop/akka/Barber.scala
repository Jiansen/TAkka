package sample.barbershop.akka

import akka.actor.{ActorRef, Actor}
import akka.event.Logging // new import: for logging

case class Barber(name: String, cutTime: () => Long) extends Actor with PostStart {

  // no need for id, set id when initialise Barber
 // self.id = name

  val log = Logging(context.system, this) //new val: for logging
  override def postStart() = {
    log.debug("%s came to work", name)
    startSleeping
  }

  override def postStop = {
    log.debug("%s going home", name)
  }

  private def startSleeping = {
    log.debug("%s going to sleep", name)
    sign ! Sleeping
    tracker ! TrackSleeping
  }


  def receive = {

    case RequestBarber(customer) => cut(customer)
    case RequestBarber => // cut(self.sender.get)
      cut(sender)
  }

  def cut(customer: ActorRef): Any = {
    // log.debug("%s is cutting %s", name, customer.getId)
    log.debug("%s is cutting %s", name, customer)
    customer ! Cutting
    tracker ! TrackCutting(customer)
    val time = cutTime()
    Thread.sleep(time)
    // log.debug("%s cut %s in %d ms", name, customer.getId, time)
    log.debug("%s cut %s in %d ms", name, customer, time)
    customer ! CutDone
    tracker ! TrackCutDone
    startSleeping
  }

}

