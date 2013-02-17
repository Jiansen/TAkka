package sample.barbershop.takka

import takka.actor.{ActorRef, TypedActor}
import akka.event.Logging // new import: for logging

case class Barber(name: String, cutTime: () => Long) extends TypedActor[BarberMessages] with PostStart {

  // no need for id, set id when initialise Barber
 // self.id = name

  val log = Logging(context.system, this) //new val: for logging
  override def postStart() = {
    log.debug("{} came to work", name)
    startSleeping
  }

  override def postStop = {
    log.debug("{} going home", name)
  }

  private def startSleeping = {
    log.debug("{} going to sleep", name)
    sign ! Sleeping(typedSelf)
    tracker ! TrackSleeping(typedSelf)
  }


  protected def typedReceive = {

    case RequestBarber(customer) => cut(customer)
    /*
    case RequestBarber => // cut(self.sender.get)
      cut(sender)
      */
    //TODO: why duplicate messages?
  }

  def cut(customer: ActorRef[CustomerMessage]): Any = {
    // log.debug("%s is cutting %s", name, customer.getId)
    log.debug("{} is cutting {}", name, customer)
    customer ! Cutting
    tracker ! TrackCutting(typedSelf, customer)
    val time = cutTime()
    Thread.sleep(time)
    // log.debug("%s cut %s in %d ms", name, customer.getId, time)
    log.debug("{} cut {} in {} ms", name, customer, time)
    customer ! CutDone
    tracker ! TrackCutDone(typedSelf)
    startSleeping
  }

}

