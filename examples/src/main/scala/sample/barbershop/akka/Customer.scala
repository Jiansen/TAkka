package sample.barbershop.akka

import akka.actor.Actor
import akka.event.Logging // new import: for logging

case class Customer(id: String, barbershop: Barbershop) extends Actor {
  // no need for id, set id when initialise Customer
  // self.id = id

  val log = Logging(context.system, this) //new val: for logging
  override def preStart = {
    log.debug("%s entered shop", id)
    barbershop.line ! RequestBarber
  }

  protected def receive = customerReceive(CustomerStats())

  def customerReceive(stats: CustomerStats): Receive = {

    case Cutting =>
      //log.debug("%s is being cut by %s", id, self.sender.get.id)
      //become(customerReceive(stats.cut))
      log.debug("%s is being cut by %s", id, sender)
      context.become(customerReceive(stats.cut))

    case CutDone => {
      log.debug("%s leaving, %s", id, stats.done)
      barbershop.tracker ! TrackLeaving(Some(stats.done))
    }

    case WaitInLine => {
      log.debug("%s waiting in line", id)
      barbershop.tracker ! TrackEnteredLine
      //become(customerReceive(stats.stand))
      context.become(customerReceive(stats.stand))
    }

    case LineFull => {
      log.debug("%s no place to stand, I'm leaving", id)
      barbershop.tracker ! TrackLeaving(None)
    }
    case TakeChair(chair) => {
      log.debug("%s sitting down in chair %d", id, chair)
      barbershop.tracker ! TrackSat(chair)
      // become(customerReceive(stats.sit(chair)))
      context.become(customerReceive(stats.sit(chair)))
    }
    
    case STOPCustomer => // stopped by Tracker
      log.debug("customer %d stoped by tracker", id)      
      context.stop(self)

  }
}

object CustomerStats {
  def apply() = new CustomerStats(-1, 0L, 0L, 0L, 0L)

  def apply(standAt: Long, satAt: Long, cutAt: Long, doneAt: Long) = new CustomerStats(-1, standAt, satAt, cutAt, doneAt)
}

case class CustomerStats(chair: Int, standAt: Long, satAt: Long, cutAt: Long, doneAt: Long) {
  def stand: CustomerStats = copy(standAt = System.currentTimeMillis)

  def sit(c: Int): CustomerStats = copy(chair = c, satAt = System.currentTimeMillis)

  def cut: CustomerStats = copy(cutAt = System.currentTimeMillis)

  def done: CustomerStats = copy(doneAt = System.currentTimeMillis)

  val timeStanding = if (standAt == 0) 0 else (if (satAt == 0) cutAt else satAt) - standAt
  val timeSitting = if (satAt == 0) 0 else cutAt - satAt
  val timeCut = if (cutAt == 0) 0 else doneAt - cutAt
  val timeTotal = timeStanding + timeSitting + timeCut

  override def toString = "standing=%d sitting=%d, cutting=%d, total=%d".format(timeStanding, timeSitting, timeCut, timeTotal)
}




