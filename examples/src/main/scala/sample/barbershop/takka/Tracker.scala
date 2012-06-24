/*
 * adapted from
 * https://github.com/cyberzac/BarberShop
 * 
 * retrieved date: 23/04/2012
 * 
 * original akka version: <1.2
 * 
 * new akka version: 2.0
 * 
 * adapted by Jiansen HE
 */
package sample.barbershop.takka

import takka.actor.{ActorRef, Actor}
import collection.immutable.Queue
import akka.event.Logging // new import: for logging

case class Tracker(totalCustomers: Int, numberOfChairs: Int, maxLine: Int) extends Actor[TrackerMessages] with PostStart {
  protected def typedReceive = trackerReceive(TrackerState(numberOfChairs, maxLine))

  val log = Logging(context.system, this) //new val: for logging
  
  def trackerReceive(state: TrackerState) : PartialFunction[TrackerMessages, Unit] = {
    case TrackLeaving(customerStats) => {
      val customerRef = sender // self.sender.get -> sender
      //customerRef.stop // deprecated method
      customerRef ! STOPCustomer
      val customerId = customerRef.toString//.getId
      val nextState = state.customerLeft(customerStats)
      become(nextState)
      if (nextState.stats.total == totalCustomers) {
        self ! CloseShop
      }
    }
    case TrackSleeping(barber) => become(state.sleeping(BarberRef(barber)))
    case TrackCutting(barber, customer) => become(state.cutting(BarberRef(barber), CustomerRef(customer)))
    case TrackCutDone(barber) => become(state.cutDone(BarberRef(barber)))
    case TrackSat(customer, chair) => become(state.sat(CustomerRef(customer), chair))
    case TrackLeftChair(chair) => become(state.leftChair(chair))
    case TrackEnteredLine(customer) => become(state.enterLine(CustomerRef(customer)))
    case TrackLeftLine => become(state.leftLine)
    case CloseShop => {
      log.info("Last customer left, closing shop")
      // Actor.registry.shutdownAll
      context.system.shutdown()
    }
  }

  // def sender = self.sender.get

  def become(state: TrackerState) = {
    log.info("{}", state)
    // super.become(trackerReceive(state))
    context.become({
      case m:TrackerMessages => trackerReceive(state)(m)
    })
  }

}

case class BarberRef(ref: ActorRef[BarberMessages])

case class CustomerRef(ref: ActorRef[CustomerMessage]) {
  // val id = ref.getId
  val id = ref.toString()
}

case class Chair(id: Int)

object Statistics {
  def apply() = new Statistics(0, 0, TimeSum(0, 0, 0))
}

object TimeSum {
  def apply() = new TimeSum(0,0,0)
  def apply(customerStats:CustomerStats) =  new TimeSum(customerStats.timeCut, customerStats.timeSitting, customerStats.timeStanding)
}

case class TimeSum(cut:Long, sit:Long, stand:Long) {

  def +(time:TimeSum):TimeSum = {
    copy(cut = cut+time.cut, sit = sit + time.sit, stand = stand + time.stand)
  }
  def /(n:Long):TimeSum = copy(cut=cut/n, sit=sit/n, stand=stand/n)
}

case class Statistics(total: Int, rejected: Int, times: TimeSum) {
  def meanTime: TimeSum = {
    if (total == 0) {
      TimeSum(0, 0, 0)
    } else {
      times / total
    }
  }

    def addStatistics(customerStats: Option[CustomerStats]):Statistics = {
      val cs = customerStats.getOrElse(return   copy(total = total + 1, rejected = rejected + 1))
      copy(total = total + 1, times = times + TimeSum(cs) )
    }

}

object TrackerState {
  def apply(chairs: Int, maxLine: Int) = new TrackerState(maxLine, Map[BarberRef, String](), Vector.fill(chairs)("."), Queue[String](), Statistics())
}

case class TrackerState(maxLine: Int, barbers: Map[BarberRef, String], chairs: Vector[String], line: Queue[String], stats: Statistics) {

  def sleeping(barber: BarberRef): TrackerState = copy(barbers = (barbers.updated(barber, " z ")))

  def cutting(barber: BarberRef, customer: CustomerRef): TrackerState = copy(barbers = barbers.updated(barber, customer.id))

  def cutDone(barber: BarberRef): TrackerState = copy(barbers = barbers.updated(barber, "."))

  def sat(customer: CustomerRef, chair: Int): TrackerState = copy(chairs = chairs.updated(chair, customer.id))

  def leftChair(chair: Int): TrackerState = copy(chairs = chairs.updated(chair, "."))

  def enterLine(customer: CustomerRef): TrackerState = copy(line = (line.enqueue(customer.id)))

  def leftLine: TrackerState = {
    val (customer, newLine) = line.dequeue
    copy(line = newLine)
  }

  def customerLeft(customerStats: Option[CustomerStats]): TrackerState =  copy(stats = stats.addStatistics(customerStats))

  def foldFormat(it: Iterable[String]): String = {
    it.foldLeft("")(_ + "%3.3s ".format(_))
  }

  override def toString = {

    val mean = stats.meanTime

    "|" + foldFormat(barbers.values) +
      "|" + foldFormat(chairs) +
      "|" + foldFormat(line ++ Vector.fill(maxLine - line.size)(".")) +
      "|%3d|%3d".format(stats.total, stats.rejected) +
      " |%3d|%3d|%3d|".format(mean.stand, mean.sit, mean.cut)
  }


}