package sample.barbershop.akka

import collection.immutable.Queue
import akka.actor.{Actor, ActorRef}
import akka.event.Logging // new import: for logging

case class Line(maxLine: Int) extends Actor with PostStart {

  def receive = lineReceive(Queue[ActorRef]())
  val log = Logging(context.system, this) //new val: for logging

  def lineReceive(queue: Queue[ActorRef]): Receive = {
    case RequestBarber => {
      val customer = sender // self.sender.get
      if (queue.isEmpty) {
        log.debug("No one in the line try the lounge %s", customer)//.id)
        lounge ! RequestBarber(customer)
      } else {
        queueCustomer(customer, queue)
      }
    }

    case Wait(customer) => queueCustomer(customer, queue)

    case NextCustomer => if (queue.isEmpty) {
      log.debug("no customers standing", queue.size)
    } else {
      val (customer, tail) = queue.dequeue
      // become(lineReceive(tail))
      context.become(lineReceive(tail))
      log.debug("told %s to go to the lounge", customer)//.getId)
      lounge ! RequestBarber(customer)
      tracker ! TrackLeftLine
    }
  }

  def queueCustomer(customer: ActorRef, queue: Queue[ActorRef]): Unit = {
    if (queue.size < maxLine) {
      val newQueue = queue enqueue customer
      // log.debug("%s wait in line (%d)".format(customer.id, newQueue.size))
      log.debug("%s wait in line (%d)".format(customer, newQueue.size))
      customer ! WaitInLine
      // become(lineReceive(newQueue))
      context.become(lineReceive(newQueue))
    } else {
      // log.debug("%s sorry waitingline is full (%d)".format(customer.id, queue.size))
      log.debug("%s sorry waitingline is full (%d)".format(customer, queue.size))
      customer ! LineFull
    }
  }

}