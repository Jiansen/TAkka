package sample.barbershop.akka

import collection.immutable.Queue
import akka.actor.{ActorRef, Actor}
import akka.event.Logging // new import: for logging

case class Lounge(count: Int) extends Actor with PostStart {

  // no need for id, set id when initialise Lounge
//  self.id = "Lounge"
  val log = Logging(context.system, this) //new val: for logging
 
  def receive = chairsReceive(LoungeState(Queue[OccupiedChair](), Range(0, count).foldLeft(List[Int]())(_ :+ _)))

  def chairsReceive(state: LoungeState): Receive = {

    case requestBarber@RequestBarber(customer) => {
      if (state.isAllChairsFree) {
        log.debug("All chairs are empty, try the sign for a barber %s", customer)//customer.id)
        sign ! requestBarber
      } else {
        queueCustomer(state, customer)
      }
    }

    case Wait(customer) => {
      queueCustomer(state, customer)
    }

    case NextCustomer => {
      if (state.isAllChairsFree) {
        log.debug("All chairs are empty")
      } else {
        val (OccupiedChair(customer, chair), tail) = state.occupied.dequeue
//        log.debug("sending %s to %s", customer.id, self.sender.get.id)
        log.debug("sending %s to %s", customer, sender)
        sign ! RequestBarber(customer)
        tracker ! TrackLeftChair(chair)
        //become(chairsReceive(LoungeState(tail, chair :: state.free)))
        context.become(chairsReceive(LoungeState(tail, chair :: state.free)))
      }
    }
    log.debug("offer a chair to customers standing in line")
    line ! NextCustomer
  }

  def queueCustomer(state: LoungeState, customer: ActorRef) = {
    if (state.free.isEmpty) {
      log.debug("All chairs are taken, try the line %s", customer)//.id)
      line ! Wait(customer)
    } else {
      val (chair :: free) = state.free
      val occupied = state.occupied enqueue OccupiedChair(customer, chair)
//      log.debug("told %s to sit down in chair %d", customer.id, chair)
      log.debug("told %s to sit down in chair %d", customer, chair)      
      customer ! TakeChair(chair)
      //become(chairsReceive(LoungeState(occupied, free)))
      context.become(chairsReceive(LoungeState(occupied, free)))
    }
  }

}


case class OccupiedChair(customer: ActorRef, chair: Int)

case class LoungeState(occupied: Queue[OccupiedChair], free: List[Int]) {
  def hasFreeChairs = !free.isEmpty

  def isAllChairsFree = occupied.isEmpty

  def chairs: Map[Int, Option[ActorRef]] = {
    val start = Map[Int, Option[ActorRef]]()
    (occupied.foldLeft(start)((m, oc) => m + (oc.chair -> Some(oc.customer))) ++
      free.foldLeft(start)((m, i) => m + (i -> None)))
  }
}