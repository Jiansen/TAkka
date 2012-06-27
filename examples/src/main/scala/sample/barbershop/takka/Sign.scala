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

class Sign extends Actor[SignMessages] with PostStart {
  val log = Logging(context.system, this) //new val: for logging

  protected def typedReceive = signReceive(Queue[ActorRef[BarberMessages]]()) //type clearer

  def signReceive(sleeping: Queue[ActorRef[BarberMessages]]): Receive = {

    case Sleeping(barber) => {
//      val newSleeping = sleeping enqueue self.sender.get
      val newSleeping = sleeping enqueue barber      
      //log.debug("%s going to sleep, calling for next customer, (sleeping: %s)", self.sender.get.id, barberNames(newSleeping))
      log.debug("{} going to sleep, calling for next customer, (sleeping: {})", barber, barberNames(newSleeping))      
      lounge ! NextCustomer
      context.become(signReceive(newSleeping)) // new API.  become(...)  -> context.become
    }
    case rb@RequestBarber(customer) => {
      if (sleeping.isEmpty) {
        log.debug("{} is told to wait", customer) // customer.id -> customer
//        self.reply(Wait(customer))
        sender ! Wait(customer)
      } else {
        val (barber, tail) = sleeping.dequeue
        context.become(signReceive(tail)) // become(...)  -> context.become
        log.debug("sending {} to  {} (sleeping {})", customer, barber, barberNames(tail))
        barber ! rb
      }
    }
  }

  def barberNames(sleeping: Queue[ActorRef[BarberMessages]]): String = {
    if (sleeping.isEmpty) {
      "no barbers"
    } else {
      sleeping map (_.toString) reduceLeft (_ + ", " + _) // getId -> toString
    }
  }

}