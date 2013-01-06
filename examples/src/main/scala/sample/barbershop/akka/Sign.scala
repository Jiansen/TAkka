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


package sample.barbershop.akka

import akka.actor.{ActorRef, Actor}
import collection.immutable.Queue
import akka.event.Logging // new import: for logging

class Sign extends Actor with PostStart {
  val log = Logging(context.system, this) //new val: for logging

  def receive = signReceive(Queue[ActorRef]())

  def signReceive(sleeping: Queue[ActorRef]): Receive = {

    case Sleeping => {
//      val newSleeping = sleeping enqueue self.sender.get
      val newSleeping = sleeping enqueue sender      
      //log.debug("%s going to sleep, calling for next customer, (sleeping: %s)", self.sender.get.id, barberNames(newSleeping))
      log.debug("%s going to sleep, calling for next customer, (sleeping: %s)", sender, barberNames(newSleeping))      
      lounge ! NextCustomer
      context.become(signReceive(newSleeping)) // new API.  become(...)  -> context.become
    }
    case rb@RequestBarber(customer) => {
      if (sleeping.isEmpty) {
        log.debug("%s is told to wait", customer) // customer.id -> customer
//        self.reply(Wait(customer))
        sender ! Wait(customer)
      } else {
        val (barber: ActorRef, tail) = sleeping.dequeue
        context.become(signReceive(tail)) // become(...)  -> context.become
        log.debug("sending %s to  %s (sleeping %s)", customer, barber, barberNames(tail))
        barber ! rb
      }
    }
//    case _ => "\n\n\n\n\n\nHAHA"; Thread.sleep(1000)
  }

  def barberNames(sleeping: Queue[ActorRef]): String = {
    if (sleeping.isEmpty) {
      "no barbers"
    } else {
      sleeping map (_.toString) reduceLeft (_ + ", " + _) // getId -> toString
    }
  }

}