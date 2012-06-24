/*
 * adapted from
 * https://github.com/cyberzac/BarberShop
 * 
 * retrieved date: 23/04/2012
 * 
 * original akka version: <1.2
 * 
 * new akka version: 2.0
 * with type-parametrised actor library
 * 
 * adapted by Jiansen HE
 */

package sample.barbershop.takka

//import akka.actor.Actor._
import takka.actor._
import scala.math.random
import akka.event.Logging // new import: for logging

object AkkaHairs {

  val numberOfCustomers = 200
     val numberOfChairs = 10
     val maxLine = 10
     val maxCutTime = 150
     val minCutTime = 10
     val barberNames = List("Edward", "Jean-Paul", "Gulletussan")

  def main(args: Array[String]) {
    // add actor system
    val system = ActorSystem("TestSystem")
    // val log = Logging(system, this) //new val: for logging
  
    /*
    val sign = actorOf[Sign]
    val tracker = actorOf(new Tracker(numberOfCustomers, numberOfChairs, maxLine))
    val lounge = actorOf(new Lounge(numberOfChairs))
    val line = actorOf(new Line(maxLine))
    val barbers = barberNames map {name => actorOf(new Barber(name, cutTime _))}
    val shop = Barbershop(sign = sign, lounge = lounge, line = line, tracker = tracker, barbers = barbers)
    shop start
    */
    val sign = system.actorOf(Props[SignMessages, Sign], "sign")
    val tracker = system.actorOf(Props[TrackerMessages](new Tracker(numberOfCustomers, numberOfChairs, maxLine)), "tracker")
    val lounge = system.actorOf(Props(new Lounge(numberOfChairs)), "Lounge")
    val line = system.actorOf(Props(new Line(maxLine)), "line")
    val barbers = barberNames map {name => system.actorOf(Props(new Barber(name, cutTime _)), name)}
    val shop = Barbershop(sign = sign, lounge = lounge, line = line, tracker = tracker, barbers = barbers)
    shop start
    
    1.to(numberOfCustomers) foreach {
      id =>
        val wait: Long = poisson((maxCutTime-minCutTime)/(3*2))
        // log.debug("Waiting %d", wait)
        // log.debugFormat("Waiting " + wait)
        println("Waiting " + wait)
        Thread.sleep(wait)
        system.actorOf(Props(new Customer(id.toString, shop)), id.toString())//.start
    }
    Thread.sleep(1000)


    // def delay = poisson(100)

    def uniform: Long = {
      val max = 100
      val min = 0
      min + (random * (max - min)).intValue
    }
  }

  def poisson(lamda: Int): Int = {
    val L = math.exp(-lamda)
    var k = 0
    var p = 1D
    while (p > L) {
      k += 1
      p *= random
    }
    k - 1
  }

  def cutTime: Long = {
    minCutTime + (random * (maxCutTime - minCutTime)).intValue
  }

}
