package sample.barbershop.akka

import akka.actor.ActorRef

/**
 * Messages to the tracker
 */
sealed trait TrackerMessages

/**
 * Messages to Customers
 */
sealed trait CustomerMessage

/**
 * Messages to the Line
 */
sealed trait LineMessage

/**
 * Messages to the Lounge
 */

sealed trait ChairsMessages

/**
 * Messages to the sign
 */
sealed trait SignMessages

/**
 * Messages to the barbers
 */
sealed trait BarberMessages

/**
 * Barber is sleeping
 */
case object TrackSleeping extends TrackerMessages

/**
 * Barber is cutting
 */
case class TrackCutting(customer: ActorRef) extends TrackerMessages

/**
 * Barber completed cutting
 */
case object TrackCutDone extends TrackerMessages

/**
 * Customer is leaving
 */
case class TrackLeaving(stats: Option[CustomerStats]) extends TrackerMessages

/**
 * Customer is sitting down
 */
case class TrackSat(chair: Int) extends TrackerMessages

/**
 * Customer left a chair
 */
case class TrackLeftChair(chair: Int) extends TrackerMessages

/**
 * Customer is getting in line
 */
case object TrackEnteredLine extends TrackerMessages

/**
 * Customer left line
 */
case object TrackLeftLine extends TrackerMessages

/**
 * Closing the shop
 */
case object CloseShop extends TrackerMessages

/**
 * Barber is sleeping
 * @param time, when started sleeping
 */
case class Sleeping(barber:ActorRef) extends SignMessages

/**
 * Request for a barber from a customer
 */
case object RequestBarber extends LineMessage


/**
 * Request for a barber, from  Line or Chair
 * @param customer, the original requester
 */
case class RequestBarber(customer: ActorRef) extends ChairsMessages with SignMessages with BarberMessages

/**
 * Barber wants the next waiting customer
 */
case object NextCustomer extends LineMessage with ChairsMessages

/**
 * Unable to serve, please wait
 */
case class Wait(customer: ActorRef) extends LineMessage with ChairsMessages

/**
 * Barber begins to cut
 */
case object Cutting extends CustomerMessage

/**
 * Hair cut is done
 */
case object CutDone extends CustomerMessage

/**
 * Take a seat in the waiting room
 */
case class TakeChair(chair: Int) extends CustomerMessage

/**
 * Customer is allowed into the waiting line
 */
case object WaitInLine extends CustomerMessage

/**
 *   Waiting line is full
 */
case object LineFull extends CustomerMessage


/**
 *   Stop the Customer Actor
 *   Sent by Tracker
 */
case object STOPCustomer extends CustomerMessage

