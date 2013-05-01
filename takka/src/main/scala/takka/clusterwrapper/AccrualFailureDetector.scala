package takka.clusterwrapper

import takka.actor.{ActorSystem}
import akka.cluster.ClusterSettings
import akka.actor.Address

import akka.event.Logging
import scala.collection.immutable.Map
import scala.annotation.tailrec
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.TimeUnit.NANOSECONDS
import scala.concurrent.duration._

object AccrualFailureDetector {
  private def realClock: () ⇒ Long = () ⇒ NANOSECONDS.toMillis(System.nanoTime)
}

class AccrualFailureDetector(system: ActorSystem) {
  private var afd:akka.cluster.AccrualFailureDetector = _
  def this(system: ActorSystem, settings: ClusterSettings) = {
    this(system)
    afd = new akka.cluster.AccrualFailureDetector(system.system, settings)
  }
  def this(system: ActorSystem, threshold: Double, maxSampleSize: Int, minStdDeviation: Duration, acceptableHeartbeatPause: Duration, firstHeartbeatEstimate: Duration, clock: () ⇒ Long = AccrualFailureDetector.realClock) = {
    this(system)
    afd = new akka.cluster.AccrualFailureDetector(system.system, threshold, maxSampleSize, minStdDeviation, acceptableHeartbeatPause, firstHeartbeatEstimate, clock)
  }
  
  val acceptableHeartbeatPause: Duration = afd.acceptableHeartbeatPause
  val clock: () ⇒ Long = afd.clock
  val firstHeartbeatEstimate: Duration = afd.firstHeartbeatEstimate

  final def heartbeat(connection: Address): Unit = {
    afd.heartbeat(connection)
  }

  def isAvailable(connection: Address): Boolean = {
    afd.isAvailable(connection)
  }

  def isMonitoring(connection: Address): Boolean = {
    afd.isMonitoring(connection)
  }

  val maxSampleSize: Int = afd.maxSampleSize
  val minStdDeviation: Duration = afd.minStdDeviation

  def phi(connection: Address): Double = {    afd.phi(connection)  }

  final def remove(connection: Address): Unit = {
    afd.remove(connection)
  }

  def reset(): Unit = afd.reset

  val threshold: Double = afd.threshold
}