package takka.clusterwrapper

import akka.actor.{ActorSystem, Address}
import akka.cluster.ClusterSettings

import akka.cluster.FailureDetector
import akka.cluster.ClusterEvent.ClusterDomainEvent

import takka.actor.{ActorSystem, ActorRef}

class Cluster(system: takka.actor.ActorSystem) {
  
  val akkacluster = akka.cluster.Cluster(system.system)
  
  def down(address: Address): Unit = {
    akkacluster.down(address)
  }

  val failureDetector: FailureDetector = {
    akkacluster.failureDetector
  }
  
  def isTerminated: Boolean = {akkacluster.isTerminated}  
  def join(address: Address): Unit = {akkacluster.join(address)}
  def leave(address: Address): Unit = {akkacluster.leave(address)}
  def publishCurrentClusterState(): Unit = {akkacluster.publishCurrentClusterState}

  def registerOnMemberUp(callback: Runnable): Unit = {
    akkacluster.registerOnMemberUp(callback)
  }
   
  def registerOnMemberUp[T](code: ⇒ T): Unit = {
    akkacluster.registerOnMemberUp(code)
  }
  
  val selfAddress: Address = {akkacluster.selfAddress}

  def sendCurrentClusterState(receiver: ActorRef[ClusterDomainEvent]): Unit = {
    akkacluster.sendCurrentClusterState(receiver.untypedRef)
  }

  val settings: ClusterSettings = akkacluster.settings

  def subscribe(subscriber: ActorRef[ClusterDomainEvent], to: Class[_]): Unit = {
    akkacluster.subscribe(subscriber.untypedRef, to)
  }

  def unsubscribe(subscriber: ActorRef[ClusterDomainEvent], to: Class[_]): Unit = {
    akkacluster.unsubscribe(subscriber.untypedRef, to)
  }

  def unsubscribe(subscriber: ActorRef[ClusterDomainEvent]): Unit = {
    akkacluster.unsubscribe(subscriber.untypedRef)
  }
}