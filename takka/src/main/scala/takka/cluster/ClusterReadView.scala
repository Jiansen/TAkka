///*
//   Copyright 2012 Jiansen HE
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.
//*/
///**
// * Copied and modified from akka.cluster by Typesafe Inc. <http://www.typesafe.com>
// */
//package takka.cluster
//
//import java.io.Closeable
//import scala.collection.immutable.SortedSet
////import akka.actor.{ Actor, ActorRef, ActorSystemImpl, Address, Props }
//import akka.actor.{ ActorSystemImpl, Address}
//import takka.actor.{ TypedActor, ActorRef, Props }
//import takka.cluster.ClusterEvent._
//import akka.actor.PoisonPill
//
///**
// * INTERNAL API
// *
// * Read view of cluster state, updated via subscription of
// * cluster events published on the event bus.
// */
//private[takka] class ClusterReadView(cluster: Cluster) extends Closeable {
//
//  /**
//   * Current state
//   */
//  @volatile
//  private var state: CurrentClusterState = CurrentClusterState()
//
//  /**
//   * Current internal cluster stats, updated periodically via event bus.
//   */
//  @volatile
//  private var _latestStats = ClusterStats()
//
//  /**
//   * Current cluster metrics, updated periodically via event bus.
//   */
//  @volatile
//  private var _clusterMetrics: Set[NodeMetrics] = Set.empty
//
//  val selfAddress = cluster.selfAddress
//
//  // create actor that subscribes to the cluster eventBus to update current read view state
//  private val eventBusListener: ActorRef[ClusterDomainEvent] = {
////    cluster.system.asInstanceOf[ActorSystemImpl].systemActorOf(Props(new Actor {
//    cluster.system.actorOf(Props(new TypedActor[ClusterDomainEvent] {    
//      override def preStart(): Unit = cluster.subscribe(self, classOf[ClusterDomainEvent])
//      override def postStop(): Unit = cluster.unsubscribe(self)
//
//      def typedReceive = {
//        case e: ClusterDomainEvent ⇒ e match {
//          case SeenChanged(convergence, seenBy) ⇒
//            state = state.copy(seenBy = seenBy)
//          case MemberRemoved(member) ⇒
//            state = state.copy(members = state.members - member, unreachable = state.unreachable - member)
//          case UnreachableMember(member) ⇒
//            // replace current member with new member (might have different status, only address is used in equals)
//            state = state.copy(members = state.members - member, unreachable = state.unreachable - member + member)
//          case MemberDowned(member) ⇒
//            // replace current member with new member (might have different status, only address is used in equals)
//            state = state.copy(members = state.members - member, unreachable = state.unreachable - member + member)
//          case event: MemberEvent ⇒
//            // replace current member with new member (might have different status, only address is used in equals)
//            state = state.copy(members = state.members - event.member + event.member,
//              unreachable = state.unreachable - event.member)
//          case LeaderChanged(leader)                          ⇒ state = state.copy(leader = leader)
//          case s: CurrentClusterState                         ⇒ state = s
//          case CurrentInternalStats(stats)                    ⇒ _latestStats = stats
//          case ClusterMetricsChanged(nodes)                   ⇒ _clusterMetrics = nodes
//          case _: InstantClusterState | _: InstantMemberEvent ⇒ // not used here
//        }
//      }
//    }).withDispatcher(cluster.settings.UseDispatcher), name = "clusterEventBusListener")
//  }
//
//  def self: Member = {
//    state.members.find(_.address == selfAddress).orElse(state.unreachable.find(_.address == selfAddress)).
//      getOrElse(Member(selfAddress, MemberStatus.Removed))
//  }
//
//  /**
//   * Returns true if this cluster instance has be shutdown.
//   */
//  def isTerminated: Boolean = cluster.isTerminated
//
//  /**
//   * Current cluster members, sorted by address.
//   */
//  def members: SortedSet[Member] = state.members
//
//  /**
//   * Members that has been detected as unreachable.
//   */
//  def unreachableMembers: Set[Member] = state.unreachable
//
//  /**
//   * Member status for this node ([[akka.cluster.MemberStatus]]).
//   *
//   * NOTE: If the node has been removed from the cluster (and shut down) then it's status is set to the 'REMOVED' tombstone state
//   *       and is no longer present in the node ring or any other part of the gossiping state. However in order to maintain the
//   *       model and the semantics the user would expect, this method will in this situation return `MemberStatus.Removed`.
//   */
//  def status: MemberStatus = self.status
//
//  /**
//   * Is this node the leader?
//   */
//  def isLeader: Boolean = leader == Some(selfAddress)
//
//  /**
//   * Get the address of the current leader.
//   */
//  def leader: Option[Address] = state.leader
//
//  /**
//   * Does the cluster consist of only one member?
//   */
//  def isSingletonCluster: Boolean = members.size == 1
//
//  /**
//   * Returns true if the node is not unreachable and not `Down`
//   * and not `Removed`.
//   */
//  def isAvailable: Boolean = {
//    val myself = self
//    !unreachableMembers.contains(myself) &&
//      myself.status != MemberStatus.Down &&
//      myself.status != MemberStatus.Removed
//  }
//
//  /**
//   * Current cluster metrics.
//   */
//  def clusterMetrics: Set[NodeMetrics] = _clusterMetrics
//
//  /**
//   * INTERNAL API
//   * The nodes that has seen current version of the Gossip.
//   */
//  private[cluster] def seenBy: Set[Address] = state.seenBy
//
//  /**
//   * INTERNAL API
//   */
//  private[cluster] def latestStats: ClusterStats = _latestStats
//
//  /**
//   * Unsubscribe to cluster events.
//   */
//  def close(): Unit = if (!eventBusListener.isTerminated) {
//    eventBusListener.poisonPill
//  }
//
//}
