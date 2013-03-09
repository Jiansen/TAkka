package org.enmas.messaging

import org.enmas.pomdp._,
       org.enmas.server._,
       org.enmas.client._,
       org.enmas.util.FileUtils._,
       akka.actor._

case object Ping
case object Pong

/** Wraps a series of Messages destined for agents 
  * cohabitating on a given host.  The ClientManager
  * allows for the creation of many agents per host,
  * so this takes advantage of that to reduce unnecessay
  * network overhead and send/receive events.
  */
case class MessageBundle(content: List[AgentMessage])

sealed trait Message

sealed trait AgentMessage extends Message { val agentNumber: Int }

/** Represents a reference to a remote Agent from the point
  * of view of a Server.
  */
case class AgentSpec(
  sessionID: Int,
  agentNumber: Int,
  agentType: AgentType
) {
  final override def toString() =
    "Number: [%s], Type: [%s]".format(agentNumber, agentType)
}

/** Represents a reference to a Server from the point of
  * view of a ServerManager or ClientManager
  */
case class ServerSpec(
  ref: ActorRef,
  pomdpClassName: String,
  pomdpName: String,
  pomdpDescription: String
) {
  final override def toString() = pomdpName
}

/** Represents a reference to a Session from the point
 * of view of a Server.
 */
case class SessionSpec(
  val id: Int,
  val ref: ActorRef
)

/** Sent from a ClientManager to a ServerManager
  */
case object RequestProvisions

/** Sent from a ClientManager to a ServerManager
  */
case class Provision(fileData: FileData)

/** Sent from a ClientManager to a ServerManager
  */
case class CreateServerFor(className: String)

/** Sent from a ClientManger to a Server
  */
case class RegisterHost(ref: ActorRef) extends Message

/** Sent from a Server to a ClientManager
  */
case class ConfirmHostRegistration(id: Int) extends Message

/** Sent from a ClientManager to a ServerManager
  */
case object Discovery extends Message

/** Sent from a ServerManager to ClientManager in
  * response to Discovery
  */
case class DiscoveryReply(
  servers: List[ServerSpec]
) extends Message

/** Sent from a Server to a ClientManager
  */
case object DenyHostRegistration extends Message

/** Sent from a ClientManger to a Server
  */
case class RegisterAgent(
  sessionID: Int,
  agentType: AgentType
) extends Message

/** Sent from a Server to a Session gui
  */
case class ConfirmAgentRegistration(
  agentNumber: Int,
  agentType: AgentType,
  actions: Set[Action]
) extends AgentMessage {
  final override def toString() = "Number: [%s], Type: [%s]".format(agentNumber, agentType)
}

/** Used within Session
  */
case class ConfirmClientRegistration(
  clientNumber: Int,
  clientType: String
) {
  final override def toString() = "Number: [%s], Type: [%s]".format(clientNumber, clientType)
}

/** Sent from a Server to a Session
  */
case object DenyAgentRegistration extends Message

/** Sent from an Agent to a Session, then
  * forwarded from that Session to a Server
  */
case class TakeAction(agentNumber: Int, action: Action ) extends Message

/** Sent from a Server to a Session, then
  * forwarded from that Session to an Agent
  */
case class UpdateAgent(
  agentNumber: Int,
  observation: Observation,
  reward: Float
) extends AgentMessage

/** Sent from a Session to a Server
*/
case class AgentDied(id: Int)

/** For lightweight subscription / publisher support
  */
case object Subscribe

/** For lightweight subscription / publisher support
  */
case object Unsubscribe

sealed trait Error extends Message { val cause: Throwable }
case class ClientError(cause: Throwable) extends Error
case class ServerError(cause: Throwable) extends Error
