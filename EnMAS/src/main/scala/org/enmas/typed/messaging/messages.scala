package org.enmas.typed.messaging

import org.enmas.pomdp._,
       org.enmas.typed.server._,
       org.enmas.typed.client._,
       org.enmas.util.FileUtils._,
       takka.actor._ 

       
sealed trait ServerMessage// extends Message // new message type
trait ClientMessage// extends Message // new message type


sealed trait ServerManagerMessage// extends Message // new message type
trait ClientManagerMessage// extends Message // new message type
case object Init extends ClientManagerMessage // new message type

case object Ping extends ServerMessage with ClientManagerMessage// new subtyping relation
case object Pong extends ClientMessage // new subtyping relation

/** Wraps a series of Messages destined for agents 
  * cohabitating on a given host.  The ClientManager
  * allows for the creation of many agents per host,
  * so this takes advantage of that to reduce unnecessay
  * network overhead and send/receive events.
  */
case class MessageBundle(content: List[AgentMessage]) extends ClientManagerMessage

//sealed trait Message

//sealed trait AgentMessage extends Message { val agentNumber: Int }
sealed trait AgentMessage extends ClientMessage { val agentNumber: Int }

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
  ref: ActorRef[ServerMessage],
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
  val ref: ActorRef[ClientManagerMessage]
)

/** Sent from a ClientManager to a ServerManager
  */
case object RequestProvisions extends ServerManagerMessage // new subtyping relation

/** Sent from a ClientManager to a ServerManager
  */
case class Provision(fileData: FileData) extends ClientManagerMessage with ServerManagerMessage // new subtyping relation

/** Sent from a ClientManager to a ServerManager
  */
case class CreateServerFor(className: String) extends ServerManagerMessage // new subtyping relation

/** Sent from a ClientManger to a Server
  */
case class RegisterHost(ref: ActorRef[ClientManagerMessage]) extends ServerMessage // Message

/** Sent from a Server to a ClientManager
  */
//  case class ConfirmHostRegistration(id: Int) extends Message
case class ConfirmHostRegistration(id: Int) extends ClientManagerMessage // new subtyping relation

/** Sent from a ClientManager to a ServerManager
  */
//case object Discovery extends Message
case object Discovery extends ServerManagerMessage // new subtyping relation

/** Sent from a ServerManager to ClientManager in
  * response to Discovery
  */
case class DiscoveryReply(
  servers: List[ServerSpec]
) // extends Message
extends ClientManagerMessage // new subtyping relation
 
/** Sent from a Server to a ClientManager
  */
case object DenyHostRegistration extends ClientManagerMessage // new subtyping relation // extends Message

/** Sent from a ClientManger to a Server
  */
case class RegisterAgent(
  sessionID: Int,
  agentType: AgentType
) extends ServerMessage //extends Message


sealed trait SessionGUIMessage
/** Sent from a Server to a Session GUI
  */
case class ConfirmAgentRegistration (
  agentNumber: Int,
  agentType: AgentType,
  actions: Set[Action]
) extends SessionGUIMessage with AgentMessage {
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

/** Sent from a Server to a Session GUI
  */
case object DenyAgentRegistration extends SessionGUIMessage// extends Message

/** Sent from an Agent to a Session, then
  * forwarded from that Session to a Server
  */
case class TakeAction(agentNumber: Int, action: Action ) extends ClientManagerMessage with ServerMessage//extends Message

/** Sent from a Server to a Session, then
  * forwarded from that Session to an Agent
  */
case class UpdateAgent(
  agentNumber: Int,
  observation: Observation,
  reward: Float
) extends ClientManagerMessage with AgentMessage

/** Sent from a Session to a Server
*/
// also a message for Session
case class AgentDied(id: Int) extends ServerMessage with ClientManagerMessage

/** For lightweight subscription / publisher support
  */
//case object Subscribe
case object Subscribe extends ServerMessage

/** For lightweight subscription / publisher support
  */
// case object Unsubscribe
case object Unsubscribe extends ServerMessage


//sealed trait Error extends Message { val cause: Throwable }
sealed trait Error { val cause: Throwable }
case class ClientError(cause: Throwable) extends Error with ClientMessage
case class ServerError(cause: Throwable) extends Error with ServerMessage
case class ClientManagerError(cause: Throwable) extends Error with ClientManagerMessage
