package org.enmas.typed.client.http

import org.enmas.pomdp._, org.enmas.typed.client.ClientManager, org.enmas.typed.messaging._,
       org.enmas.typed.client.Agent, org.enmas.util.voodoo.ClassLoaderUtils._,
       scala.swing._, scala.swing.event._, scala.swing.BorderPanel.Position._,
       //akka.actor._, akka.dispatch._, akka.util.duration._, akka.pattern.ask,
       takka.actor._, akka.dispatch._, scala.concurrent.duration._, takka.pattern,
       unfiltered.request._, unfiltered.response._, unfiltered.netty._

class NetInterface(application: ActorRef[ClientManagerMessage]) {
  import ClientManager._

  unfiltered.jetty.Http(8080).filter(RequestHandler).run // starts HTTP service

  private object RequestHandler extends unfiltered.filter.async.Plan {
    def intent = {

      // List locally available POMDPs
      // /pomdps
      case req@Path(Seg("pomdpClasses" :: Nil))  ⇒ {
        (application ? GetLocalPOMDPs) onSuccess {
          case POMDPList(pomdps)  ⇒ { req respond ResponseString(
            pomdps.foldLeft("{ success: [") {
              (s, p)  ⇒ s + "\"%s\", ".format(p.getClass.getName)
            }.stripSuffix(", ") + "] }\n"
          )}
        } onFailure { case _  ⇒ req respond ResponseString(
          "{ error: \"An unknown error occurred.\" }\n"
        )}
      }

      // Request New Server Instance on Host
      // /<hostAddress>/pomdp/create/<className>
      case req@Path(Seg(address :: "instance" :: "create" :: name :: Nil))  ⇒ {
        application ! CreateServer(address, name)
        req respond ResponseString(
          "{ success: \"Request sent.\" }\n"
        )
      }

      // List Server Instances on Host
      // /<hostAddress>/instances
      case req@Path(Seg(address :: "instances" :: Nil))  ⇒ {
        (application ? ScanHost(address)) onSuccess {
          case reply: DiscoveryReply  ⇒ { req respond ResponseString(
            reply.servers.foldLeft("{ success: [") {
              (s: String, srv: ServerSpec)  ⇒ s + "\"%s\", ".format(srv)
            }.stripSuffix(", ") + "] }\n"
          )}
        } onFailure { case _  ⇒ req respond ResponseString(
          "{ error: \"The specified host could not be contacted.\" }\n"
        )}
      }

      // Create Session
      // <hostAddress>/session/create/<instanceNumber>
      case req@Path(Seg(address :: "session" :: "create" :: instance :: Nil))  ⇒ {
        try {
          (application ? ScanHost(address)) onSuccess {
            case reply: DiscoveryReply  ⇒ {
              val serverSpec = reply.servers.toSeq(instance.toInt)
              (application ? CreateSession(serverSpec)) onSuccess {
                case true  ⇒ req respond ResponseString(
                  "{ success: \"Session created.\" }\n")
                case false  ⇒ req respond ResponseString(
                  "{ error: \"Session creation failed.\" }\n")
              }
            }
          } onFailure { case _  ⇒ req respond ResponseString(
            "{ error: \"The specified host could not be contacted.\" }\n"
          )}
        }
        catch { case _:Throwable  ⇒ req respond ResponseString(
          "{ error: \"Argument formatting error.\" }\n"
        )}
      }

      // List Sessions
      // /sessions
      case req@Path(Seg("sessions" :: Nil))  ⇒ {
        (application ? GetSessions) onSuccess {
          case ActiveSessionList(sessions)  ⇒ {
            req respond ResponseString(
              "{ success: \""+sessions+"\" }\n"
            )
          }
        } onFailure { case _  ⇒ req respond ResponseString(
          "{ error: \"The specified host could not be contacted.\" }\n"
        )}
      }

      // List Agents on Session
      // /session/<hostAddress>/<sessionId>/agents
      case req@Path(Seg("session" :: hostAddress :: sessionId :: "agentClasses" :: Nil))  ⇒ {
        req respond ResponseString("{ error: \"Not implemented yet.\" }\n")
      }

      // List Iteration Subscribers on Session
      // /session/<hostAddress>/<sessionId>/subscribers
      case req@Path(Seg("session" :: hostAddress :: sessionId :: "subscriberClasses" :: Nil))  ⇒ {
        req respond ResponseString("{ error: \"Not implemented yet.\" }\n")
      }

      // Create Agent on Session
      // /session/<hostAddress>/<sessionId>/agent/create/<name>
      case req@Path(Seg("session" :: hostAddress :: sessionId :: "agent" :: "create" :: name :: Nil))  ⇒ {
        req respond ResponseString("{ error: \"Not implemented yet.\" }\n")
      }

      // Create Iteration Subscriber on Session
      // /session/<hostAddress>/<sessionId>/subscriber/create/<name>
      case req@Path(Seg("session" :: hostAddress :: sessionId :: "subscriber" :: "create" :: name :: Nil))  ⇒ {
        req respond ResponseString("{ error: \"Not implemented yet.\" }\n")
      }

      // Get Session Status
      // /session/<hostAddress>/<sessionId>
      case req@Path(Seg("session" :: hostAddress :: sessionId :: Nil))  ⇒ {
        req respond ResponseString("{ error: \"Not implemented yet.\" }\n")
      }
      
    }
  }
}