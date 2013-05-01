package sample.takka

import com.typesafe.config.{ConfigFactory, Config}

import takka.actor._
import takka.clusterwrapper.Cluster
import akka.cluster.ClusterEvent._

object ClusterConfig {
  def getConfig():Config = {
    val configStr = """      
      akka {
        actor {
          provider = "akka.cluster.ClusterActorRefProvider"
        }
        remote {
          transport = "akka.remote.netty.NettyRemoteTransport"
          log-remote-lifecycle-events = off
            netty {
              hostname = "127.0.0.1"
              port = 0
            }
        }
 
        cluster {
          seed-nodes = [
            "akka://ClusterSystem@127.0.0.1:2551", 
            "akka://ClusterSystem@127.0.0.1:2552"]
 
            auto-down = on
        }
      }
    """  
          
    ConfigFactory.parseString(configStr)    
  }
}


class ClusterListener extends TypedActor[ClusterDomainEvent] with akka.actor.ActorLogging{
    def typedReceive = {
        case state: CurrentClusterState ⇒
          log.info("Current members: {}", state.members)
        case MemberJoined(member) ⇒
          log.info("Member joined: {}", member)
        case MemberUp(member) ⇒
          log.info("Member is Up: {}", member)
        case UnreachableMember(member) ⇒
          log.info("Member detected as unreachable: {}", member)
        case _: ClusterDomainEvent ⇒ // ignore
 
    }  
}

object TAkkClusterExample extends App {
// Override the configuration of the port 
    // when specified as program argument
    if (args.nonEmpty) System.setProperty("akka.remote.netty.port", args(0))
 
    // Create an Akka system
    val system = ActorSystem("ClusterSystem", ClusterConfig.getConfig)
    val clusterListener = system.actorOf(Props[ClusterDomainEvent, ClusterListener], name = "clusterListener")
 
    Cluster(system).subscribe(clusterListener, classOf[ClusterDomainEvent])
}