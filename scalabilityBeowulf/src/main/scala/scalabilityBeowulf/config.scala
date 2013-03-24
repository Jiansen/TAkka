package scalabilityBeowulf

import com.typesafe.config.{ConfigFactory, Config}

case class BeowulfNode(ip:String, port:Int)

object BeowulfConfig {
  //  Beowulf Cluster at Heriot-Watt University    

  def node(ID:Int):BeowulfNode = ID match {
    case x if 1  <= x && x <= 9  => BeowulfNode("137.195.143.10"+x, 2552)
    case x if 10 <= x && x <= 32 => BeowulfNode("137.195.143.1"+x, 2552)
  }

  // localhost
/*
  def node(ID:Int):BeowulfNode = {
    BeowulfNode("127.0.0.1", 2500+ID)
  }
*/
  
  def masterNodeConfig(workerNodePrefix:String, processPathPrefix:String, processNamePrefix:String, p:Int, nodes:Int):Config = {
    def actorDeploymentString(p:Int, nodes:Int):String = {
      def calnode(i:Int,n:Int):Int = i % n match {
        case 0 => n
        case x => x
      }
      var result:String = ""
      for (i <- 1 to p) {
        val depNode:Int = calnode(i, nodes)
          result +=
            """/"""+processPathPrefix+"""/"""+processNamePrefix+i+""" {
//              remote = "akka.tcp://"""+workerNodePrefix+depNode+"@"+node(depNode).ip+""":"""+node(depNode).port+""""
              remote = "akka://"""+workerNodePrefix+depNode+"@"+node(depNode).ip+""":"""+node(depNode).port+""""
            }
            """
      }

      result
    }
    
    val configStr = """      
    akka {
      actor {
        provider = "akka.remote.RemoteActorRefProvider"
        deployment { 
          """ + actorDeploymentString(p, nodes) + """
        }
      }
      remote {
        enabled-transports = ["akka.remote.netty.tcp"]
        netty.tcp {
          hostname = "137.195.143.132"
          port = 2552
        }
      }
    }
    """  
    
//          println("config is: "+configStr)
    ConfigFactory.parseString(configStr)
  }
  
  def WorkerNodeConfig(nodeID:Int):Config = {
    ConfigFactory.parseString(
    """
        include "common"
        
    akka {
      actor {
        provider = "akka.remote.RemoteActorRefProvider"
      }
      remote {
        startup-timeout = 200 s
//        enabled-transports = ["akka.remote.netty.tcp"]
        transport = "akka.remote.netty.NettyRemoteTransport"
//        netty.tcp{
        netty {
          hostname = """" + node(nodeID).ip + """"
          port = """ + node(nodeID).port + """
        }
      }
    } 
    """
    )
  }
  
  val WorkerNodePrefix = "WorkerNode"
  val ProcessPathPrefix = "MasterActor"
  val ProcessNamePrefix = "Process"    
}
