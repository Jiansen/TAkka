package scalabilityBeowulf.akka

import scalabilityBeowulf.BeowulfConfig._

object AkkaWorkerNode extends App {  
    private val nodeID = args(0).toInt
    private val system = akka.actor.ActorSystem(WorkerNodePrefix+nodeID, WorkerNodeConfig(nodeID))
}
