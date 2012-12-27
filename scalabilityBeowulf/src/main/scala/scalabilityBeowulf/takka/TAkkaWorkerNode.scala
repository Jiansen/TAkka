package scalabilityBeowulf.takka

import scalabilityBeowulf.BeowulfConfig._

object TAkkaWorkerNode extends App {  
    private val nodeID = args(0).toInt
    private val system = takka.actor.ActorSystem(WorkerNodePrefix+nodeID, WorkerNodeConfig(nodeID))
}
