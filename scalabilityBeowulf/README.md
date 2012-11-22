# Scalability Test on Beowulf #

## Test Instruction
Step 1:

Implement the scalabilityBeowulf.BeowulfConfig.node(ID:Int) method, which maps node ID to node address and port number

Step 2: 

On each Beowulf node (1,2,3 ..)

$sbt

\>project scalabilityBeowulf

\>run $node id$

[info] Running scalabilityBeowulf.akka.ran.RANNode 5


Step 3:

 On the master node

$sbt

\>project scalabilityBeowulf

\>run $args$

\>select main application

[info] Running scalabilityBeowulf.akka.ran.RAN 6


