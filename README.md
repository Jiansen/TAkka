# TAkka #

TAkka provides better typed [akka](http://akka.io) style APIs for concurrent, fault-tolerant and scalable applications.

TAkka is Open Source and available under the Apache 2 License.

## Source Code in This Repository ##
Source code in this repository is either original or derivative works of open source project.  Work in this reporsitory are licensed under the Apache License, Version 2.0.

* HWbwlfscript: shell script for running scalability benchmarks on the Beowulf cluster at Heriot-Watt University

* EnMAS: TAkka implementation of EnMAS (https://github.com/EnMAS/EnMAS-Framework) project.

* examples: small takka and akka examples.

* scalabilityBenchmark: scalability Benchmarks on a signle machine.

* scalabilityBeowulf: scalability Benchmark on Beowulf cluster, tested on the cluster at Heriot-Watt University.

* socko-webserver: source code of the [socko webserver project](http://sockoweb.org/).  Retrieved on 24 June 2012 

* socko-examples: examples of socko.  Retrieved on 24 June 2012

* takka-gatling: TAkka implementation of the [Gatling](https://github.com/excilys/gatling) project.

* takka-socko-webserver: TAkka implementation of Socko[https://github.com/mashupbots/socko]

* takka-socko-examples: TAkka implementation of Socko examples

* takka: source code of the TAkka library.


## Related Code in Other Repository ##
* [TAkka Play](https://github.com/Jiansen/TAkka-Play)
* The ATM example and the Elavator example are ported from commercial courses used at QuivQ and Erlang Solutions.   They are saved in a private repository.   If you would like to have an access to the code, please contact me first or directly contact QuviQ.com and Erlang Solutions for their permissions.

## Editing the Source Code

* We are currently using [Eclipse 3.8 Juno](http://projects.eclipse.org/releases/juno) 
  with [Scala IDE nightly build](http://scala-ide.org/download/nightly.html)
* We are currently using Scala 2.10 and JDK7.

* copy to a local working folder. $ git clone git@github.com:Jiansen/TAkka.git
* (read-only copy.  $ git clone git://github.com/Jiansen/TAkka.git)
* enter to the working folder. $ cd TAkka
* (option) update the working copy. $ git pull
* (option) $ sbt update

* Generate eclipse project files: `sbt eclispse`

* Start `Eclipse`

* From the top menu, select `File` | `Import`
  * Select `General` | `Existing Projects into Workspace`. 
  * Click `Next`.
  * Select the `TAkka` source code directory as the root
  * Should see `snapshot` etc. under `Projects`
  * Click `Finish`


## Testing on Beowulf

Modify the script used in this project or create your own script according to the following guidance.

* running sbt on different beowulf nodes may re-compile the code.
* We use [sbt-assembly](https://github.com/sbt/sbt-assembly) to package all requried files in a single jar, so that the benchmark application can be run as standard JAVA application
* the bash script for running the benchmark are given at ***

$ sbt "project scalabilityBeowulf" "assembly"

* start node

$ java -cp ./scalabilityBeowulf/target/scala-2.10/scalabilityBeowulf-assembly-0.1-SNAPSHOT.jar scalabilityBeowulf.takka.TAkkaWorkerNode 01


* run bench

## Setting up EC2 with Auto Scaling

Follow instructions given in [Web_Server_with_AWS_Auto_Scaling.md](https://github.com/Jiansen/TAkka/blob/master/Web_Server_with_AWS_Auto_Scaling.md)

## Testing Socko on EC2 with Auto Scaling

* build application

$ sbt "project takka-socko-examples" "assembly"

* start application

$ java -cp ./takka-socko-examples/target/scala-2.10/takka-socko-examples-assembly-0.1-SNAPSHOT.jar org.mashupbots.socko.framework_benchmarks.BenchmarkApp


* run bench
http://hostname:8888/json


