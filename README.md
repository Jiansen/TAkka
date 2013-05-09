# TAkka #

TAkka provides better typed [akka](http://akka.io) style APIs for concurrent, fault-tolerant and scalable applications.

TAkka is Open Source and available under the Apache 2 License.

## Source Code in This Repository ##
Source code in this repository is either original or derivative works of open source project.  Work in this reporsitory are licensed under the Apache License, Version 2.0.

* snapshot: the developer version of the takka framework.

* examples: small takka and akka examples

* socko-webserver: source code of the [socko webserver project](http://sockoweb.org/).  Retrieved on 24 June 2012 

* socko-examples: examples of socko.  Retrieved on 24 June 2012

* takka-socko-webserver: rewrite socko using takka.

* takka-socko-examples: rewrite socko examples using takka-socko.

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

* running sbt on different beowulf nodes may re-compile the code.  BUG?
* We use [sbt-assembly](https://github.com/sbt/sbt-assembly) to package all requried files in a single jar, so that the benchmark application can be run as standard JAVA application
* the bash script for running the benchmark are given at ***

$ sbt
> project scalabilityBeowulf
> assembly

java -cp /home/jiansen/TAkka/scalabilityBeowulf/target/scala-2.10/scalabilityBeowulf-assembly-0.1-SNAPSHOT.jar scalabilityBeowulf.takka.TAkkaWorkerNode 01

## Getting Help

Section under construction

## Links

Section under construction


