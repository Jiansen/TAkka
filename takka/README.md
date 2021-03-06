



# Examples #

## Running sample applications in sbt ##

Examples in this repository is orgnised using *sbt*.  Users are recommanded to run sample examples as follows:

### 1. Set environment ###
1. Install a recent JRE (JAVA Runtime Enviromement).
2. Install *git* so that it will be easier to access this repository. See http://git-scm.com/.
3. Install *sbt*.  See https://github.com/harrah/xsbt/wiki/Getting-Started-Welcome.

### 2. Obtain the latest release ###
1. copy to a local working folder. $ git clone git@github.com:Jiansen/TAkka.git
2. enter to the working folder. $ cd TAkka/< version >
3. (option) update the working copy. $ git pull
4. (option) $ sbt update

### 3. Run applications ###
$ cd < TAkka/version folder>  
$ sbt run  
*sbt* will promote you to choose which application to run.  Enter the application number to run.  

## List of sample examples ##
sample.untyped.< Example.scala >: examples written in Akka or other poorly typed frameworks
sample.takka.< Example.scala >: examples written in TAkka.

+ FirstActorTest(takka version only)  
  A tiny example demonstrating how to  
  -- run program in sbt  
  -- define a TAkka actor  
  -- send messages  
  -- use typed nameserver
+ SupervisionTreeDemo
  building supervision tree in akka and takka.
+ Pingpong
  A local ping-pong application, including a simple benchmark.
+ FSMEffTest  
  benchmark on the efficiency of state transactions in FSM
+ DiningHakkersOnFsm  
  The Dining Hakkers problem using FSM.  
  -- The akka version is copied from 
https://github.com/akka/akka/blob/master/akka-samples/akka-sample-fsm/src/main/scala/DiningHakkersOnFsm.scala#L1  
  -- The TAkka version is adapted from the akka version.


## List of bigger examples ##
Following examples are adapted from open source repositories.



