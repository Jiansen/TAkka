//
// TAkka project build file
//

import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._
import sbt.Project.Initialize
import sbtassembly.Plugin._
import AssemblyKeys._


object TAkkaBuild extends Build {
  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",

    scalaVersion := "2.10.1",

    // Compile options
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-optimize"),
    javacOptions  ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),
    
    retrieveManaged := true,
    EclipseKeys.withSource := true

  )
   

 
  // Projects
  lazy val root = Project(id = "takka_root",
                          base = file("."),
                          settings = defaultSettings
                         ) aggregate(takka, scalabilityBeowulf,
 examples, scalabilityBenchmark)//,
//                                                                takkasockowebserver, takkasockoexamples,
//	                                                                sockowebserver, sockoexamples)

  lazy val takka = Project(id = "takka",
                         base = file("takka"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.snapshot
                      ))

  lazy val examples = Project(id = "examples",
                         base = file("examples"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.examples
                         ))  dependsOn(takka % "compile->compile")

  lazy val takkasockowebserver = Project(id = "takka-socko-webserver",
                         base = file("takka-socko-webserver"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.takkasockowebserver
                         )) dependsOn(takka)

  lazy val takkasockoexamples = Project(id = "takka-socko-examples",
                         base = file("takka-socko-examples"),
                         settings = defaultSettings ++ assemblySettings ++ Seq(
                           libraryDependencies ++= Dependencies.takkasockoexamples
                         )) dependsOn(takkasockowebserver)  

  lazy val sockowebserver = Project(id = "socko-webserver",
                         base = file("socko-webserver"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.sockowebserver 
                         ))


  lazy val sockoexamples = Project(id = "socko-examples",
                         base = file("socko-examples"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.sockoexamples
                         )) dependsOn(sockowebserver)

  lazy val scalabilityBenchmark = Project(id = "scalabilityBenchmark",
                         base = file("scalabilityBenchmark"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.scalability
                         )) dependsOn(takka)

  lazy val scalabilityBeowulf = Project(id = "scalabilityBeowulf",
                         base = file("scalabilityBeowulf"),
                         settings = defaultSettings ++ assemblySettings ++ Seq(
                           libraryDependencies ++= Dependencies.beowulf
                         )) dependsOn(takka)

  lazy val EnMAS = Project(id = "EnMAS",
                         base = file("EnMAS"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.EnMAS 
                         )) dependsOn(takka)

  /*
  lazy val scalabilityAWS = Project(id = "scalabilityAWS",
                         base = file("scalabilityAWS"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.aws
                         )) dependsOn(takka)

  lazy val scalabilityGAE = Project(id = "scalabilityGAE",
                         base = file("scalabilityGAE"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.gae
                         )) dependsOn(takka)

*/
  lazy val unfilteredScalate =
    uri("git://github.com/unfiltered/unfiltered-scalate#0.6.7")
}

// Dependencies
object Dependencies {
  import Dependency._

  val snapshot = Seq(
    Dependency.akkaActor, Dependency.akkaKernel, Dependency.akkaRemote, Dependency.akkaSlf4j, Dependency.akkaTestKit, // Dependency.akkaCluster, 
    Dependency.netty, Dependency.logback, Dependency.junit, Dependency.scalaSwing, Dependency.scalacheck,
    Dependency.scala_lib, Dependency.scala_comp, Dependency.akkaCluster
  )
  
  val examples = Seq(
    Dependency.logback
  ) 
  

  // socko webserver in takka
  val takkasockowebserver = Seq(
    Dependency.akkaSlf4j, Dependency.akkaTestKit,
    Dependency.netty, Dependency.logback, Dependency.junit , Dependency.scalatest
  )
  
  // socko webserver examples in takka
  val takkasockoexamples = Seq(
    Dependency.logback
  )  

  val sockowebserver = Seq(
    Dependency.akkaActor, Dependency.akkaSlf4j, Dependency.akkaTestKit,
    Dependency.netty, Dependency.logback, Dependency.junit , Dependency.scalatest
  )
  
  val sockoexamples = Seq(
    Dependency.logback
  )
  
  val EnMAS = Seq(
    Dependency.akkaActor, Dependency.logback, Dependency.unfiltered_async, Dependency.unfiltered_jetty, Dependency.unfiltered_netty
  )

  val scalability = Seq(
    Dependency.akkaActor, Dependency.logback
  )

  val beowulf = Seq(
    Dependency.akkaRemote, Dependency.netty, Dependency.logback
  )
}

object Dependency {
  // Versions
  object V {
    val Akka      = "2.1.2"
  }
  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.10.1"
  val akkaActor     = "com.typesafe.akka"   %% "akka-actor"  	% V.Akka
  val akkaKernel    = "com.typesafe.akka"   %% "akka-kernel" % V.Akka
  val akkaSlf4j     = "com.typesafe.akka"   %% "akka-slf4j"  % V.Akka
  val akkaRemote    = "com.typesafe.akka"   %% "akka-remote" % V.Akka
  val akkaTestKit   = "com.typesafe.akka"   %% "akka-testkit" % V.Akka

  val akkaCluster   = "com.typesafe.akka" %% "akka-cluster-experimental" % V.Akka
  
  val netty         = "io.netty"            % "netty"              % "3.6.5.Final"
  val logback       = "ch.qos.logback"      % "logback-classic"    % "1.0.11"         % "runtime"
  val junit         = "junit"               % "junit"              % "4.11"           % "test"

  val scalatest     = "org.scalatest"       %% "scalatest"         % "1.9.1" % "test"
  val scalacheck    = "org.scalacheck"      %% "scalacheck"        % "1.10.0" % "test"

  val scala_lib     = "org.scala-lang" % "scala-library" % "2.10.1" % "provided"
  val scala_comp    = "org.scala-lang" % "scala-compiler" % "2.10.1"

  val unfiltered_async = "net.databinder" %% "unfiltered-filter-async" % "0.6.7"
  val unfiltered_jetty = "net.databinder" %% "unfiltered-jetty" % "0.6.7"
  val unfiltered_netty = "net.databinder" %% "unfiltered-netty" % "0.6.7"

}
