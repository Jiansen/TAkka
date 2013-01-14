//
// TAkka project build file
//

import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._
import sbt.Project.Initialize

object TAkkaBuild extends Build {
  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",

    scalaVersion := "2.10.0",

    // Compile options
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-optimize"),
    javacOptions  ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),
    
    // sbtEclipse - see examples https://github.com/typesafehub/sbteclipse/blob/master/sbteclipse-plugin/src/sbt-test/sbteclipse/02-contents/project/Build.scala
    retrieveManaged := true,
//    EclipseKeys.createSrc := EclipseCreateSrc.ValueSet(EclipseCreateSrc.Managed, EclipseCreateSrc.Resource),
    EclipseKeys.withSource := true    
  )
    
  // Projects
  lazy val root = Project(id = "takka",
                          base = file("."),
                          settings = defaultSettings) aggregate(snapshot, examples,
                                          scalabilityBenchmark, scalabilityBeowulf)
//                                                                takkasockowebserver, takkasockoexamples,
//	                                                                sockowebserver, sockoexamples)

  lazy val snapshot = Project(id = "snapshot",
                         base = file("snapshot"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.snapshot
                      ))

  lazy val examples = Project(id = "examples",
                         base = file("examples"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.examples
                         ))  dependsOn(snapshot)
/*
  lazy val takkasockowebserver = Project(id = "takka-socko-webserver",
                         base = file("takka-socko-webserver"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.takkasockowebserver
                         )) dependsOn(snapshot)

  lazy val takkasockoexamples = Project(id = "takka-socko-examples",
                         base = file("takka-socko-examples"),
                         settings = defaultSettings ++ Seq(
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
*/                         
  lazy val scalabilityBenchmark = Project(id = "scalabilityBenchmark",
                         base = file("scalabilityBenchmark"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.scalability
                         )) dependsOn(snapshot)

  lazy val scalabilityBeowulf = Project(id = "scalabilityBeowulf",
                         base = file("scalabilityBeowulf"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.beowulf
                         )) dependsOn(snapshot)

  /*
  lazy val scalabilityAWS = Project(id = "scalabilityAWS",
                         base = file("scalabilityAWS"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.aws
                         )) dependsOn(snapshot)

  lazy val scalabilityGAE = Project(id = "scalabilityGAE",
                         base = file("scalabilityGAE"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.gae
                         )) dependsOn(snapshot)

*/
}

// Dependencies
object Dependencies {
  import Dependency._

  val snapshot = Seq(
    Dependency.akkaActor, Dependency.akkaKernel, Dependency.akkaRemote, Dependency.akkaSlf4j, Dependency.akkaTestKit,
    Dependency.netty, Dependency.logback, Dependency.junit, Dependency.scalaSwing, Dependency.scalacheck,
    Dependency.scala_lib, Dependency.scala_comp
  )
  
  val examples = Seq(
    Dependency.logback
  ) 
  

  // socko webserver in takka
  val takkasockowebserver = Seq(
    Dependency.akkaSlf4j, Dependency.akkaTestKit,
    Dependency.netty, Dependency.logback, Dependency.junit//, Dependency.scalatest
  )
  
  // socko webserver examples in takka
  val takkasockoexamples = Seq(
    Dependency.logback
  )  

  val sockowebserver = Seq(
    Dependency.akkaActor, Dependency.akkaSlf4j, Dependency.akkaTestKit,
    Dependency.netty, Dependency.logback, Dependency.junit//, Dependency.scalatest
  )
  
  val sockoexamples = Seq(
    Dependency.logback
  )
  
  val scalability = Seq(
    Dependency.akkaActor, Dependency.logback
  )
  
  val aws = Seq(
    Dependency.awsJava, Dependency.logback
  )

  val gae = Seq(
    Dependency.logback
  )

  val beowulf = Seq(
    Dependency.akkaRemote, Dependency.netty, Dependency.logback
  )
}

object Dependency {
  // Versions
  object V {
    val Akka      = "2.1.0"
  }
  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.10.0"
  val akkaActor     = "com.typesafe.akka"   % "akka-actor_2.10"  	% V.Akka
  val akkaKernel    = "com.typesafe.akka"   % "akka-kernel_2.10" 	% V.Akka
  val akkaRemote    = "com.typesafe.akka"   % "akka-remote_2.10" 	% V.Akka
  val akkaSlf4j     = "com.typesafe.akka"   % "akka-slf4j_2.10"  	% V.Akka
  val akkaTestKit   = "com.typesafe.akka"   % "akka-testkit_2.10"	% V.Akka
  val netty         = "io.netty"            % "netty"              % "3.6.1.Final"
  val logback       = "ch.qos.logback"      % "logback-classic"    % "1.0.7"         % "runtime"
  val junit         = "junit"               % "junit"              % "4.10"           % "test"
  val scalatest     = "org.scalatest"       %% "scalatest"         % "1.9.1" % "test"
  val scalacheck    = "org.scalacheck"      %% "scalacheck"        % "1.10.0" % "test"

  val awsJava       = "com.amazonaws" %% "aws-java-sdk" % "1.3.9"
  val scala_lib     = "org.scala-lang" % "scala-library" % "2.10.0" % "provided"
  val scala_comp    = "org.scala-lang" % "scala-compiler" % "2.10.0"
}
