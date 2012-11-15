//
// TAkka project build file
//

import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._
import sbt.Project.Initialize

// Build setup
object TAkkaBuild extends Build {

  // Settings
  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    // Info
    organization := "takka",
    version      := "0.2.1",

    // Repositories
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    
    // Compile options
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-optimize"),
    javacOptions  ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),
    
    // sbtEclipse - see examples https://github.com/typesafehub/sbteclipse/blob/master/sbteclipse-plugin/src/sbt-test/sbteclipse/02-contents/project/Build.scala
    EclipseKeys.createSrc := EclipseCreateSrc.ValueSet(EclipseCreateSrc.Unmanaged, EclipseCreateSrc.Source, EclipseCreateSrc.Resource),
    EclipseKeys.withSource := true    
  )
    
  // Projects
  lazy val root = Project(id = "takka",
                          base = file("."),
                          settings = defaultSettings) aggregate(snapshot, examples,
                                                                takkasockowebserver, takkasockoexamples,
                                                                sockowebserver, sockoexamples)

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
                         
  lazy val scalabilityBenchmark = Project(id = "scalabilityBenchmark",
                         base = file("scalabilityBenchmark"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.scalability
                         )) dependsOn(snapshot)

  lazy val scalabilityAWS = Project(id = "scalabilityAWS",
                         base = file("scalabilityAWS"),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.aws
                         )) dependsOn(snapshot)

}

// Dependencies
object Dependencies {
  import Dependency._

  val snapshot = Seq(
    Dependency.akkaActor, Dependency.akkaKernel, Dependency.akkaSlf4j, Dependency.akkaTestKit,
    Dependency.netty, Dependency.logback, Dependency.junit, Dependency.scalacheck, Dependency.scalaSwing
  )
  
  val examples = Seq(
    Dependency.logback
  ) 
  

  // socko webserver in takka
  val takkasockowebserver = Seq(
    Dependency.akkaSlf4j, Dependency.akkaTestKit,
    Dependency.netty, Dependency.logback, Dependency.junit, Dependency.scalatest
  )
  
  // socko webserver examples in takka
  val takkasockoexamples = Seq(
    Dependency.logback
  )  

  val sockowebserver = Seq(
    Dependency.akkaActor, Dependency.akkaSlf4j, Dependency.akkaTestKit,
    Dependency.netty, Dependency.logback, Dependency.junit, Dependency.scalatest
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

}

object Dependency {
  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.9.1"
  val akkaActor     = "com.typesafe.akka"   % "akka-actor"         % "2.0.2"
  val akkaKernel    = "com.typesafe.akka"   % "akka-kernel"        % "2.0.2"
  val akkaRemote    = "com.typesafe.akka"   % "akka-remote"        % "2.0.2"
  val akkaSlf4j     = "com.typesafe.akka"   % "akka-slf4j"         % "2.0"
  val akkaTestKit   = "com.typesafe.akka"   % "akka-testkit"       % "2.0"
  val netty         = "io.netty"            % "netty"              % "3.5.2.Final"
  val logback       = "ch.qos.logback"      % "logback-classic"    % "1.0.3"         % "runtime"
  val junit         = "junit"               % "junit"              % "4.9"           % "test"
  val scalatest     = "org.scalatest"       %% "scalatest"         % "1.7.1"         % "test"
  val scalacheck    = "org.scalacheck"      %% "scalacheck"        % "1.9"

  val awsJava           = "com.amazonaws" % "aws-java-sdk" % "1.3.9"

}
