//
// TAkka project build file
//

import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._
import sbt.Project.Initialize

//
// Build setup
//
object TAkkaBuild extends Build {

  //
  // Settings
  //
  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    // Info
    // organization := "takka",
    version      := "0.2.0",

    // Repositories
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    
    // Compile options
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-optimize"),
    javacOptions  ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),
    
    // sbtEclipse - see examples https://github.com/typesafehub/sbteclipse/blob/master/sbteclipse-plugin/src/sbt-test/sbteclipse/02-contents/project/Build.scala
    EclipseKeys.createSrc := EclipseCreateSrc.ValueSet(EclipseCreateSrc.Unmanaged, EclipseCreateSrc.Source, EclipseCreateSrc.Resource),
    EclipseKeys.withSource := true    
  )
    
  //
  // Packaging to SonaType using SBT
  //
  // https://github.com/sbt/sbt.github.com/blob/gen-master/src/jekyll/using_sonatype.md
  // http://www.cakesolutions.net/teamblogs/2012/01/28/publishing-sbt-projects-to-nexus/
  // https://docs.sonatype.org/display/Repository/How+To+Generate+PGP+Signatures+With+Maven
  //    
  /*
  def sockoPomExtra = {
    <url>http://www.sockoweb.org</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:mashupbots/socko.git</url>
      <connection>scm:git:git@github.com:mashupbots/socko.git</connection>
    </scm>
    <developers>
      <developer>
        <id>veebs</id>
        <name>Vibul Imtarnasan</name>
        <url>https://github.com/veebs</url>
      </developer>
      <developer>
        <id>lightningdb</id>
        <name>David Bolton</name>
        <url>https://github.com/lightningdb</url>
      </developer>
    </developers>
  }

  def sockoPublishTo: Initialize[Option[Resolver]] = {
    (version) { version: String =>
      val nexus = " https://oss.sonatype.org/"
      if (version.trim.endsWith("SNAPSHOT")) {
        Some("snapshots" at nexus + "content/repositories/snapshots/")
      } else {
        Some("releases" at nexus + "service/local/staging/deploy/maven2/")
      }
    }
  }
  */    
  //
  // Projects
  //
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
                         dependencies = Seq(snapshot),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.examples
                         ))  

  lazy val takkasockowebserver = Project(id = "takka-socko-webserver",
                         base = file("takka-socko-webserver"),
                         dependencies = Seq(snapshot),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.takkasockowebserver ))
//                           publishTo <<= sockoPublishTo,
//                           publishMavenStyle := true,
//                           publishArtifact in Test := false,
//                           pomIncludeRepository := { x => false },
//                           pomExtra := sockoPomExtra
//                         ))

  lazy val takkasockoexamples = Project(id = "takkasockoexamples",
                         base = file("takka-socko-examples"),
                         dependencies = Seq(takkasockowebserver),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.takkasockoexamples
                         ))  

  lazy val sockowebserver = Project(id = "socko-webserver",
                         base = file("socko-webserver"),
                         dependencies = Seq(snapshot),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.sockowebserver ))
//                           publishTo <<= sockoPublishTo,
//                           publishMavenStyle := true,
//                           publishArtifact in Test := false,
//                           pomIncludeRepository := { x => false },
//                           pomExtra := sockoPomExtra
//                         ))

  lazy val sockoexamples = Project(id = "sockoexamples",
                         base = file("socko-examples"),
                         dependencies = Seq(sockowebserver),
                         settings = defaultSettings ++ Seq(
                           libraryDependencies ++= Dependencies.sockoexamples
                         ))  
}

//
// Dependencies
//
object Dependencies {
  import Dependency._

  val snapshot = Seq(
    Dependency.akkaActor, Dependency.akkaKernel, Dependency.akkaSlf4j, Dependency.akkaTestKit,
    Dependency.netty, Dependency.logback, Dependency.junit, Dependency.scalacheck
  )
  
  val examples = Seq(
    Dependency.logback
  ) 
  

  // socko webserver in takka
  val takkasockowebserver = Seq(
    Dependency.akkaActor, Dependency.akkaSlf4j, Dependency.akkaTestKit,
    Dependency.netty, Dependency.logback, Dependency.junit, Dependency.scalatest
  )
  
  // socko webserver in takka
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
}

object Dependency {
  val akkaActor     = "com.typesafe.akka"   % "akka-actor"         % "2.0.2"
  val akkaKernel    = "com.typesafe.akka"   % "akka-kernel"        % "2.0.2"
  val akkaRemote    = "com.typesafe.akka"   % "akka-remote"        % "2.0.2"
  val akkaSlf4j     = "com.typesafe.akka"   % "akka-slf4j"         % "2.0"
  val akkaTestKit   = "com.typesafe.akka"   % "akka-testkit"       % "2.0"
  val netty         = "io.netty"            % "netty"              % "3.5.0.Final"
  val logback       = "ch.qos.logback"      % "logback-classic"    % "1.0.3"         % "runtime"
  val junit         = "junit"               % "junit"              % "4.9"           % "test"
  val scalatest     = "org.scalatest"       %% "scalatest"         % "1.7.1"         % "test"
  val scalacheck    = "org.scalacheck"      %% "scalacheck"        % "1.9"
}




