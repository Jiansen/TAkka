name := "TAkka"
 
version := "snapshot"
 
scalaVersion := "2.9.1"

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-actor" % "2.0",
  "com.typesafe.akka" % "akka-remote" % "2.0",
  "com.typesafe.akka" % "akka-kernel" % "2.0",
  "org.scalacheck" %% "scalacheck" % "1.9"
)

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }
