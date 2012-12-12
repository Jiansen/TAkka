
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.0")

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.cc",
  Resolver.url("sbt-plugin-releases",
    url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)
)

