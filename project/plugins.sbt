resolvers ++= Seq(
  "repo.jenkins-ci.org" at "https://repo.jenkins-ci.org/public",
  Resolver.jcenterRepo
)
addSbtPlugin("ohnosequences" % "nice-sbt-settings" % "0.9.0")
