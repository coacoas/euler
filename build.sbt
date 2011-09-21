name := "euler"

scalaVersion := "2.9.1"

version := "1.0"

resolvers ++= Seq(
  "Local repository" at "http://localhost:8081/artifactory/repo"
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.6.1" % "test",
  "org.specs2" %% "specs2-scalaz-core" % "6.0.1"
)
