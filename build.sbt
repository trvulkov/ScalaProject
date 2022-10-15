name := "final-project"
version := "0.1"

scalaVersion := "3.1.1"

scalacOptions ++= Seq(
  "-new-syntax",
  "-indent",
  "-encoding", "utf8"
)

libraryDependencies ++= Seq(
  // Enables logging if required by some library
  // You can use it via https://github.com/lightbend/scala-logging
  "ch.qos.logback" % "logback-classic" % "1.2.11",
  "org.scalatest" %% "scalatest" % "3.2.11" % Test
)

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.13"
