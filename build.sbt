name := """account-transfer"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
	"com.typesafe.akka" % "akka-actor_2.11" % "2.4.2",
	"com.typesafe.akka" % "akka-stream-experimental_2.11" % "2.0.3",
	"com.typesafe.akka" % "akka-http-experimental_2.11" % "2.4.2",
	"com.typesafe.akka" % "akka-http-spray-json-experimental_2.11" % "2.4.2",
	"com.typesafe.akka" %% "akka-slf4j" % "2.4.2",
	"ch.qos.logback" % "logback-classic" % "1.1.3",
	"org.scalaz" %% "scalaz-core" % "7.1.0",
	"org.scalactic" %% "scalactic" % "2.2.6",
	"org.scalatest" %% "scalatest" % "2.2.6" % "test",
	"org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)

//scalacOptions += "-Xprint:typer"
