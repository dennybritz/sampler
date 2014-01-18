import AssemblyKeys._

name := "sampler"

version := "0.1"

scalaVersion := "2.10.3"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= List(
  "ch.qos.logback" % "logback-classic" % "1.0.7",
  "com.github.scopt" %% "scopt" % "3.2.0",
  "com.typesafe.akka" %% "akka-actor" % "2.3-M2",
  "com.typesafe.akka" %% "akka-slf4j" % "2.3-M2",
  "com.typesafe.akka" %% "akka-testkit" % "2.3-M2",
  "net.sf.opencsv" % "opencsv" % "2.3",
  "org.scalatest" % "scalatest_2.10" % "2.0.RC2" % "test"
)

assemblySettings