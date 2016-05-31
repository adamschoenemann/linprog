name := "Linear Programming for Intelligent Systems Programming"

version := "0.0"

scalaVersion := "2.11.7"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"

val libraryVersion = "1.2.0-M1" // or "1.3.0-SNAPSHOT"

mainClass in Compile := Some("adsc.isp.Main")