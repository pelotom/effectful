name := "effectful"

description := "A syntax for type-safe effectful computations in Scala"

version := "0.1-SNAPSHOT"

organization := "org.effectful"

homepage := Some(url("http://github.com/pelotom/effectful"))

startYear := Some(2013)

scalaVersion := "2.10.2"

libraryDependencies <++= (scalaVersion) { sv => Seq("org.scala-lang" % "scala-reflect" % sv) }

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint", "-Xfatal-warnings")

scalacOptions ++= Seq("-language:higherKinds", "-language:postfixOps")
