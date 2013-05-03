name := "monad-syntax"

description := "An intuitive notation for specifying type-safe effectful computations in Scala"

version := "0.1-SNAPSHOT"

organization := "org.pelotom"

homepage := Some(url("http://github.com/pelotom/monad-syntax"))

startYear := Some(2013)

scalaVersion := "2.10.0"

libraryDependencies <++= (scalaVersion) { sv => Seq("org.scala-lang" % "scala-reflect" % sv) }

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint", "-Xfatal-warnings")

scalacOptions ++= Seq("-language:higherKinds", "-language:postfixOps")
