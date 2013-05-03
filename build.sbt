name := "monad-syntax"

description := "An alternative to for-comprehensions for succinctly specifying monadic computations"

version := "0.1-SNAPSHOT"

organization := "org.pelotom"

scalaVersion := "2.10.0"

libraryDependencies <++= (scalaVersion) { sv => Seq("org.scala-lang" % "scala-reflect" % sv) }

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint", "-Xfatal-warnings")

scalacOptions ++= Seq("-language:higherKinds", "-language:postfixOps")

homepage := Some(url("http://github.com/pelotom/monad-syntax"))

startYear := Some(2013)
