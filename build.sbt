scalaVersion := "2.10.0"

organization := "org.pelotom"

name := "monad-syntax"

version := "0.1-SNAPSHOT"

libraryDependencies <++= (scalaVersion) { sv => Seq("org.scala-lang" % "scala-reflect" % sv) }

libraryDependencies += "org.scalaz" %% "scalaz-core" % "[7,8)"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint", "-Xfatal-warnings")

description := "An alternative to for-comprehensions for succinctly specifying monadic computations"

homepage := Some(url("http://github.com/pelotom/monad-syntax"))

startYear := Some(2013)
