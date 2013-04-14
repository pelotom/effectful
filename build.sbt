scalaVersion := "2.10.1"

organization := "com.pelotom"

name := "monadblocks"

version := "0.1"

libraryDependencies <++= (scalaVersion) {
  sv => Seq(
    "org.scala-lang" % "scala-reflect" % sv
  )
}

libraryDependencies += "org.scalaz" %% "scalaz-core" % "[7,8)"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xlint", "-feature")

description := "An alternative to for-comprehensions for succinctly specifying monadic computations"

homepage := Some(url("http://github.com/pelotom/monadblocks"))

startYear := Some(2013)
