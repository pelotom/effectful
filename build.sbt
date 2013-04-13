scalaVersion := "2.10.1"

organization := "com.pelotom"

name := "monadblocks"

version := "0.1"

libraryDependencies <++= (scalaVersion) {
  sv => Seq(
    "org.scala-lang" % "scala-reflect" % sv
  )
}

libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test"

libraryDependencies += "junit" % "junit-dep" % "4.10" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "[7,8)"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xlint", "-feature")

description := "An alternative to for-comprehensions for succinctly specifying monadic computations"

homepage := Some(url("http://github.com/pelotom/monadblocks"))

startYear := Some(2013)
