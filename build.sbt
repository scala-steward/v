import sbt._

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "lgbt.princess"
ThisBuild / organizationName := "NthPortal"

mimaPreviousArtifacts := Set()
mimaFailOnNoPrevious := false

lazy val v = project
  .in(file("."))
  .settings(
    name := "v",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.1.0" % Test
    )
  )
