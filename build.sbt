ThisBuild / scalaVersion := "2.13.1"
ThisBuild / autoAPIMappings := true

// publishing info
inThisBuild(
  Seq(
    organization := "lgbt.princess",
    homepage := Some(url("https://github.com/NthPortal/v")),
    licenses := Seq("The Apache License, Version 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    developers := List(
      Developer(
        "NthPortal",
        "April | Princess",
        "dev@princess.lgbt",
        url("https://nthportal.com")
      )
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/NthPortal/v"),
        "scm:git:git@github.com:NthPortal/v.git",
        "scm:git:git@github.com:NthPortal/v.git"
      )
    )
  )
)

lazy val v = project
  .in(file("."))
  .settings(
    name := "v",
    mimaPreviousArtifacts := Set("0.1.0").map(organization.value %% name.value % _),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.1.2" % Test
    ),
    scalacOptions ++= {
      if (isSnapshot.value) Nil
      else
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 13)) => Seq("-opt:l:inline", "-opt-inline-from:lgbt.princess.v.**")
          case _             => Nil
        }
    }
  )
