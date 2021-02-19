ThisBuild / scalaVersion := "2.13.3"
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
        url("https://nthportal.com"),
      )
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/NthPortal/v"),
        "scm:git:git@github.com:NthPortal/v.git",
        "scm:git:git@github.com:NthPortal/v.git",
      )
    ),
  )
)

lazy val sharedSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.4" % Test
  ),
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-Xlint",
    "-Werror",
  ),
  scalacOptions ++= {
    if (isSnapshot.value) Nil
    else Seq("-opt:l:inline", "-opt-inline-from:lgbt.princess.v.**")
  },
)

lazy val core = project
  .in(file("core"))
  .settings(sharedSettings)
  .settings(
    name := "v-core",
    mimaPreviousArtifacts := Set().map(organization.value %% name.value % _),
  )
lazy val coreTest = core % "test->test"

lazy val semver = project
  .in(file("semver"))
  .dependsOn(
    core,
    coreTest,
  )
  .settings(sharedSettings)
  .settings(
    name := "v-semver",
    mimaPreviousArtifacts := Set().map(organization.value %% name.value % _),
  )

lazy val root = project
  .in(file("."))
  .aggregate(
    core,
    semver,
  )
  .dependsOn(
    core,
    semver,
  )
  .settings(
    name := "v",
    mimaPreviousArtifacts := Set.empty,
    Compile / doc / sources :=
      (Compile / doc / sources).value ++
        (core / Compile / doc / sources).value ++
        (semver / Compile / doc / sources).value,
  )
