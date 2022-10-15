ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "2048",
    scalacOptions := Seq(
      "-feature" // Emit warning and location for usages of features that should be imported explicitly.
    )
  )
