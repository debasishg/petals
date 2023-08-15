ThisBuild / scalaVersion := "3.3.0"
ThisBuild / version := "0.0.1"
ThisBuild / organization := "dev.petals"
ThisBuild / organizationName := "petals"

lazy val trees = (project in file("./modules"))
  .settings(
    name := "Trees",
    scalafmtOnCompile := true,
  )