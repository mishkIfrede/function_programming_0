//ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.12"
ThisBuild / organization := "function_programming_0"

lazy val root = (project in file("."))
  .settings(
    name := "function_programming_0",
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.10.4", // преобразование Rule, Policy, Config, ConfigLoader в json
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    ),
    Compile / mainClass := Some("function_programming_0.Main")
  )