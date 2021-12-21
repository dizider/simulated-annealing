ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "cz.fit.cvut"
ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "simulovane-ochlazovani",
    idePackagePrefix := Some("cz.fit.cvut")
  )

libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1"


lazy val app = (project in file("app"))
  .settings(
    assembly / mainClass := Some("cz.fit.cvut.App"),
    assembly / assemblyJarName := "simulated-annealing.jar"
  )
exportJars := true
