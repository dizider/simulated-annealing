ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "simulovane-ochlazovani",
    idePackagePrefix := Some("cz.fit.cvut")
  )

libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"

