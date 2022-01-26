ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "cz.fit.cvut"
ThisBuild / scalaVersion := "2.13.7"

val MonixVersion = "3.2.2"
val CatsVersion = "2.1.1"
val CatsEffectVersion = "2.1.4"
val SimulacrumVersion = "1.0.0"
val MacroParadiseVersion = "2.1.1"
val ScalaTestVersion = "3.2.0"
val ScalaTestPlusVersion = "3.2.0.0"
val ScalaCheckVersion = "1.14.3"
val KindProjectorVersion = "0.11.0"
val BetterMonadicForVersion = "0.3.1"
val SilencerVersion = "1.7.0"
val fs2Version = "2.5.10"

lazy val root = (project in file("."))
  .settings(
    name := "simulovane-ochlazovani",
    idePackagePrefix := Some("cz.fit.cvut")
  )

//libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1"
//libraryDependencies += "io.monix" %% "monix-catnap" % "3.4.0"
libraryDependencies ++= Seq(
  "io.monix" %% "monix" % MonixVersion,
  "org.typelevel" %% "simulacrum" % SimulacrumVersion % Provided,
  "org.typelevel" %% "cats-core" % CatsVersion,
  "org.typelevel" %% "cats-effect" % CatsEffectVersion,
  // For testing
  "org.scalatest" %% "scalatest" % ScalaTestVersion % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % ScalaTestPlusVersion % Test,
  "org.scalacheck" %% "scalacheck" % ScalaCheckVersion % Test,
  "org.typelevel" %% "cats-laws" % CatsVersion % Test,
  "org.typelevel" %% "cats-effect-laws" % CatsEffectVersion % Test,
  "co.fs2" %% "fs2-core" % fs2Version,
  "co.fs2" %% "fs2-io" % fs2Version,
  "co.fs2" %% "fs2-reactive-streams" % fs2Version
)

lazy val app = (project in file("app"))
  .settings(
    assembly / mainClass := Some("cz.fit.cvut.App"),
    assembly / assemblyJarName := "simulated-annealing.jar"
  )
exportJars := true
