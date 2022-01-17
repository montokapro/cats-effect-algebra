val scala2Version = "2.13.7"
val scala3Version = "3.0.2"
val scalaVersions = List(scala2Version, scala3Version)

val catsVersion = "2.7.0"
val catsEffectVersion = "3.3.0"
val scalaTestVersion = "3.2.10"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cats-effect-algebra",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    crossScalaVersions := scalaVersions,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "algebra" % catsVersion,
      "org.typelevel" %% "cats-effect" % catsEffectVersion,

      "org.scalactic" %% "scalactic" % scalaTestVersion % Test,
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test
    )
  )
