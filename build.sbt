lazy val commons = Seq(
  scalaVersion := "2.13.3",
  crossScalaVersions := Seq("2.11.12", "2.12.11", "2.13.3"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.0"))

lazy val core = (project in file("core")).settings(
  commons,
  name := "scala-cli"
)

lazy val macros = (project in file("macros")).settings(
  commons,
  name := "scala-cli-derive",
  libraryDependencies += ("org.scala-lang" % "scala-reflect" % scalaVersion.value)
)

lazy val colorful = (project in file("colorful")).settings(
  commons,
  name := "scala-cli-colorful",
  libraryDependencies += ("org.fusesource.jansi" % "jansi" % "1.18")
)

lazy val root = (project in file("."))
  .aggregate(core, macros, colorful)
  .settings(
    commons,
    name := "scala-cli-root",
    crossScalaVersions := Nil,
    publish / skip := true,
    publishArtifact := false,
    aggregate in update := false)

