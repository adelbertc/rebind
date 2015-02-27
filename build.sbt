organization in ThisBuild := "com.adelbertc"

version in ThisBuild := "0.1.0-SNAPSHOT"

licenses in ThisBuild += ("BSD New", url("http://opensource.org/licenses/BSD-3-Clause"))

scalaVersion in ThisBuild := "2.11.5"

crossScalaVersions in ThisBuild := List("2.10.4", scalaVersion.value)

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xlog-reflective-calls",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

lazy val core = project.in(file("core"))

lazy val example = project.in(file("example")).dependsOn(core)
