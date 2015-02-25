name := "rebind"

organization := "com.adelbertc"

licenses += ("BSD-3-Clause", url("http://opensource.org/licenses/BSD-3-Clause"))

scalaVersion in ThisBuild := "2.11.5"

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xlint",
  "-Xlog-reflective-calls",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-value-discard"
)

scalacOptions in compile ++= Seq(
  "-Xfatal-warnings",
  "-Ywarn-unused-import"
)

lazy val core = project.in(file("core"))

lazy val example = project.in(file("example")).dependsOn(core)
