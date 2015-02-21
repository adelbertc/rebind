name := "rebind"

organization := "com.adelbertc"

licenses += ("BSD-3-Clause", url("http://opensource.org/licenses/BSD-3-Clause"))

scalaVersion := "2.11.5"

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

val scalazVersion = "7.1.1"

libraryDependencies ++= Seq(
  "org.scalaz"  %% "scalaz-core"        % scalazVersion,
  "org.scalaz"  %% "scalaz-effect"      % scalazVersion,
  "org.specs2"  %% "specs2-core"        % "2.4.15"        % "test"
)

addCompilerPlugin("org.spire-math" % "kind-projector_2.11" % "0.5.2")

scalacOptions ++= Seq(
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
