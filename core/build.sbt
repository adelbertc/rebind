name := "rebind-core"

organization := "com.adelbertc"

resolvers in ThisBuild += "bintray/non" at "http://dl.bintray.com/non/maven"

addCompilerPlugin("org.spire-math" % "kind-projector_2.11" % "0.5.2")

val scalazVersion = "7.1.1"

val specs2Version = "2.4.15"

libraryDependencies ++= Seq(
  "org.scalaz"      %% "scalaz-core"                % scalazVersion,
  "org.scalacheck"  %% "scalacheck"                 % "1.12.2"          % "test",
  "org.scalaz"      %% "scalaz-scalacheck-binding"  % scalazVersion     % "test",
  "org.specs2"      %% "specs2-core"                % specs2Version     % "test",
  "org.specs2"      %% "specs2-scalacheck"          % specs2Version     % "test"
)
