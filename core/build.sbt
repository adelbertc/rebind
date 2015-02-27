name := "rebind-core"

organization := "com.adelbertc"

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

val scalazVersion = "7.1.1"

val specs2Version = "2.4.15"

libraryDependencies ++= Seq(
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.2"),

  "org.scalaz"      %% "scalaz-core"                % scalazVersion,
  "org.scalacheck"  %% "scalacheck"                 % "1.12.2"          % "test",
  "org.scalaz"      %% "scalaz-scalacheck-binding"  % scalazVersion     % "test",
  "org.specs2"      %% "specs2-core"                % specs2Version     % "test",
  "org.specs2"      %% "specs2-scalacheck"          % specs2Version     % "test"
)

seq(bintraySettings:_*)
