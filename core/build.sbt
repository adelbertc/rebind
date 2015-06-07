name := "rebind-core"

organization := "com.adelbertc"

resolvers ++= Seq(
  "bintray/non"     at "http://dl.bintray.com/non/maven",
  "scalaz-bintray"  at "http://dl.bintray.com/scalaz/releases"
)

val scalazVersion = "7.1.2"

val specs2Version = "3.6.1"

libraryDependencies ++= Seq(
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4"),

  "org.scalaz"      %% "scalaz-core"                % scalazVersion,
  "org.scalacheck"  %% "scalacheck"                 % "1.12.2"          % "test",
  "org.scalaz"      %% "scalaz-scalacheck-binding"  % scalazVersion     % "test",
  "org.specs2"      %% "specs2-core"                % specs2Version     % "test",
  "org.specs2"      %% "specs2-scalacheck"          % specs2Version     % "test"
)

seq(bintraySettings:_*)
