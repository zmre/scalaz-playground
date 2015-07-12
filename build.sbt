scalaVersion := "2.11.7"
version := "1.0"


resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/release/",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

// Production
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.2", // for type awesomeness
  "org.scalaz" %% "scalaz-concurrent" % "7.1.2", // for type awesomeness
  "org.scalaz.stream" %% "scalaz-stream" % "0.7.1a" // for streaming stuff
)

// Test
libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "org.typelevel" %% "scalaz-scalatest" % "0.2.2" % "test"
)

scalariformSettings

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8", // yes, this is 2 args
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-language:existentials",
  "-language:higherKinds"
)




