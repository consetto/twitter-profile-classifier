name := "twitter-profile-classifier"

organization := "com.consetto"

scalaVersion := "2.11.8"

val javacppVersion = "1.2"

classpathTypes += "maven-plugin"

lazy val platform = org.bytedeco.javacpp.Loader.getPlatform

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.0",
  "org.twitter4j" % "twitter4j-core" % "4.0.4",
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3",
  "org.bytedeco" % "javacpp" % javacppVersion,
  "org.bytedeco" % "javacv" % javacppVersion,
  "org.bytedeco.javacpp-presets" % "flandmark" % ("1.07-" + javacppVersion) classifier "",
  "org.bytedeco.javacpp-presets" % "flandmark" % ("1.07-" + javacppVersion) classifier platform,
  "org.bytedeco.javacpp-presets" % "opencv" % ("3.1.0-" + javacppVersion) classifier "",
  "org.bytedeco.javacpp-presets" % "opencv" % ("3.1.0-" + javacppVersion) classifier platform,
  "org.json" % "json" % "20160212"
)

fork in run := true
