name := """politevo"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws
)

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "2.1.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "mysql" % "mysql-connector-java" % "5.1.36",
  "org.mindrot" % "jbcrypt" % "0.3m",
  "com.typesafe.play" %% "play-mailer" % "3.0.1",
  "org.scalaj" %% "scalaj-http" % "2.3.0",
  "com.amazonaws" % "aws-java-sdk" % "1.11.0",
  "org.twitter4j" % "twitter4j-stream" % "3.0.3", 
  "org.twitter4j"% "twitter4j-core"% "4.0.1",
  "org.jsoup" % "jsoup" % "1.10.1",
  cache
)
