organization  := "com.monolito"

name := "kiros-auth"

version       := "0.1"

scalaVersion  := "2.11.6"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV = "2.4.4"
  val specs2V = "2.4.16"
  Seq(
	"com.typesafe.akka" %% "akka-actor" % "2.3.11",
	"com.typesafe.akka" %% "akka-http-experimental" % akkaV,
	"com.typesafe.akka" %% "akka-http-spray-json-experimental" % akkaV,
	"com.typesafe.akka" %% "akka-stream" % akkaV,
	"com.typesafe.akka" %% "akka-stream-testkit" % akkaV,
	"com.typesafe.akka" %% "akka-testkit" % "2.3.11" % "test",
    "org.specs2"          %%  "specs2-core"    % specs2V % "test",
    "org.specs2"          %%  "specs2-junit"   % specs2V % "test",
    "org.specs2"          %%  "specs2-mock"    % specs2V % "test",
    "org.mockito"         %   "mockito-all"    % "1.9.5" % "test",
    "org.scalaz"          %%  "scalaz-core"    % "7.1.1",
    "com.roundeights"     %% "hasher"          % "1.0.0",
    "com.fasterxml.uuid" % "java-uuid-generator" % "3.1.4",
    "ch.qos.logback"      %  "logback-classic" % "1.1.1",
    "org.bouncycastle"    %  "bcprov-jdk16"    % "1.46",
    "com.typesafe"        %  "config"          % "1.2.1",
	"btomala" %% "akka-http-twirl" % "1.1.0"
  )
}

Revolver.settings

javaOptions in Revolver.reStart += "-Xmx64M"

resolvers ++= Seq(
    "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
    "Bartek's repo at Bintray" at "https://dl.bintray.com/btomala/maven",
    "RoundEights" at "http://maven.spikemark.net/roundeights"
)

mappings in (Compile, packageBin) ~= {_.filter (!_._1.getName.equals("application.conf"))}

lazy val core = RootProject(file("../kiros-commons"))
lazy val root = (project in file(".")).enablePlugins(SbtTwirl).dependsOn(core)

lazy val buildSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "com.monolito",
  scalaVersion := "2.11.2"
)

val app = (project in file("app")).
  settings(buildSettings: _*)

test in assembly := {} //skip test in assembly