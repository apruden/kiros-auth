organization  := "com.monolito"

version       := "0.1"

scalaVersion  := "2.11.5"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV = "2.3.9"
  val sprayV = "1.3.2"
  val specs2V = "2.4.16"
  Seq(
    "io.spray"            %%  "spray-can"      % sprayV,
    "io.spray"            %%  "spray-routing"  % sprayV,
    "io.spray"            %%  "spray-testkit"  % sprayV  % "test",
    "io.spray"            %%  "spray-json"     % "1.3.1",
    "com.typesafe.akka"   %%  "akka-actor"     % akkaV,
    "com.typesafe.akka"   %%  "akka-testkit"   % akkaV   % "test",
    "org.specs2"          %%  "specs2-core"    % specs2V % "test",
    "org.specs2"          %%  "specs2-junit"   % specs2V % "test",
    "org.specs2"          %%  "specs2-mock"    % specs2V % "test",
    "org.mockito"         %   "mockito-all"    % "1.9.5" % "test",
    "org.scalaz"          %%  "scalaz-core"    % "7.1.1",
    "com.roundeights"     %% "hasher"          % "1.0.0",
    "com.sksamuel.elastic4s" %% "elastic4s"    % "1.4.11",
    "org.elasticsearch" % "elasticsearch-groovy"    % "1.4.2",
    "org.apache.lucene" % "lucene-expressions" % "4.10.2",
    "ch.qos.logback"      %  "logback-classic" % "1.1.1",
    "org.bouncycastle"    %  "bcprov-jdk16"    % "1.46",
    "com.typesafe"        %  "config"          % "1.2.1"
  )
}

Revolver.settings

javaOptions in Revolver.reStart += "-Xmx64M"

resolvers ++= Seq(
    "spray repo" at "http://repo.spray.io",
    "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
    "RoundEights" at "http://maven.spikemark.net/roundeights"
)

lazy val root = (project in file(".")).enablePlugins(SbtTwirl)

lazy val buildSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "com.monolito",
  scalaVersion := "2.11.2"
)

val app = (project in file("app")).
  settings(buildSettings: _*)

test in assembly := {} //skip test in assembly
