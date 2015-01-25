organization  := "com.monolito"

version       := "0.1"

scalaVersion  := "2.10.4"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV = "2.2.4"
  val sprayV = "1.2.1"
  val specs2V = "2.4.9"
  Seq(
    "io.spray"            %  "spray-can"      % sprayV,
    "io.spray"            %  "spray-routing"  % sprayV,
    "io.spray"            %  "spray-testkit"  % sprayV  % "test",
    "io.spray"            %%  "spray-json"     % "1.2.6",
    "com.typesafe.akka"   %%  "akka-actor"     % akkaV,
    "com.typesafe.akka"   %%  "akka-testkit"   % akkaV   % "test",
    "org.specs2"          %%  "specs2-core"    % specs2V % "test",
    "org.specs2"          %%  "specs2-junit"   % specs2V % "test",
    "org.specs2"          %%  "specs2-mock"    % specs2V % "test",
    "org.mockito"         %   "mockito-all"    % "1.9.5" % "test",
    "org.scalaz"          %%  "scalaz-core"    % "7.1.0",
    "com.roundeights"     %% "hasher"          % "1.0.0",
    "com.sksamuel.elastic4s" %% "elastic4s"    % "1.4.0",
    "ch.qos.logback"      %  "logback-classic" % "1.1.1",
    "org.bouncycastle"    %  "bcprov-jdk16"    % "1.46"
  )
}

Revolver.settings

resolvers ++= Seq(
//    "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
    "RoundEights" at "http://maven.spikemark.net/roundeights"
)

lazy val root = (project in file(".")).enablePlugins(SbtTwirl)
