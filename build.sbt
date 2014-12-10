organization  := "com.example"

version       := "0.1"

scalaVersion  := "2.11.2"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV = "2.3.6"
  val sprayV = "1.3.1"
  Seq(
    "io.spray"            %%  "spray-can"     % sprayV,
    "io.spray"            %%  "spray-routing" % sprayV,
    "io.spray"            %%  "spray-testkit" % sprayV  % "test",
    "io.spray"            %%  "spray-json"    % sprayV,
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
    "org.specs2"          %%  "specs2-core"   % "2.3.11" % "test",
    "org.reactivemongo"   %%  "reactivemongo" % "0.10.5.0.akka23",
    "ch.qos.logback"      %  "logback-classic" % "1.1.1",
	"org.scalaz"          %% "scalaz-core" % "7.1.0"
  )
}

Revolver.settings

resolvers ++= Seq(
    "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"
)
