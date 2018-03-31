lazy val settings = Seq(
  scalaVersion := "2.11.8",
  version := "0.0.1"
)

lazy val dependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.specs2" %% "specs2" % "3.7" % "test",
  "org.mockito" % "mockito-all" % "1.10.5" % "test"
)

lazy val dsaScala = (project in file(".")).
  settings {
    settings ++ Seq(
      name := "dsa-scala",
      libraryDependencies ++= dependencies,
      publishTo := None,
      publish := None,
      publishLocal := None
    )
  }