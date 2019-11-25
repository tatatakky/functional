val defaultSetting = Seq(
  organization := "com.github.tatatakky",
  scalaVersion := "2.12.8"
)


lazy val root = (project in file("."))
  .settings(
    name := "functional-programming",
    defaultSetting,
    libraryDependencies ++= Seq(
      "org.scalatest"   %% "scalatest" % "3.0.7" % "test",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % "test" //testing
    )
  )

scalacOptions += "-feature"