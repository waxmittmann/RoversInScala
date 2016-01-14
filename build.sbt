name := """hello-scala"""

version := "1.0"

scalaVersion := "2.11.7"

//libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

//libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.7" % "test")
libraryDependencies ++= Seq("org.specs2" %% "specs2" % "3.7" % "test")
libraryDependencies += "org.mockito" % "mockito-all" % "1.9.5"
libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "2.3.13" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")

fork in run := true
