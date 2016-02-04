name := "scala-invoke"
description := "Dynamic invocation of functions, methods and constructors in Scala with compile-time parameter validation."
homepage := Some(url("https://github.com/barnardb/scala-invoke"))

licenses := Seq("MIT Licence" -> new URL("http://opensource.org/licenses/MIT"))
scmInfo := Some(ScmInfo(
  browseUrl  = homepage.value.get,
  connection = "scm:git:git@github.com:barnardb/scala-invoke.git"
))

developers :=
  Developer("barnardb", "Ben Barnard", "barnardb@gmail.com", url("https://github.com/barnardb")) ::
  Nil



crossScalaVersions in Global := Seq("2.11.7", "2.12.0-M3")
scalaVersion in Global <<= (crossScalaVersions in Global)(_.head)

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" %% "scalatest" % (if (scalaVersion.value == "2.12.0-M3") "2.2.5-M3" else "2.2.6") % Test

conflictManager := ConflictManager.latestCompatible
dependencyOverrides += "org.scala-lang" % "scala-library" % scalaVersion.value
dependencyOverrides += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions := Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-explaintypes",
  "-feature",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import"
) ++ {
  if (scalaVersion.value == "2.12.0-M3") Nil
  else Seq(
    "-Yclosure-elim",
    "-Yconst-opt",
    "-Ydead-code"
  )
}
scalacOptions in Test := (scalacOptions in Test).value filterNot (_ == "-Ywarn-dead-code")

testOptions in Test += Tests.Argument("-oF")
