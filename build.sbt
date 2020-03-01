import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val commonSettings = Seq(
  organization := "ru.primetalk",
  version := "0.1.0-SNAPSHOT", //
  scalaVersion := "2.13.1",
  scalacOptions ++= Seq(
    "-Ymacro-annotations",// required for simulacrum starting from Scala 2.13+
    "-deprecation",
    "-feature",
    "-language:higherKinds"
  ),
  publishArtifact in Test := false,
//  libraryDependencies += "org.specs2" %% "specs2-core" % "4.7.0" % Test,
//  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  scalacOptions in Test ++= Seq("-Yrangepos")
)

// TODO: use sbt-crossproject plugin
lazy val environment =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("environment")).settings(
  commonSettings,
  name := "environment"
)

lazy val funtik = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("funtik")).settings(
  commonSettings,
  name := "funtik",
).dependsOn(environment)

lazy val funtikScaffolding = (crossProject(JSPlatform).crossType(CrossType.Pure) in file("funtik-scaffolding")).settings(
  commonSettings,
  name := "funtik-scaffolding"
).jsSettings(
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "1.0.0",
    "org.specs2" %%% "specs2-core" % "4.8.3" % Test,
    "org.specs2" %%% "specs2-scalacheck" % "4.8.3" % Test
  ),
  scalaJSUseMainModuleInitializer := true,
  mainClass := Some("ru.primetalk.funtik.environment.Funtik")
).dependsOn(environment, funtik)

lazy val root = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file(".")).
  aggregate(environment, funtik, funtikScaffolding).
  settings(
    aggregate in update := false,
    publishArtifact := false
  )

pomExtra in Global := {
    <url>https://github.com/Primetalk/Funtik</url>
    <licenses>
      <license>
        <name>MIT Software License</name>
        <url>https://github.com/Primetalk/Funtik/blob/master/LICENSE</url>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:github.com/Primetalk/Funtik</connection>
      <developerConnection>scm:git:git@github.com:Primetalk/Funtik</developerConnection>
      <url>github.com/Primetalk/Funtik</url>
    </scm>
    <developers>
      <developer>
        <id>zhizhelev</id>
        <name>Arseniy Zhizhelev</name>
        <url>zhizhelev@primetalk.ru</url>
      </developer>
      <developer>
        <id>sd1ver</id>
        <name>Sergey Kazantsev</name>
        <url></url>
      </developer>
    </developers>
}
