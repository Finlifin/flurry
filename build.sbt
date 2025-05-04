val scala3Version = "3.6.4"

// 添加Kotlin插件和相关配置
enablePlugins(KotlinPlugin)

lazy val kotlinVersion = "2.1.20"

lazy val root = project.in(file(".")).settings(
  name := "flurry",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := scala3Version,
  libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
  libraryDependencies += "org.jetbrains.kotlin" % "kotlin-stdlib" % kotlinVersion
)
