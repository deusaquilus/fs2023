ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "org.deusaquilus"

lazy val `fs2023` =
  (project in file("."))
    .settings(
      resolvers ++= Seq(
        Resolver.mavenLocal,
        "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
        "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
      ),
      libraryDependencies ++= Seq(
        "dev.zio" %% "zio" % "2.0.2",
        // "dev.zio" %% "zio-direct" % "1.0.0-RC4"
        // "dev.zio" %% "zio-direct" % "1.0.0-RC4+25-bc408477+20230131-1846-SNAPSHOT",
        // "dev.zio" %% "zio-direct-streams" % "1.0.0-RC4+25-bc408477+20230131-1846-SNAPSHOT"
        // "dev.zio" %% "zio-direct" % "1.0.0-RC4+36-d925087a+20230131-2222-SNAPSHOT",
        // "dev.zio" %% "zio-direct-streams" % "1.0.0-RC4+36-d925087a+20230131-2222-SNAPSHOT"
        "dev.zio"     %% "zio-direct" % "1.0.0-RC7",
        "com.lihaoyi" %% "pprint"     % "0.8.1",
        "com.google.code.gson" % "gson" % "2.10.1",
        "dev.zio" %% "zio-json" % "0.6.2"
      )
    )
