import sbt._
import Keys._

object ChordsBuild extends Build {
  val buildId = "chords"
  val buildSettings = Seq(
    organization := "org.kirhgoff",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.7"
  )

  lazy val chords = Project(
    id = buildId,
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      resolvers ++= projectResolvers,
      libraryDependencies ++= projectDependencies,
      scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
      javacOptions ++= Seq("-Xlint:unchecked", "-encoding", "UTF-8")
    )
  )

  override lazy val settings = super.settings ++ buildSettings

  val projectResolvers = Seq(
    "Local Maven Repository" at "http://eif-repository.moex.com/nexus/content/repositories/releases",
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Liftmodules repo" at "https://repository-liftmodules.forge.cloudbees.com/release",
    "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
    "Apache Releases" at "https://repository.apache.org/content/repositories/releases/",
    "Nexus Snapshots" at "https://maven.mni.thm.de/content/groups/public/"
  )

  lazy val projectDependencies = Seq(
//    "ch.qos.logback" % "logback-classic" % "1.1.3",
//    "org.specs2" %% "specs2" % "3.3.1" % "test"
//    "org.mockito" % "mockito-core" % "1.10.19" % "test",
//    "org.scalamock" %% "scalamock-specs2-support" % "3.0.1" % "test",
//    "org.scalaz" %% "scalaz-core" % "7.0.2"
      "org.scalatest" % "scalatest_2.11" % "3.0.0-M14"
  )


}
