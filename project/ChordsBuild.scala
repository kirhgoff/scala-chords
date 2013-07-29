import sbt._
import Keys._

object ChordsBuild extends Build {
  val buildId = "chords"
  val buildSettings = Seq(
    organization := "org.kirhgoff",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.1"
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
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Liftmodules repo" at "https://repository-liftmodules.forge.cloudbees.com/release",
    "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
    "Apache Releases" at "https://repository.apache.org/content/repositories/releases/",
    "Nexus Snapshots" at "https://maven.mni.thm.de/content/groups/public/"
  )

  lazy val projectDependencies = Seq(
    "ch.qos.logback" % "logback-classic" % "1.0.6",
    "org.specs2" %% "specs2" % "1.13" % "test",
    "org.mockito" % "mockito-core" % "1.9.5" % "test",
    "org.scalamock" %% "scalamock-specs2-support" % "3.0.1" % "test"
  )


}
