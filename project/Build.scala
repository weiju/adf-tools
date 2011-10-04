import sbt._
import Keys._

object MyBuild extends Build {

  override lazy val settings = super.settings ++ buildSettings

  def buildSettings = Seq(
    organization := "org.dmpp",
    version := "1.0",
    scalaVersion := "2.9.1",
    javacOptions in Compile ++= Seq("-target", "6", "-source", "6")
  )

  lazy val root = Project("root", file(".")) aggregate(gui)
  lazy val gui = Project("gui", file("adf-gui")) settings(testDependencies :_*) dependsOn(core, infolib)
  lazy val infolib = Project("infolib", file("infolib")) dependsOn(core)
  lazy val core = Project("core", file("adf-core"))

  def testDependencies = libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "1.6.1",
    "junit" % "junit" % "4.9")
}

