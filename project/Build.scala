import sbt._

object MyBuild extends Build {

  lazy val root = Project("root", file(".")) aggregate(gui)
  lazy val gui = Project("gui", file("adf-gui")) dependsOn(core, infolib)
  lazy val infolib = Project("infolib", file("infolib")) dependsOn(core)
  lazy val core = Project("core", file("adf-core"))

/*
  lazy val core = project("adf-core", "adf-core")
  lazy val infolib  = project("infolib",  "infolib", core)
  lazy val gui  = project("adf-gui",  "adf-gui", core, infolib)
  lazy val infoviewer  = project("infoviewer",  "infoviewer", infolib)
  */
}

