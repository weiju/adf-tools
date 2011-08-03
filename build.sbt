name         := "adf-tools"

version      := "1.0"

organization := "org.dmpp"

scalaVersion := "2.9.0-1"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

mainClass in (Compile, run) := Some("org.dmpp.adf.gui.Main")
