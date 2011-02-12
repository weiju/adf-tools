import sbt._

class DmppProject(info: ProjectInfo) extends DefaultProject(info) {
  val junit4 = "junit" % "junit" % "4.8.2" % "test"
  val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5" % "test"
}
