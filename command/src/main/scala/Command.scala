package org.dmpp.adf.command

import scopt.OptionParser
import org.dmpp.adf.app._
import java.io.{File, FileOutputStream}

case class Config(command: String="dir", verbose: Boolean=false, files: Seq[File]=Seq())

object Main {

  def executeCommand(config: Config) {
    config.command match {
      case "create" =>
        val volume = UserVolumeFactory.createEmptyDoubleDensityDisk()
        val fos = new FileOutputStream(config.files(0))
        try {
          volume.writeToOutputStream(fos)
        } finally {
          fos.close()
        }
      case "dir" =>
        val volume = UserVolumeFactory.readFromFile(config.files(0))
        val dir = volume.rootDirectory
        if (dir.list.isEmpty) {
          printf("Directory '%s' is empty\n", dir.name)
        } else {
          val dirs = dir.list.filter(_.isDirectory)
          val files = dir.list.filter(!_.isDirectory)
          dirs.sortWith(_.name < _.name).map(f => printf("(DIR) %s\n", f))
          files.sortWith(_.name < _.name).map(println)
        }
      case _ =>
        println("ignore")
    }
  }

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("adfcmd") {
      head("adfcmd", "1.0")
      opt[Unit]("verbose") abbr("v") action { (_, c) =>
        c.copy(verbose=true)
      } text("verbose mode")
      help("help") abbr("h") text("Prints this usage text")
      cmd("dir") action { (_, c) =>
        c.copy(command="dir")
      } text("list directory contents") children(
        arg[File]("<file>") minOccurs(1) maxOccurs(1) action { (x, c) =>
          c.copy(files= c.files :+ x) } text("ADF input file")
      )
      cmd("create") action { (_, c) =>
        c.copy(command="create")
      } text("create blank ADF") children(
        arg[File]("<file>") minOccurs(1) maxOccurs(1) action { (x, c) =>
          c.copy(files= c.files :+ x) } text("output file")
      )
    }
    parser.parse(args, Config()) match {
      case Some(config) =>
        executeCommand(config)
      case _ =>
        println("")
    }
  }
}
