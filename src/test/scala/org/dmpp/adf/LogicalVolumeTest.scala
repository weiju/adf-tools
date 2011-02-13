package org.dmpp.adf

/**
 * Test cases for logical volumes.
 */
import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._

class LogicalVolumeTest extends JUnit4(LogicalVolumeSpec)
object LogicalVolumeSpecRunner extends ConsoleRunner(LogicalVolumeSpec)

object LogicalVolumeSpec extends Specification {

  var adfFile: AdfFile = null
  var logicalVolume: LogicalVolume = null

  "LogicalVolume" should {

    doBefore {
      var workbenchFile: InputStream = null
      try {
        workbenchFile = getClass.getResourceAsStream("/wbench1.3.adf")
        adfFile = AdfFileFactory.readAdfFile(workbenchFile)
        logicalVolume = new LogicalVolume(adfFile)
      } finally {
        if (workbenchFile != null) workbenchFile.close
      }
    }
    "be of file system type OFS, no dir cache and not international" in {
      logicalVolume.bootBlock.fileType must_== "OFS"
      logicalVolume.bootBlock.isInternational must beFalse
      logicalVolume.bootBlock.useDirCache must beFalse
    }
    "boot block should have a checksum" in {
      logicalVolume.bootBlock.storedChecksum must_== 0x2325e2fd
    }
    "boot block compute a valid checksum" in {
      logicalVolume.bootBlock.computedChecksum must_==
        logicalVolume.bootBlock.storedChecksum
    }
    "root block number in boot block must be 880" in {
      logicalVolume.bootBlock.rootBlockNumber must_== 880
    }
    "have standard values in root block" in {
      logicalVolume.rootBlock.blockType must_== 2
      logicalVolume.rootBlock.headerKey must_== 0
      logicalVolume.rootBlock.highSeq   must_== 0
      logicalVolume.rootBlock.hashtableSize must_== 0x48 // = 72
      logicalVolume.rootBlock.firstData must_== 0
      logicalVolume.rootBlock.storedChecksum must_== 0xf1dd3b47
    }
    "disk has workbench 1.3 name" in {
      logicalVolume.rootBlock.diskName must_== "Workbench1.3"
    }
    "compute root block checksum" in {
      logicalVolume.rootBlock.computedChecksum must_==
        logicalVolume.rootBlock.storedChecksum
    }

  }
}
