/**
 * Created on February 19, 2011
 * Copyright (c) 2011, Wei-ju Wu
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of Wei-ju Wu nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY WEI-JU WU ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL WEI-JU WU BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.dmpp.adf.logical

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._
import java.util.Date
import java.text.SimpleDateFormat
import org.dmpp.adf.physical._

/**
 * Test cases for files and directories.
 */
class DirectoryEntryTest extends JUnit4(DirectoryEntrySpec)
object DirectoryEntrySpecRunner extends ConsoleRunner(DirectoryEntrySpec)

object DirectoryEntrySpec extends Specification {

  var physicalVolume: PhysicalVolume = null
  var logicalVolume: LogicalVolume = null

  "LogicalVolume" should {

    doBefore {
      var workbenchFile: InputStream = null
      try {
        workbenchFile = getClass.getResourceAsStream("/wbench1.3.adf")
        physicalVolume = PhysicalVolumeFactory.readDoubleDensityDisk(workbenchFile)
        logicalVolume = new LogicalVolume(physicalVolume)
      } finally {
        if (workbenchFile != null) workbenchFile.close
      }
    }

    "System dir is a directory" in {
      val sysdir = logicalVolume.rootBlock.blockForName("System").get
      sysdir.isDirectory must beTrue
      sysdir.isFile must beFalse
    }
    "Disk.info should return gid and uid" in {
      val diskInfo = logicalVolume.rootBlock.blockForName("Disk.info").get
      diskInfo.isDirectory must beFalse
      diskInfo.isFile must beTrue
    }

    // access rights
    "System dir should return gid and uid" in {
      val sysdir = logicalVolume.rootBlock.blockForName("System").get
      sysdir.uid must_== 0
      sysdir.gid must_== 0
    }
    "Disk.info should return gid and uid" in {
      val diskInfo = logicalVolume.rootBlock.blockForName("Disk.info").get
      diskInfo.uid must_== 0
      diskInfo.gid must_== 0
    }
    "Disk.info should have protection flags" in {
      val diskInfo = logicalVolume.rootBlock.blockForName("Disk.info").get
      diskInfo.flags.canDelete  must beTrue
      diskInfo.flags.canExecute must beTrue
      diskInfo.flags.canWrite   must beTrue
      diskInfo.flags.canRead    must beTrue
      diskInfo.flags.isArchived must beFalse
      diskInfo.flags.isPure     must beFalse
      diskInfo.flags.isScript   must beFalse
      diskInfo.flags.hold       must beFalse
    }

    "System dir has root block as parent" in {
      val sysdir = logicalVolume.rootBlock.blockForName("System").get
      sysdir.asInstanceOf[UserDirectoryBlock].parentBlock must_== 880
    }
    "System dir lastAccessTime is supported" in {
      val sysdir = logicalVolume.rootBlock.blockForName("System").get
      formatted(sysdir.asInstanceOf[UserDirectoryBlock].lastAccessTime) must_==
        "1989-08-16 13:02:13.320"
    }

    // Files
    "Disk.info has file size" in {
      val diskInfo = logicalVolume.rootBlock.blockForName("Disk.info").get
      diskInfo.asInstanceOf[FileHeaderBlock].fileSize must_== 370
    }
    "Disk.info has data blocks" in {
      val diskInfo = logicalVolume.rootBlock.blockForName("Disk.info").get
      diskInfo.asInstanceOf[FileHeaderBlock].dataBlocks must_== List(1285)
    }
    "File lastModificationTime is supported" in {
      val sysdir = logicalVolume.rootBlock.blockForName("Disk.info").get
      formatted(sysdir.asInstanceOf[FileHeaderBlock].lastModificationTime) must_==
        "1988-08-13 19:11:55.800"
    }
    "File lastAccessTime is throws UnsupportedOperationException" in {
      val sysdir = logicalVolume.rootBlock.blockForName("Disk.info").get
      sysdir.asInstanceOf[FileHeaderBlock].lastAccessTime must
        throwA[UnsupportedOperationException]
    }
  }

  def formatted(date: Date) = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dateFormat.format(date)
  }
}
