/**
 * Created on February 23, 2011
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
package org.dmpp.adf.app

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._
import java.util.Date
import org.dmpp.adf.app._

/**
 * Test cases for user volumes.
 */
class FileWriteTest extends JUnit4(FileWriteSpec)
object FileWriteSpecRunner extends ConsoleRunner(FileWriteSpec)

object FileWriteSpec extends Specification {
  "UserVolume" should {
    var emptyDiskFFS: UserVolume = null
    var emptyDiskOFS: UserVolume = null

    doBefore {
      emptyDiskFFS = UserVolumeFactory.createEmptyDoubleDensityDisk()
      emptyDiskOFS = UserVolumeFactory.createEmptyDoubleDensityDisk("OFSEmpty", "OFS")
    }

    "create an FFS file" in {
      val data: Array[Byte] = Array(0xde.asInstanceOf[Byte],
                                    0xad.asInstanceOf[Byte],
                                    0xbe.asInstanceOf[Byte],
                                    0xef.asInstanceOf[Byte])
      emptyDiskFFS.rootDirectory.createFile("steak", data)

      emptyDiskFFS.logicalVolume.rootBlock.hashtableSize must_== 0x48
      emptyDiskFFS.logicalVolume.rootBlock.bitmapIsValid must beTrue

      val file = emptyDiskFFS.rootDirectory.find("steak").get.asInstanceOf[UserFile]
      file.fileHeaderBlock.storedChecksum must_== file.fileHeaderBlock.computedChecksum
      file.fileHeaderBlock.fileSize must_== 4
      file.fileHeaderBlock.parent must_== 880

      val resultData = file.dataBytes
      resultData.length must_== 4

      resultData(0) & 0xff must_== 0xde
      resultData(1) & 0xff must_== 0xad
      resultData(2) & 0xff must_== 0xbe
      resultData(3) & 0xff must_== 0xef
    }

    "create an FFS file that spans two blocks" in {
      val data = new Array[Byte](1024)
      for (i <- 0 to 1020 by 4) {
        data(i)     = 0xde.asInstanceOf[Byte]
        data(i + 1) = 0xad.asInstanceOf[Byte]
        data(i + 2) = 0xbe.asInstanceOf[Byte]
        data(i + 3) = 0xef.asInstanceOf[Byte]
      }
      emptyDiskFFS.rootDirectory.createFile("steak", data)
      val file = emptyDiskFFS.rootDirectory.find("steak").get.asInstanceOf[UserFile]
      file.fileHeaderBlock.storedChecksum must_== file.fileHeaderBlock.computedChecksum
      file.fileHeaderBlock.fileSize must_== 1024

      val resultData = file.dataBytes

      resultData.length must_== 1024
      for (i <- 0 to 1020 by 4) {
        resultData(i + 0) & 0xff must_== 0xde
        resultData(i + 1) & 0xff must_== 0xad
        resultData(i + 2) & 0xff must_== 0xbe
        resultData(i + 3) & 0xff must_== 0xef
      }
    }

    "create an OFS file" in {
      val data: Array[Byte] = Array(0xde.asInstanceOf[Byte],
                                    0xad.asInstanceOf[Byte],
                                    0xbe.asInstanceOf[Byte],
                                    0xef.asInstanceOf[Byte])
      emptyDiskOFS.rootDirectory.createFile("steak", data)
      val file = emptyDiskOFS.rootDirectory.find("steak").get.asInstanceOf[UserFile]
      file.fileHeaderBlock.storedChecksum must_== file.fileHeaderBlock.computedChecksum
      file.fileHeaderBlock.fileSize must_== 4

      val resultData = file.dataBytes
      resultData.length must_== 4

      resultData(0) & 0xff must_== 0xde
      resultData(1) & 0xff must_== 0xad
      resultData(2) & 0xff must_== 0xbe
      resultData(3) & 0xff must_== 0xef
    }

    "create an OFS file that spans three blocks" in {
      val data = new Array[Byte](1024)
      for (i <- 0 to 1020 by 4) {
        data(i)     = 0xde.asInstanceOf[Byte]
        data(i + 1) = 0xad.asInstanceOf[Byte]
        data(i + 2) = 0xbe.asInstanceOf[Byte]
        data(i + 3) = 0xef.asInstanceOf[Byte]
      }
      emptyDiskOFS.rootDirectory.createFile("steak", data)
      val file = emptyDiskOFS.rootDirectory.find("steak").get.asInstanceOf[UserFile]
      file.fileHeaderBlock.storedChecksum must_== file.fileHeaderBlock.computedChecksum
      file.fileHeaderBlock.fileSize must_== 1024

      val resultData = file.dataBytes

      resultData.length must_== 1024
      for (i <- 0 to 1020 by 4) {
        resultData(i + 0) & 0xff must_== 0xde
        resultData(i + 1) & 0xff must_== 0xad
        resultData(i + 2) & 0xff must_== 0xbe
        resultData(i + 3) & 0xff must_== 0xef
      }
      val rootblock = emptyDiskOFS.logicalVolume.rootBlock
      rootblock.computedChecksum must_== rootblock.storedChecksum
    }
  }
  def formatted(date: Date) = {
    val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dateFormat.format(date)
  }
}
