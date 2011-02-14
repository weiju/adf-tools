/**
 * Created on February 12, 2011
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

/**
 * Test cases for logical volumes.
 */
import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._
import java.util.Date
import java.text.SimpleDateFormat
import org.dmpp.adf.physical._

class LogicalVolumeTest extends JUnit4(LogicalVolumeSpec)
object LogicalVolumeSpecRunner extends ConsoleRunner(LogicalVolumeSpec)

object LogicalVolumeSpec extends Specification {

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
    "be of file system type OFS, no dir cache and not international" in {
      logicalVolume.bootBlock.filesystemType must_== "OFS"
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
      logicalVolume.rootBlock.primaryType   must_== BlockType.PtShort
      logicalVolume.rootBlock.headerKey     must_== 0
      logicalVolume.rootBlock.highSeq       must_== 0
      logicalVolume.rootBlock.hashtableSize must_== 0x48 // = 72
      logicalVolume.rootBlock.firstData must_== 0
      logicalVolume.rootBlock.storedChecksum must_== 0xf1dd3b47
      logicalVolume.rootBlock.bitmapIsValid must beTrue
      formatted(logicalVolume.rootBlock.lastModifiedRoot) must_== "1989-08-17 18:21:31.480"
      formatted(logicalVolume.rootBlock.lastModifiedDisk) must_== "1996-10-30 21:24:38.340"
      formatted(logicalVolume.rootBlock.fsCreationTime) must_== "1989-08-16 13:57:36.100"
      logicalVolume.rootBlock.secondaryType must_== BlockType.StRoot
    }
    "disk has workbench 1.3 name" in {
      logicalVolume.rootBlock.name must_== "Workbench1.3"
    }
    "compute root block checksum" in {
      logicalVolume.rootBlock.computedChecksum must_==
        logicalVolume.rootBlock.storedChecksum
    }
    "have root hash table that contains system entries" in {
      logicalVolume.rootBlock.hashtableEntries.length must_== 23
      logicalVolume.rootBlock.hashtableEntries.find(e => e.name == "c") must_!= None
      logicalVolume.rootBlock.hashtableEntries.find(e => e.name == "System") must_!= None

      // debugging
      for (block <- logicalVolume.rootBlock.hashtableEntries) {
        printf("sector: %d, pr. type: %d, sec. type: %d, name: %s, next: %d\n",
               block.sectorNumber,
               block.primaryType,
               block.secondaryType,
               block.name,
               block.nextInHashBucket)
      }
    }
    "get bitmap blocks" in {
      logicalVolume.rootBlock.bitmapBlocks.length must_== 1
      logicalVolume.rootBlock.bitmapBlocks.head.sectorNumber must_== 1015
    }
    "bitmap block has a checksum" in {
      logicalVolume.rootBlock.bitmapBlocks.head.storedChecksum must_== 0xb462193c
    }
    "bitmap block computes a checksum" in {
      val bitmapBlock = logicalVolume.rootBlock.bitmapBlocks.head
      bitmapBlock.computedChecksum must_== bitmapBlock.storedChecksum
    }
  }
  def formatted(date: Date) = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dateFormat.format(date)
  }
}
