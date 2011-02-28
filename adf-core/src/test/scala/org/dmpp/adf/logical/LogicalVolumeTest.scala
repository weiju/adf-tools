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

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._
import java.util.Date
import java.text.SimpleDateFormat
import org.dmpp.adf.physical._

/**
 * Test cases for logical volumes.
 */

class LogicalVolumeTest extends JUnit4(LogicalVolumeSpec)
object LogicalVolumeSpecRunner extends ConsoleRunner(LogicalVolumeSpec)

object LogicalVolumeSpec extends Specification {

  var emptyOFS: LogicalVolume = null
  var emptyFFS: LogicalVolume = null

  "LogicalVolume" should {

    doBefore {
      emptyFFS = LogicalVolumeFactory.createEmptyDoubleDensityDisk("Empty", "FFS")
      emptyOFS = LogicalVolumeFactory.createEmptyDoubleDensityDisk("Empty", "OFS")
    }
    "be empty initialized" in {
      checkForValidBootBlock(emptyFFS)
      emptyFFS.numUsedBlocks must_== 2
      emptyFFS.allocate(880) must throwA[BlockAlreadyAllocated]
      emptyFFS.allocate(881) must throwA[BlockAlreadyAllocated]
      emptyFFS.name must_== "Empty"
      emptyFFS.bootBlock.filesystemType must_== "FFS"
    }
    def checkForValidBootBlock(volume: LogicalVolume) {
      volume.sizeInBytes must_== DoubleDensityDisk.ImageSize
      volume(0).asInstanceOf[Char] must_== 'D'
      volume(1).asInstanceOf[Char] must_== 'O'
      volume(2).asInstanceOf[Char] must_== 'S'
      for (i <- 4 until 1024) volume(i) must_== 0
    }

    // bitmap blocks
    "have valid bitmap blocks" in {
      emptyFFS.rootBlock.bitmapBlocks.length must_== 1
      val bitmapBlock = emptyFFS.rootBlock.bitmapBlocks.head
      bitmapBlock.blockNumber must_== 881
      bitmapBlock.checksumIsValid must beTrue
      emptyFFS.numFreeBlocks must_== 1756
      emptyFFS.numUsedBlocks must_== 2
    }

    "allocate a block on an empty disk" in {
      emptyFFS.allocate must_== 882
    }
    "allocate a block where the blocks after 880 are all full" in {
      for (block <- 882 until 1760) emptyFFS.allocate(block)
      emptyFFS.allocate must_== 2
    }
    "allocate a block on a full volume" in {
      val full = emptyFFS
      for (block <- 882 until 1760) full.allocate(block)
      for (block <- 2 until 880) full.allocate(block)
      full.allocate must throwA[DeviceIsFull]
    }
    "allocate block 885 on an empty disk" in {
      emptyFFS.numFreeBlocks must_== 1756
      emptyFFS.allocate(882)
      emptyFFS.allocate(883)
      emptyFFS.allocate(884)
      emptyFFS.allocate(885)
      emptyFFS.numFreeBlocks must_== 1752
    }
    "create a user directory block" in {
      val rootBlock = emptyFFS.rootBlock
      val dirblock = emptyFFS.createUserDirectoryBlockIn(rootBlock, "mydir")
      dirblock.primaryType   must_== BlockType.PtShort
      dirblock.isDirectory must beTrue
      dirblock.headerKey must_== 882
      dirblock.parent must_== 880
      dirblock.name must_== "mydir"
      dirblock.checksumIsValid must beTrue
      recent(dirblock.lastModificationTime) must beTrue

      rootBlock.checksumIsValid must beTrue
      rootBlock.blockNumberForName("mydir") must_== 882
      recent(rootBlock.lastModificationTime) must beTrue
    }

    "allocates an FFS data block" in {
      val header = emptyFFS.createFileHeaderBlockIn(emptyFFS.rootBlock, "myfile")
      val block = emptyFFS.allocateDataBlock(header, 1, 42)
      block.maxDataBytes must_== 512
      block.isOFS must beFalse
      block.isFFS must beTrue
    }
    "allocates an OFS data block" in {
      val header = emptyFFS.createFileHeaderBlockIn(emptyFFS.rootBlock, "myfile")
      val block = emptyOFS.allocateDataBlock(header, 1, 42)
      block.maxDataBytes must_== (512 - 24)
      block.isOFS must beTrue
      block.isFFS must beFalse
      val ofsblock = block.asInstanceOf[OfsDataBlock]
      ofsblock.primaryType must_== BlockType.PtData
      ofsblock.headerKey must_== 882
      ofsblock.seqNum must_== 1
      ofsblock.dataSize must_== 42
    }

    "renames a non-existing directory" in {
      val dirblock = emptyFFS.createUserDirectoryBlockIn(emptyFFS.rootBlock, "mydir")
      emptyFFS.renameDirectoryEntryIn(emptyFFS.rootBlock, "nonexisting", "foo") must
        throwA[DirectoryEntryNotFound]
    }
    "renames an existing directory" in {
      val dirblock = emptyFFS.createUserDirectoryBlockIn(emptyFFS.rootBlock, "mydir")
      val dirblock2 =
        emptyFFS.renameDirectoryEntryIn(emptyFFS.rootBlock, "mydir", "mydir2")
      dirblock2.name must_== "mydir2"
      emptyFFS.rootBlock.blockForName("mydir") must_== None
      emptyFFS.rootBlock.blockForName("mydir2") must_!= None
      recent(emptyFFS.rootBlock.lastModificationTime) must beTrue
      recent(emptyFFS.rootBlock.diskLastModificationTime) must beTrue
      dirblock2.checksumIsValid must beTrue
      emptyFFS.rootBlock.checksumIsValid must beTrue
      emptyFFS.allocate(dirblock.blockNumber) must throwA[BlockAlreadyAllocated]
    }
   }
  def formatted(date: Date) = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dateFormat.format(date)
  }
  def recent(date: Date) = {
    (System.currentTimeMillis - date.getTime) < 500l
  }
}
