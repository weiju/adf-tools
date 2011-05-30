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

import org.scalatest.FlatSpec
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.io._
import java.util.Date
import java.text.SimpleDateFormat
import org.dmpp.adf.physical._

/**
 * Test cases for logical volumes.
 */
@RunWith(classOf[JUnitRunner])
class LogicalVolumeSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  var emptyOFS: LogicalVolume = null
  var emptyFFS: LogicalVolume = null

  override def beforeEach {
    emptyFFS = LogicalVolumeFactory.createEmptyDoubleDensityDisk("Empty", "FFS")
    emptyOFS = LogicalVolumeFactory.createEmptyDoubleDensityDisk("Empty", "OFS")
  }

  "LogicalVolume" should "be empty initialized" in {
    checkForValidBootBlock(emptyFFS)
    emptyFFS.numUsedBlocks should be === (2)
    evaluating { emptyFFS.allocate(880) } should produce [BlockAlreadyAllocated]
    evaluating { emptyFFS.allocate(881) } should produce [BlockAlreadyAllocated]
    emptyFFS.name should be ("Empty")
    emptyFFS.bootBlock.filesystemType should be ("FFS")
  }
  def checkForValidBootBlock(volume: LogicalVolume) {
    volume.sizeInBytes           should be === (DoubleDensityDisk.ImageSize)
    volume(0).asInstanceOf[Char] should be === ('D')
    volume(1).asInstanceOf[Char] should be === ('O')
    volume(2).asInstanceOf[Char] should be === ('S')
    for (i <- 4 until 1024) volume(i) should be === (0)
  }

  // bitmap blocks
  it should "have valid bitmap blocks" in {
    emptyFFS.rootBlock.bitmapBlocks.length should be === (1)
    val bitmapBlock = emptyFFS.rootBlock.bitmapBlocks.head
    bitmapBlock.blockNumber should be === (881)
    bitmapBlock.checksumIsValid should be (true)
    emptyFFS.numFreeBlocks should be === (1756)
    emptyFFS.numUsedBlocks should be === (2)
  }

  it should "allocate a block on an empty disk" in {
    emptyFFS.allocate should be === (882)
  }
  it should "allocate a block where the blocks after 880 are all full" in {
    for (block <- 882 until 1760) emptyFFS.allocate(block)
    emptyFFS.allocate should be === (2)
  }
  it should "allocate a block on a full volume" in {
    val full = emptyFFS
    for (block <- 882 until 1760) full.allocate(block)
    for (block <- 2 until 880) full.allocate(block)
    evaluating { full.allocate } should produce [DeviceIsFull]
  }
  it should "allocate block 885 on an empty disk" in {
    emptyFFS.numFreeBlocks should be === (1756)
    emptyFFS.allocate(882)
    emptyFFS.allocate(883)
    emptyFFS.allocate(884)
    emptyFFS.allocate(885)
    emptyFFS.numFreeBlocks should be === (1752)
  }
  it should "create a user directory block" in {
    val rootBlock = emptyFFS.rootBlock
    val dirblock = emptyFFS.createUserDirectoryBlockIn(rootBlock, "mydir")
    dirblock.primaryType   should be === (BlockType.PtShort)
    dirblock.isDirectory   should be (true)
    dirblock.headerKey     should be === (882)
    dirblock.parent        should be === (880)
    dirblock.name          should be ("mydir")
    dirblock.checksumIsValid should be (true)
    recent(dirblock.lastModificationTime) should be (true)
    
    rootBlock.checksumIsValid should be (true)
    rootBlock.blockNumberForName("mydir") should be === (882)
    recent(rootBlock.lastModificationTime) should be (true)
  }
  it should "create two nested directories" in {
    val numFreeBlocks0 = emptyFFS.numFreeBlocks
    val dirblock1 = emptyFFS.createUserDirectoryBlockIn(emptyFFS.rootBlock, "dir1")
    val dirblock2 = emptyFFS.createUserDirectoryBlockIn(dirblock1, "dir2")

    (numFreeBlocks0 - emptyFFS.numFreeBlocks) should be === (2)
    emptyFFS.isAllocated(dirblock1.blockNumber) should be (true)
    emptyFFS.isAllocated(dirblock2.blockNumber) should be (true)
    dirblock1.blockForName("dir2") should not be (None)
  }

  it should "allocate an FFS data block" in {
    val header = emptyFFS.createFileHeaderBlockIn(emptyFFS.rootBlock, "myfile")
    val block = emptyFFS.allocateDataBlock(header, 1, 42)
    block.maxDataBytes should be === (512)
    block.isOFS should be (false)
    block.isFFS should be (true)
  }
  it should "allocate an OFS data block" in {
    val header = emptyFFS.createFileHeaderBlockIn(emptyFFS.rootBlock, "myfile")
    val block = emptyOFS.allocateDataBlock(header, 1, 42)
    block.maxDataBytes should be === (512 - 24)
    block.isOFS should be (true)
    block.isFFS should be (false)
    val ofsblock = block.asInstanceOf[OfsDataBlock]
    ofsblock.primaryType should be (BlockType.PtData)
    ofsblock.headerKey   should be === (882)
    ofsblock.seqNum      should be === (1)
    ofsblock.dataSize    should be === (42)
  }

  it should "rename a non-existing directory" in {
    val dirblock = emptyFFS.createUserDirectoryBlockIn(emptyFFS.rootBlock, "mydir")
    evaluating {
      emptyFFS.renameDirectoryEntryIn(emptyFFS.rootBlock, "nonexisting", "foo")
    } should produce [DirectoryEntryNotFound]
  }
  it should "rename an existing directory" in {
    val dirblock = emptyFFS.createUserDirectoryBlockIn(emptyFFS.rootBlock, "mydir")
    val dirblock2 =
      emptyFFS.renameDirectoryEntryIn(emptyFFS.rootBlock, "mydir", "mydir2")
    dirblock2.name should be ("mydir2")
    emptyFFS.rootBlock.blockForName("mydir")  should be (None)
    emptyFFS.rootBlock.blockForName("mydir2") should not be (None)
    dirblock2.checksumIsValid should be (true)
    evaluating { emptyFFS.allocate(dirblock.blockNumber) } should produce [BlockAlreadyAllocated]
    validRootBlock(emptyFFS.rootBlock) should be (true)
  }
  it should "remove a non-existing directory" in {
    evaluating {
      emptyFFS.removeDirectoryEntryFrom(emptyFFS.rootBlock, "nonexist")
    } should produce [DirectoryEntryNotFound]
  }
  it should "remove an existing empty directory" in {
    val dirblock = emptyFFS.createUserDirectoryBlockIn(emptyFFS.rootBlock, "mydir")
    val oldNumFreeBlocks = emptyFFS.numFreeBlocks
    emptyFFS.removeDirectoryEntryFrom(emptyFFS.rootBlock, "mydir")
    emptyFFS.rootBlock.blockForName("mydir") should be (None)
    (emptyFFS.numFreeBlocks == oldNumFreeBlocks + 1) should be (true)
    emptyFFS.isAllocated(dirblock.blockNumber) should be (false)
    validRootBlock(emptyFFS.rootBlock) should be (true)
  }
  it should "remove an existing file" in {
    val numFreeBlocks0 = emptyFFS.numFreeBlocks
    val fileHeader = emptyFFS.createFileHeaderBlockIn(emptyFFS.rootBlock, "myfile")
    val dataBlock = emptyFFS.allocateDataBlock(fileHeader, 1, 356)
    fileHeader.setDataBlocks(356, List(dataBlock))
    emptyFFS.removeDirectoryEntryFrom(emptyFFS.rootBlock, "myfile")

    emptyFFS.rootBlock.blockForName("myfile") should be (None)
    emptyFFS.numFreeBlocks should be === (numFreeBlocks0)
    emptyFFS.isAllocated(fileHeader.blockNumber) should be (false)
    emptyFFS.isAllocated(dataBlock.blockNumber)  should be (false)
    validRootBlock(emptyFFS.rootBlock)           should be (true)
  }

  it should "remove two nested directories" in {
    val numFreeBlocks0 = emptyFFS.numFreeBlocks
    val dirblock1 = emptyFFS.createUserDirectoryBlockIn(emptyFFS.rootBlock, "dir1")
    val dirblock2 = emptyFFS.createUserDirectoryBlockIn(dirblock1, "dir2")
    emptyFFS.removeDirectoryEntryFrom(emptyFFS.rootBlock, "dir1")

    emptyFFS.numFreeBlocks should be === (numFreeBlocks0)
    emptyFFS.isAllocated(dirblock1.blockNumber) should be (false)
    emptyFFS.isAllocated(dirblock2.blockNumber) should be (false)
    emptyFFS.rootBlock.blockForName("dir1")     should be (None)    
    validRootBlock(emptyFFS.rootBlock)          should be (true)
  }

  it should "move a directory to a different one" in {
    val rootBlock = emptyFFS.rootBlock
    val destdir = emptyFFS.createUserDirectoryBlockIn(rootBlock, "destdir")
    val dirblock = emptyFFS.createUserDirectoryBlockIn(rootBlock, "dir")
    emptyFFS.moveDirectoryEntryTo(dirblock, destdir)

    rootBlock.blockForName("dir") should be (None)
    destdir.blockForName("dir")   should not be (None)
    validDirectoryBlock(destdir)  should be (true)
    validDirectoryBlock(dirblock) should be (true)
    validRootBlock(emptyFFS.rootBlock) should be (true)
  }

  def validDirectoryBlock(directoryBlock: UserDirectoryBlock): Boolean = {
    directoryBlock.checksumIsValid && recent(directoryBlock.lastModificationTime)
  }

  def validRootBlock(rootBlock: RootBlock): Boolean = {
    rootBlock.checksumIsValid &&
    recent(rootBlock.lastModificationTime) &&
    recent(rootBlock.diskLastModificationTime)
  }

  def formatted(date: Date) = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dateFormat.format(date)
  }
  def recent(date: Date) = {
    (System.currentTimeMillis - date.getTime) < 500l
  }
}
