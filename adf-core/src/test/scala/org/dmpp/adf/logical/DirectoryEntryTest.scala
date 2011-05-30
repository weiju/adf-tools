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
 * Test cases for files and directories.
 */
@RunWith(classOf[JUnitRunner])
class DirectoryEntrySpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  var physicalVolume: PhysicalVolume = null
  var logicalVolume: LogicalVolume = null
  var emptyOFS: LogicalVolume = null
  var emptyFFS: LogicalVolume = null

  override def beforeEach {
    var workbenchFile: InputStream = null
    try {
      workbenchFile  = getClass.getResourceAsStream("/wbench1.3.adf")
      physicalVolume = PhysicalVolumeFactory.readDoubleDensityDisk(workbenchFile)
      logicalVolume  = new LogicalVolume(physicalVolume)        
    } finally {
      if (workbenchFile != null) workbenchFile.close
    }
    emptyFFS = LogicalVolumeFactory.createEmptyDoubleDensityDisk("Empty", "FFS")
    emptyOFS = LogicalVolumeFactory.createEmptyDoubleDensityDisk("Empty", "OFS")
  }

  "DirectoryEntry" should "System dir is a directory" in {
    val sysdir = logicalVolume.rootBlock.blockForName("System").get
    sysdir should be ('directory)
    sysdir should not be ('file)
    sysdir.headerKey should be === (881)
  }
  it should "ensure Disk.info is a file" in {
    val diskInfo = logicalVolume.rootBlock.blockForName("Disk.info").get
    diskInfo should not be ('directory)
    diskInfo should be ('file)
  }

  // access rights
  it should "ensure System dir returns gid and uid" in {
    val sysdir = logicalVolume.rootBlock.blockForName("System").get
    sysdir.uid should be === (0)
    sysdir.gid should be === (0)
  }
  it should "ensure Disk.info returns gid and uid" in {
    val diskInfo = logicalVolume.rootBlock.blockForName("Disk.info").get
    diskInfo.uid should be === (0)
    diskInfo.gid should be === (0)
  }
  it should "ensure Disk.info should have protection flags" in {
    val diskInfo = logicalVolume.rootBlock.blockForName("Disk.info").get
    diskInfo.flags.canDelete  should be (true)
    diskInfo.flags.canExecute should be (true)
    diskInfo.flags.canWrite   should be (true)
    diskInfo.flags.canRead    should be (true)
    diskInfo.flags.isArchived should be (false)
    diskInfo.flags.isPure     should be (false)
    diskInfo.flags.isScript   should be (false)
    diskInfo.flags.hold       should be (false)
  }

  it should "ensure System dir has root block as parent" in {
    val sysdir = logicalVolume.rootBlock.blockForName("System").get
    sysdir.asInstanceOf[UserDirectoryBlock].parent should be === (880)
  }
  it should "ensure System dir lastAccessTime is supported" in {
    val sysdir = logicalVolume.rootBlock.blockForName("System").get
    formatted(sysdir.asInstanceOf[UserDirectoryBlock].lastModificationTime) should be ===
      ("1989-08-16 13:02:13.320")
  }

  // Files
  it should "ensure Disk.info has file size" in {
    val diskInfo = logicalVolume.rootBlock.blockForName("Disk.info").get
    diskInfo.asInstanceOf[FileHeaderBlock].fileSize should be === (370)
  }
  it should "ensure Disk.info has data blocks" in {
    val diskInfo = logicalVolume.rootBlock.blockForName("Disk.info").get
    diskInfo.asInstanceOf[FileHeaderBlock].dataBlockNumbers should be === (List(1285))
  }
  it should "ensure File lastModificationTime is supported" in {
    val file = logicalVolume.rootBlock.blockForName("Disk.info").get
    formatted(file.asInstanceOf[FileHeaderBlock].lastModificationTime) should be ===
      ("1988-08-13 19:11:55.800")
  }
  it should "support File rename" in {
    val file = logicalVolume.rootBlock.blockForName("Disk.info").get
    file.name = "newname"
    file.name should be === ("newname")
  }
  it should "should throw exception on File rename with too many characters" in {
    val file = logicalVolume.rootBlock.blockForName("Disk.info").get
    evaluating {
      (file.name = "a ridiculously long name that is too long")
    } should produce [IllegalArgumentException]
  }

  it should "support File comment change" in {
    val file = logicalVolume.rootBlock.blockForName("Disk.info").get
    file.comment = "newcomment"
    file.comment should be === ("newcomment")
  }
  it should "throw exception on File comment change with too many characters" in {
    val file = logicalVolume.rootBlock.blockForName("Disk.info").get
    evaluating {
      (file.comment = "a ridiculously long comment that is too long even for the long comment field which is 79 characters")
    } should produce [IllegalArgumentException]
  }

  it should "ensure Disk.info has 1 data block" in {
    val fileHeader = logicalVolume.rootBlock.blockForName("Disk.info").get.
    asInstanceOf[FileHeaderBlock]
    fileHeader.blockCount should be === (1)
  }
  it should "ensure Disk.info has root parent" in {
    val fileHeader = logicalVolume.rootBlock.blockForName("Disk.info").get.
    asInstanceOf[FileHeaderBlock]
    fileHeader.parent should be === (880)
  }
  it should "ensure Disk.info changes parent" in {
    val fileHeader = logicalVolume.rootBlock.blockForName("Disk.info").get.
    asInstanceOf[FileHeaderBlock]
    fileHeader.parent = 123
    fileHeader.parent should be === (123)
    fileHeader.checksumIsValid should be (true)
  }
  it should "ensure Disk.info changes nextInHashBucket" in {
    val fileHeader = logicalVolume.rootBlock.blockForName("Disk.info").get.
    asInstanceOf[FileHeaderBlock]
    fileHeader.nextInHashBucket = 123
    fileHeader.nextInHashBucket should be === (123)
    fileHeader.checksumIsValid  should be (true)
  }

  // general HeaderBlock data
  it should "change a header key" in {
    val sysdir = logicalVolume.rootBlock.blockForName("System").get
    sysdir.headerKey = 42
    sysdir.headerKey should be === (42)
  }
  it should "change a secondary type" in {
    val sysdir = logicalVolume.rootBlock.blockForName("System").get
    sysdir.secondaryType = 4711
    sysdir.secondaryType should be === (4711)
  }
  it should "change next in hash" in {
    val sysdir = logicalVolume.rootBlock.blockForName("System").get
    sysdir.nextInHashBucket = 1234
    sysdir.nextInHashBucket should be === (1234)
  }

  it should "update last modification time" in {
    val sysdir = logicalVolume.rootBlock.blockForName("System").get
    sysdir.updateLastModificationTime
    (System.currentTimeMillis - sysdir.lastModificationTime.getTime) should be <= (1000l)
  }

  it should "create a file header block" in {
    val rootBlock = emptyFFS.rootBlock
    val header = emptyFFS.createFileHeaderBlockIn(rootBlock, "myfile")
    header.primaryType   should be === (BlockType.PtShort)
    header.isFile        should be (true)
    header.headerKey     should be === (882)
    header.parent        should be === (880)
    header.name          should be ("myfile")
    header.checksumIsValid should be (true)
    recent(header.lastModificationTime) should be (true)

    rootBlock.checksumIsValid should be (true)
    rootBlock.blockNumberForName("myfile") should be === (882)
    recent(rootBlock.lastModificationTime) should be (true)
  }

  def formatted(date: Date) = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dateFormat.format(date)
  }
  def recent(date: Date) = {
    (System.currentTimeMillis - date.getTime) < 500l
  }
}
