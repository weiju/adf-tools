/**
 * Created on February 17, 2011
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

import org.scalatest.FlatSpec
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.io._
import java.util.Date
import org.dmpp.adf.app._

/**
 * Test cases for user volumes.
 */
@RunWith(classOf[JUnitRunner])
class UserVolumeSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  val workbenchFile = new File(getClass.getResource("/wbench1.3.adf").getFile)
  var emptyDisk: UserVolume = null
  var emptyDiskOFS: UserVolume = null
  var workbenchDisk: UserVolume = null

  override def beforeEach {
    workbenchDisk = UserVolumeFactory.readFromFile(workbenchFile)
    emptyDisk = UserVolumeFactory.createEmptyDoubleDensityDisk()
    emptyDiskOFS = UserVolumeFactory.createEmptyDoubleDensityDisk("OFSEmpty", "OFS")
  }

  "UserVolume" should "new disk must be valid dos disk" in {
    val empty = UserVolumeFactory.createEmptyDoubleDensityDisk()
    empty should be ('valid)
  }
  it should "ensure a  new disk has a recent creation date" in {
    val empty = UserVolumeFactory.createEmptyDoubleDensityDisk()
    recent(empty.creationTime) should be (true)
    recent(empty.rootDirectory.lastModificationTime) should be (true)
    recent(empty.lastModificationTime) should be (true)
  }
  it should "ensure workbench disk is valid dos disk" in {
    workbenchDisk should be ('valid)
  }
  it should "read workbench root directory" in {
    workbenchDisk.name                    should be ("Workbench1.3")
    workbenchDisk.toString                should be ("Workbench1.3")
    formatted(workbenchDisk.creationTime) should be ("1989-08-16 13:57:36.100")
    val rootdir = workbenchDisk.rootDirectory
    rootdir                               should be ('directory)
    rootdir.name                          should be ("Workbench1.3")
    rootdir.list.length                   should equal (24)
  }
  it should "find Disk.info in root directory" in {
    workbenchDisk.rootDirectory.find("Disk.info").get should be ('file)
  }
  it should "ensure Disk.info has file size" in {
    val diskInfo = workbenchDisk.rootDirectory.find("Disk.info").get
    diskInfo.asInstanceOf[UserFile].size should be === (370)
  }
  it should "get Disk.info data bytes" in {
    val diskInfo = workbenchDisk.rootDirectory.find("Disk.info").get
    val dataBytes = diskInfo.asInstanceOf[UserFile].dataBytes
    dataBytes.length should be === (370)
  }
  it should "not find a file in root directory" in {
    workbenchDisk.rootDirectory.find("notfound") should be (None)
  }
  it should "find a directory root directory" in {
    val utilDir = workbenchDisk.rootDirectory.find("Utilities").get
    utilDir should be ('directory)
  }
  it should "rename a file" in {
    val diskInfo = workbenchDisk.rootDirectory.find("Disk.info").get.asInstanceOf[UserFile]
    diskInfo.name = "NewDisk.info"
    diskInfo.name                            should be ("NewDisk.info")
    diskInfo.fileHeaderBlock.checksumIsValid should be (true)
    recent(diskInfo.lastModificationTime)    should be (true)
  }
  it should "rename a directory" in {
    val dir = workbenchDisk.rootDirectory.find("Utilities").get.asInstanceOf[UserDirectory]
    dir.name = "Utils"
    dir.name                               should be ("Utils")
    dir.thisDirectoryBlock.checksumIsValid should be (true)
    workbenchDisk.logicalVolume.rootBlock.checksumIsValid should be (true)
    recent(dir.lastModificationTime)       should be (true)
  }
  it should "rename a disk" in {
    workbenchDisk.name = "MyBench"
    workbenchDisk.name                                    should be ("MyBench")
    recent(workbenchDisk.lastModificationTime)            should be (true)
    workbenchDisk.logicalVolume.rootBlock.checksumIsValid should be (true)
  }
  it should "create a directory" in {
    val numFreeBlocks0 = emptyDisk.numFreeBlocks
    val rootdir = emptyDisk.rootDirectory
    val newdir = rootdir.createDirectory("mydir")

    newdir.name                                           should be ("mydir")
    recent(newdir.lastModificationTime)                   should be (true)
    newdir.thisDirectoryBlock.checksumIsValid             should be (true)
    emptyDisk.numFreeBlocks                               should be === (numFreeBlocks0 - 1)
    recent(rootdir.lastModificationTime)                  should be (true)
    recent(emptyDisk.lastModificationTime)                should be (true)
    workbenchDisk.logicalVolume.rootBlock.checksumIsValid should be (true)
  }
  def formatted(date: Date) = {
    val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dateFormat.format(date)
  }
  def recent(date: Date) = {
    (System.currentTimeMillis - date.getTime) < 500l
  }
}
