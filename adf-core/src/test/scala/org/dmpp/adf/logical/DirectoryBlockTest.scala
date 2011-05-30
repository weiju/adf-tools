/**
 * Created on February 26, 2011
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

import java.io._
import java.util.Date

import org.scalatest.FlatSpec
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.dmpp.adf.physical._

@RunWith(classOf[JUnitRunner])
class DirectoryBlockSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  var physicalVolume: PhysicalVolume = null
  var logicalVolume: LogicalVolume = null
  def dirBlock      = logicalVolume.rootBlock

  override def beforeEach {
    var workbenchFile: InputStream = null
    try {
      workbenchFile = getClass.getResourceAsStream("/wbench1.3.adf")
      physicalVolume = PhysicalVolumeFactory.readDoubleDensityDisk(workbenchFile)
      logicalVolume = new LogicalVolume(physicalVolume)
    } finally {
      if (workbenchFile != null) workbenchFile.close
    }
  }

  "DirectoryBlock" should "return 0 for non-existing file name" in {
    dirBlock.blockNumberForName("notexisting") should be === (0)
  }

  it should "have hash table that contains system entries" in {
    dirBlock.hashtableEntries.length                        should be === (24)
    dirBlock.hashtableEntries.find(e => e.name == "c")      should not be (None)
    dirBlock.hashtableEntries.find(e => e.name == "System") should not be (None)
  }
  it should "return valid block numbers for valid file names" in {
    dirBlock.blockNumberForName("System")      should be === (881)
    dirBlock.blockNumberForName("System.info") should be === (1289)
    dirBlock.blockNumberForName("Empty")       should be === (1281)
  }
  it should "add a directory in a filled slot" in {
    val oldEntryCount = dirBlock.hashtableEntries.length
    // places itself in front of "Utilities"
    addDirectory("myname")
    dirBlock.hashtableEntries.find(e => e.name == "myname") should not be (None)
    dirBlock.hashtableEntries.length                        should be === (oldEntryCount + 1)
    dirBlock.checksumIsValid                                should be (true)
    recent(dirBlock.lastModificationTime)                   should be (true)
  }
  it should "remove a directory just added" in {
    val oldEntryCount = dirBlock.hashtableEntries.length
    addDirectory("myname")
    dirBlock.removeFromHashtable("myname")

    dirBlock.hashtableEntries.find(e => e.name == "myname") should be (None)
    dirBlock.hashtableEntries.length                        should be === (oldEntryCount)
    dirBlock.checksumIsValid                                should be (true)
    recent(dirBlock.lastModificationTime)                   should be (true)
  }
  it should "remove a non-existing entry" in {
    val oldEntryCount = dirBlock.hashtableEntries.length
    dirBlock.removeFromHashtable("doesnotexist")

    dirBlock.hashtableEntries.length should be === (oldEntryCount)
    dirBlock.checksumIsValid         should be (true)
  }

  it should "remove an entry in the middle of the chain" in {
    val oldEntryCount = dirBlock.hashtableEntries.length
    addDirectory("myname")
    dirBlock.removeFromHashtable("Utilities")

    dirBlock.hashtableEntries.find(e => e.name == "Utilities") should be (None)
    dirBlock.hashtableEntries.length                           should be === (oldEntryCount)
    dirBlock.checksumIsValid                                   should be (true)
    recent(dirBlock.lastModificationTime)                      should be (true)
  }

  private def addDirectory(name: String) {
    val newdir = new UserDirectoryBlock(logicalVolume, logicalVolume.allocate)
    newdir.initialize(880, name)
    dirBlock.addToHashtable(newdir)
  }

  def recent(date: Date): Boolean = {
    System.currentTimeMillis - date.getTime < 500l
  }
}
