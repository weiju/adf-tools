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
import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import org.dmpp.adf.physical._

class DirectoryBlockTest extends JUnit4(DirectoryBlockSpec)
object DirectoryBlockSpecRunner extends ConsoleRunner(DirectoryBlockSpec)

object DirectoryBlockSpec extends Specification {

  var physicalVolume: PhysicalVolume = null
  var logicalVolume: LogicalVolume = null
  def dirBlock      = logicalVolume.rootBlock

  "DirectoryBlock" should {
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

    "return 0 for non-existing file name" in {
      dirBlock.blockNumberForName("notexisting") must_== 0
    }

    "have hash table that contains system entries" in {
      dirBlock.hashtableEntries.length must_== 24
      dirBlock.hashtableEntries.find(e => e.name == "c") must_!= None
      dirBlock.hashtableEntries.find(e => e.name == "System") must_!= None
    }

    "return valid block numbers for valid file names" in {
      dirBlock.blockNumberForName("System") must_== 881
      dirBlock.blockNumberForName("System.info") must_== 1289
      dirBlock.blockNumberForName("Empty") must_== 1281
    }

    "add a directory in a filled slot" in {
      val oldEntryCount = dirBlock.hashtableEntries.length
      // places itself in front of "Utilities"
      addDirectory("myname")
      dirBlock.hashtableEntries.find(e => e.name == "myname") must_!= None
      dirBlock.hashtableEntries.length must_== oldEntryCount + 1
      dirBlock.checksumIsValid must beTrue
      recent(dirBlock.lastModificationTime) must beTrue
    }

    "remove a directory just added" in {
      val oldEntryCount = dirBlock.hashtableEntries.length
      addDirectory("myname")
      dirBlock.removeFromHashtable("myname")

      dirBlock.hashtableEntries.find(e => e.name == "myname") must_== None
      dirBlock.hashtableEntries.length must_== oldEntryCount
      dirBlock.checksumIsValid must beTrue
      recent(dirBlock.lastModificationTime) must beTrue
    }

    "remove an entry in the middle of the chain" in {
      val oldEntryCount = dirBlock.hashtableEntries.length
      addDirectory("myname")
      dirBlock.removeFromHashtable("Utilities")

      dirBlock.hashtableEntries.find(e => e.name == "Utilities") must_== None
      dirBlock.hashtableEntries.length must_== oldEntryCount
      dirBlock.checksumIsValid must beTrue
      recent(dirBlock.lastModificationTime) must beTrue
    }
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
