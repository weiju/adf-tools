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

import java.util.Date
import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

class RootBlockTest extends JUnit4(RootBlockSpec)
object RootBlockSpecRunner extends ConsoleRunner(RootBlockSpec)

object RootBlockSpec extends Specification {
  var emptyVolume : LogicalVolume = null
  def rootBlock = emptyVolume.rootBlock

  "RootBlock" should {
    doBefore {
      emptyVolume = LogicalVolumeFactory.createEmptyDoubleDensityDisk("TestDisk")
    }

    "be inititialized with meaningful values" in {
      rootBlock.primaryType     must_== BlockType.PtShort
      rootBlock.secondaryType   must_== BlockType.StRoot
      rootBlock.headerKey       must_== 0
      rootBlock.highSeq         must_== 0
      rootBlock.hashtableSize   must_== 0x48 // = 72
      rootBlock.firstData       must_== 0
      rootBlock.bitmapIsValid   must beTrue
      rootBlock.name            must_== "TestDisk"
      rootBlock.storedChecksum  must_== rootBlock.computedChecksum
      rootBlock.checksumIsValid must beTrue
      recent(rootBlock.lastModificationTime)     must beTrue
      recent(rootBlock.diskLastModificationTime) must beTrue
      recent(rootBlock.creationTime)             must beTrue
    }

    "root block can be renamed" in {
      rootBlock.name = "NewDisk"
      rootBlock.name must_== "NewDisk"
      rootBlock.checksumIsValid must beTrue
      recent(rootBlock.lastModificationTime) must beTrue
    }

    "update disk modification time" in {
      val previousTime = rootBlock.diskLastModificationTime      
      Thread.sleep(500) // waste some time
      rootBlock.updateDiskLastModificationTime
      (rootBlock.diskLastModificationTime.getTime - previousTime.getTime) must
        beGreaterThan(0l)
      rootBlock.checksumIsValid must beTrue
    }

    "throw an error when renaming with too long name" in {
      (rootBlock.name = "this is a ridiculously long disk name with error") must throwA[IllegalArgumentException]
    }

  }

  def recent(date: Date): Boolean = {
    System.currentTimeMillis - date.getTime < 500l
  }
}
