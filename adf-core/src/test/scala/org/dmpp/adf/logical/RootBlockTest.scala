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
import org.scalatest.FlatSpec
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RootBlockSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {
  var emptyVolume : LogicalVolume = null
  def rootBlock = emptyVolume.rootBlock

  override def beforeEach {
    emptyVolume = LogicalVolumeFactory.createEmptyDoubleDensityDisk("TestDisk")
  }

  "RootBlock" should "be inititialized with meaningful values" in {
    rootBlock.primaryType                      should be (BlockType.PtShort)
    rootBlock.secondaryType                    should be (BlockType.StRoot)
    rootBlock.headerKey                        should be === (0)
    rootBlock.highSeq                          should be === (0)
    rootBlock.hashtableSize                    should be === (0x48) // = 72
    rootBlock.firstData                        should be === (0)
    rootBlock.bitmapIsValid                    should be (true)
    rootBlock.name                             should be ("TestDisk")
    rootBlock.storedChecksum                   should be === (rootBlock.computedChecksum)
    rootBlock.checksumIsValid                  should be (true)
    recent(rootBlock.lastModificationTime)     should be (true)
    recent(rootBlock.diskLastModificationTime) should be (true)
    recent(rootBlock.creationTime)             should be (true)
  }

  it should "rename the root block" in {
    rootBlock.name = "NewDisk"
    rootBlock.name                         should be ("NewDisk")
    rootBlock.checksumIsValid              should be (true)
    recent(rootBlock.lastModificationTime) should be (true)
  }

  it should "update disk modification time" in {
    val previousTime = rootBlock.diskLastModificationTime      
    Thread.sleep(500) // waste some time
    rootBlock.updateDiskLastModificationTime
    (rootBlock.diskLastModificationTime.getTime - previousTime.getTime) should be > (0l)
    rootBlock.checksumIsValid should be (true)
  }

  it should "throw an error when renaming with too long name" in {
    evaluating {
      (rootBlock.name = "this is a ridiculously long disk name with error")
    } should produce [IllegalArgumentException]
  }

  def recent(date: Date): Boolean = {
    System.currentTimeMillis - date.getTime < 500l
  }
}
