/**
 * Created on February 16, 2011
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

import org.dmpp.adf.physical._

/**
 * Test cases for bitmap blocks.
 */
@RunWith(classOf[JUnitRunner])
class BitmapBlockSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  val NumBitsPerBitmapBlock = (DoubleDensityDisk.BytesPerSector - 4) * 8
  var bitmapBlock : BitmapBlock = null

  override def beforeEach {
    val emptyVolume = LogicalVolumeFactory.createEmptyDoubleDensityDisk("TestDisk")
    bitmapBlock = new BitmapBlock(emptyVolume, 881)
    bitmapBlock.initialize
  }
  "BitmapBlock" should "initialize a bitmap block" in {
    bitmapBlock.checksumIsValid         should be (true)
    bitmapBlock.freeBlockIndexes.length should be === (NumBitsPerBitmapBlock)
    bitmapBlock.usedBlockIndexes        should be (Nil)
  }
  it should "allocate a block" in {
    val oldChecksum = bitmapBlock.storedChecksum
    bitmapBlock.allocate(0)

    bitmapBlock.freeBlockIndexes.length should be === (NumBitsPerBitmapBlock - 1)
    bitmapBlock.usedBlockIndexes        should be === (List(0))
    bitmapBlock.isAllocated(0)          should be (true)
    bitmapBlock.isFree(0)               should be (false)
    bitmapBlock.storedChecksum          should not be === (oldChecksum)
    bitmapBlock.checksumIsValid         should be (true)
  }
  it should "allocate another block" in {
    bitmapBlock.allocate(880)
    bitmapBlock.isAllocated(880) should be (true)
    bitmapBlock.isFree(880)      should be (false)
    bitmapBlock.checksumIsValid  should be (true)
  }
  it should "prevent allocating the same block twice" in {
    bitmapBlock.allocate(878)
    evaluating { bitmapBlock.allocate(878) } should produce [BlockAlreadyAllocated]
  }
  it should "free an allocated block" in {
    bitmapBlock.allocate(878)
    val oldChecksum = bitmapBlock.storedChecksum
    bitmapBlock.free(878)
    bitmapBlock.isFree(878)     should be (true)
    bitmapBlock.storedChecksum  should not be === (oldChecksum)
    bitmapBlock.checksumIsValid should be (true)
  }
  it should "have block 0 and 3 marked" in {
    val sector = bitmapBlock.physicalVolume.sector(881)
    sector.setInt32At(4, 0x6fffffff)
    bitmapBlock.isAllocated(31) should be (true)
    bitmapBlock.isFree(31)      should be (false)
    bitmapBlock.isAllocated(28) should be (true)
    bitmapBlock.isFree(28)      should be (false)
  }
}
