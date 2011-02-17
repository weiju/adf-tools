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

/**
 * Test cases for bitmap blocks.
 */
import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import org.dmpp.adf.physical._

class BitmapBlockTest extends JUnit4(BitmapBlockSpec)
object BitmapBlockSpecRunner extends ConsoleRunner(BitmapBlockSpec)

object BitmapBlockSpec extends Specification {

  var physicalVolume: PhysicalVolume = null
  var logicalVolume: LogicalVolume = null  
  val NumBitsPerBitmapBlock = (DoubleDensityDisk.BytesPerSector - 4) * 8

  "BitmapBlock" should {

    doBefore {
      physicalVolume = new DoubleDensityDisk(new Array[Byte](DoubleDensityDisk.ImageSize))
    }
    "create an uninitialized bitmap block" in {
      val bitmapBlock = new BitmapBlock(physicalVolume, 881)
      bitmapBlock.storedChecksum must_== 0
      bitmapBlock.freeBlockIndexes must_== Nil
      bitmapBlock.usedBlockIndexes.length must_== NumBitsPerBitmapBlock
    }
    "initialize a bitmap block" in {
      val bitmapBlock = new BitmapBlock(physicalVolume, 881)
      bitmapBlock.initialize
      bitmapBlock.storedChecksum must_!= 0
      bitmapBlock.freeBlockIndexes.length must_== NumBitsPerBitmapBlock
      bitmapBlock.usedBlockIndexes must_== Nil
    }
    "allocate a block" in {
      val bitmapBlock = new BitmapBlock(physicalVolume, 881)
      bitmapBlock.initialize
      val oldChecksum = bitmapBlock.storedChecksum
      bitmapBlock.allocate(0)
      bitmapBlock.storedChecksum must_!= oldChecksum
      bitmapBlock.freeBlockIndexes.length must_== NumBitsPerBitmapBlock - 1
      bitmapBlock.usedBlockIndexes must_== List(0)
      bitmapBlock.isAllocated(0) must beTrue
      bitmapBlock.isFree(0) must beFalse
    }
    "allocate another block" in {
      val bitmapBlock = new BitmapBlock(physicalVolume, 881)
      bitmapBlock.initialize
      bitmapBlock.allocate(880)
      bitmapBlock.isAllocated(880) must beTrue
      bitmapBlock.isFree(880) must beFalse
    }
    "prevent allocating the same block twice" in {
      val bitmapBlock = new BitmapBlock(physicalVolume, 881)
      bitmapBlock.initialize
      bitmapBlock.allocate(878)
      bitmapBlock.allocate(878) must throwA[BlockAlreadyAllocated]
    }
    "frees an allocated block" in {
      val bitmapBlock = new BitmapBlock(physicalVolume, 881)
      bitmapBlock.initialize
      bitmapBlock.allocate(878)
      val oldChecksum = bitmapBlock.storedChecksum
      bitmapBlock.free(878)
      bitmapBlock.isFree(878) must beTrue
      bitmapBlock.storedChecksum must_!= oldChecksum
    }
  }
}
