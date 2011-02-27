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

  var empty: LogicalVolume = null

  "LogicalVolume" should {

    doBefore {
      empty = LogicalVolumeFactory.createEmptyDoubleDensityDisk()
    }
    "be empty initialized" in {
      checkForValidBootBlock(empty)
      empty.numUsedBlocks must_== 2
      empty.allocate(880) must throwA[BlockAlreadyAllocated]
      empty.allocate(881) must throwA[BlockAlreadyAllocated]
      empty.name must_== "Empty"
      empty.bootBlock.filesystemType must_== "FFS"
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
      empty.rootBlock.bitmapBlocks.length must_== 1
      val bitmapBlock = empty.rootBlock.bitmapBlocks.head
      bitmapBlock.blockNumber must_== 881
      bitmapBlock.checksumIsValid must beTrue
      empty.numFreeBlocks must_== 1756
      empty.numUsedBlocks must_== 2
    }

    "allocate a block on an empty disk" in {
      empty.allocate must_== 882
    }
    "allocate a block where the blocks after 880 are all full" in {
      for (block <- 882 until 1760) empty.allocate(block)
      empty.allocate must_== 2
    }
    "allocate a block on a full volume" in {
      val full = empty
      for (block <- 882 until 1760) full.allocate(block)
      for (block <- 2 until 880) full.allocate(block)
      full.allocate must throwA[DeviceIsFull]
    }
    "allocate block 885 on an empty disk" in {
      empty.numFreeBlocks must_== 1756
      empty.allocate(882)
      empty.allocate(883)
      empty.allocate(884)
      empty.allocate(885)
      empty.numFreeBlocks must_== 1752
    }
  }
  def formatted(date: Date) = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dateFormat.format(date)
  }
}
