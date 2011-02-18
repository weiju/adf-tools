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
package org.dmpp.adf.logical

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import org.dmpp.adf.physical._

class LogicalVolumeFactoryTest extends JUnit4(LogicalVolumeFactorySpec)
object LogicalVolumeFactorySpecRunner
extends ConsoleRunner(LogicalVolumeFactorySpec)

object LogicalVolumeFactorySpec extends Specification {

  "LogicalVolumeFactory" should {

    "create an empty volume" in {
      val volume = LogicalVolumeFactory.createEmptyDoubleDensityDisk()
      checkForValidBootBlock(volume)      
      checkForValidRootBlock(volume)
      volume.usedBlockNumbers must_== List(880, 881)
      volume.name must_== "Empty"
    }

    def checkForValidBootBlock(volume: LogicalVolume) {
      volume.sizeInBytes must_== DoubleDensityDisk.ImageSize
      volume(0).asInstanceOf[Char] must_== 'D'
      volume(1).asInstanceOf[Char] must_== 'O'
      volume(2).asInstanceOf[Char] must_== 'S'
      for (i <- 3 until 1024) volume(i) must_== 0
    }
    def checkForValidRootBlock(volume: LogicalVolume) {
      volume.rootBlock.primaryType must_== BlockType.PtShort
      volume.rootBlock.secondaryType must_== BlockType.StRoot
      volume.rootBlock.bitmapIsValid must beTrue
      volume.rootBlock.name must_== "Empty"
      volume.rootBlock.hashtableSize must_== 0x48 // = 72
      volume.rootBlock.storedChecksum must_== volume.rootBlock.computedChecksum
      volume.rootBlock.bitmapBlockIdAt(0) must_== 881
      volume.rootBlock.bitmapBlockIdAt(1) must_== 0
    }
  }
}
