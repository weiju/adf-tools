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
package org.dmpp.adf.physical

/**
 * Test cases for physical volumes.
 */
import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._

class PhysicalVolumeTest extends JUnit4(PhysicalVolumeSpec)
object PhysicalVolumeSpecRunner extends ConsoleRunner(PhysicalVolumeSpec)

object PhysicalVolumeSpec extends Specification {

  var disk: PhysicalVolume = null

  "PhysicalVolume" should {

    doBefore {
      var workbenchFile: InputStream = null
      try {
        workbenchFile = getClass.getResourceAsStream("/wbench1.3.adf")
        disk = PhysicalVolumeFactory.readDoubleDensityDisk(workbenchFile)
      } finally {
        if (workbenchFile != null) workbenchFile.close
      }
    }

    "data size should be correct" in {
      disk.sizeInBytes must_== DoubleDensityDisk.ImageSize
      disk.numSectorsTotal must_== 1760
      disk.bytesPerSector  must_== 512
    }
    "first three bytes of sector 0 must be DOS" in {
      disk.sector(0)(0).asInstanceOf[Char] must_== 'D'
      disk.sector(0)(1).asInstanceOf[Char] must_== 'O'
      disk.sector(0)(2).asInstanceOf[Char] must_== 'S'
    }
    "accessing sector 1759 should succeed" in {
      disk.sector(1759)(0) must_== 0
      disk.sector(1759)(511) must_== 121
    }
    "accessing byte 512 of a sector should fail" in {
      disk.sector(1759)(512) must throwA[IndexOutOfBoundsException]
      disk.sector(0)(512) must throwA[IndexOutOfBoundsException]
    }
    "accessing sector 1760 should fail" in {
      disk.sector(1760)(0) must throwA[IndexOutOfBoundsException]
    }
    "write a byte in a sector" in {
      disk.sector(3)(2) = 42
      disk.sector(3)(3) = 128
      disk.sector(3)(4) = 255

      disk.sector(3)(2) must_== 42
      disk.sector(3)(3) must_== 128
      disk.sector(3)(4) must_== 255
    }
    "access data bytes directly" in {
      disk(42) = 42
      disk(42) must_== 42
    }
  }

  "PhysicalVolumeFactory" should {
    "create an empty double density disk" in {
      val disk = PhysicalVolumeFactory.createEmptyDoubleDensityDisk
      disk.sizeInBytes must_== DoubleDensityDisk.ImageSize
      // check boot block
      for (i <- 0 until 1024) {
        disk(i) must_== 0
      }
      // check root block
      for (i <- 0 until 1024) {
        disk(512 * 880 + i) must_== 0
      }
    }
  }
}
