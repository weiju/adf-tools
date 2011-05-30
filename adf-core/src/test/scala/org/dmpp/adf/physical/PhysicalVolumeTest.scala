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

import java.io._

import org.scalatest.FlatSpec
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PhysicalVolumeSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  var disk: PhysicalVolume = null
  override def beforeEach {
    var workbenchFile: InputStream = null
    try {
      workbenchFile = getClass.getResourceAsStream("/wbench1.3.adf")
      disk = PhysicalVolumeFactory.readDoubleDensityDisk(workbenchFile)
    } finally {
      if (workbenchFile != null) workbenchFile.close
    }
  }

  "PhysicalVolume" should "data size should be correct" in {
    disk.sizeInBytes     should be (DoubleDensityDisk.ImageSize)
    disk.numSectorsTotal should be === (1760)
    disk.bytesPerSector  should be === (512)
  }
  it should "first three bytes of sector 0 must be DOS" in {
    disk.sector(0)(0).asInstanceOf[Char] should be === ('D')
    disk.sector(0)(1).asInstanceOf[Char] should be === ('O')
    disk.sector(0)(2).asInstanceOf[Char] should be === ('S')
  }
  it should "accessing sector 1759 should succeed" in {
    disk.sector(1759)(0)   should be === (0)
    disk.sector(1759)(511) should be === (121)
  }
  it should "accessing byte 512 of a sector should fail" in {
    evaluating { disk.sector(1759)(512) } should produce [IndexOutOfBoundsException]
    evaluating { disk.sector(0)(512) }    should produce [IndexOutOfBoundsException]
  }
  it should "accessing sector 1760 should fail" in {
    evaluating { disk.sector(1760)(0) } should produce [IndexOutOfBoundsException]
  }
  it should "write a byte in a sector" in {
    disk.sector(3)(2) = 42
    disk.sector(3)(3) = 128
    disk.sector(3)(4) = 255

    disk.sector(3)(2) should be === (42)
    disk.sector(3)(3) should be === (128)
    disk.sector(3)(4) should be === (255)
  }
  it should "access data bytes directly" in {
    disk(42) = 42
    disk(42) should be === (42)
  }
}

@RunWith(classOf[JUnitRunner])
class PhysicalVolumeFactorySpec extends FlatSpec with ShouldMatchers {
  "PhysicalVolumeFactory" should "create an empty double density disk" in {
    val disk = PhysicalVolumeFactory.createEmptyDoubleDensityDisk

    disk.sizeInBytes should be (DoubleDensityDisk.ImageSize)
    // check boot block
    for (i <- 0 until 1024) disk(i) should be === (0)

    // check root block
    for (i <- 0 until 1024) disk(512 * 880 + i) should be === (0)
  }
}
