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

class BootBlockTest extends JUnit4(BootBlockSpec)
object BootBlockSpecRunner extends ConsoleRunner(BootBlockSpec)

object BootBlockSpec extends Specification {
  var emptyOFS : LogicalVolume = null
  var emptyFFS : LogicalVolume = null
  def bootBlockOFS = emptyOFS.bootBlock
  def bootBlockFFS = emptyFFS.bootBlock

  "BootBlock" should {
    doBefore {
      emptyOFS = LogicalVolumeFactory.createEmptyDoubleDensityDisk("TestDisk", "OFS")
      emptyFFS = LogicalVolumeFactory.createEmptyDoubleDensityDisk("TestDisk", "FFS")
    }

    "boot block is initialized" in {
      bootBlockOFS.filesystemType must_== "OFS"
      bootBlockOFS.isInternational must beFalse
      bootBlockOFS.useDirCache must beFalse

      bootBlockFFS.filesystemType must_== "FFS"
      bootBlockFFS.isInternational must beFalse
      bootBlockFFS.useDirCache must beFalse
    }
  }
}


