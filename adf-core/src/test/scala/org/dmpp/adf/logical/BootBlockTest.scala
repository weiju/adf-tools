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
class BootBlockSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  var emptyOFS : LogicalVolume = null
  var emptyFFS : LogicalVolume = null
  def bootBlockOFS = emptyOFS.bootBlock
  def bootBlockFFS = emptyFFS.bootBlock

  override def beforeEach {
    emptyOFS = LogicalVolumeFactory.createEmptyDoubleDensityDisk("TestDisk", "OFS")
    emptyFFS = LogicalVolumeFactory.createEmptyDoubleDensityDisk("TestDisk", "FFS")
  }

  "BootBlock" should "ensure boot block is initialized" in {
    bootBlockOFS.isDosDisk       should be (true)
    bootBlockOFS.filesystemType  should be ("OFS")
    bootBlockOFS.isInternational should be (false)
    bootBlockOFS.useDirCache     should be (false)

    bootBlockFFS.isDosDisk       should be (true)
    bootBlockFFS.filesystemType  should be ("FFS")
    bootBlockFFS.isInternational should be (false)
    bootBlockFFS.useDirCache     should be (false)
  }
}


