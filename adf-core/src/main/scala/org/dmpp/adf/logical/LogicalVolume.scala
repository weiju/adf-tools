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

import java.util.Date
import org.dmpp.adf.util._
import org.dmpp.adf.physical._
import java.io._

/**
 * A factory to create logical volume instances.
 */
object LogicalVolumeFactory {
  /**
   * Creates an empty, non-bootable DD "formatted" disk.
   * @param volume name (optional)
   * @return initialized [[org.dmpp.adf.logical.LogicalVolume]] instance
   */
  def createEmptyDoubleDensityDisk(name: String = "Empty") = {
    val logicalVolume =
      new LogicalVolume(PhysicalVolumeFactory.createEmptyDoubleDensityDisk)
    logicalVolume.initialize(name)
    logicalVolume
  }
}

/**
 * Symbolic constants for logical volumes.
 */
object LogicalVolume {
  val RootSectorNumber   = 880
}

/**
 * A logical volume based on an underlying physical volume. On this
 * layer, we talk about blocks, which are effectively typed sectors.
 *
 * @constructor creates a logical volume instance with a physical volume
 * @param physicalVolume the physical volume the logical volume is based on
 */
class LogicalVolume(physicalVolume: PhysicalVolume) {
  import LogicalVolume._

  /**
   * Initializes this volume as an empty volume.
   * @param name volume name
   * @param filesystemType file system type, defaults to "FFS"
   */
  def initialize(name: String,
                 filesystemType: String = "FFS") {
    bootBlock.initialize(filesystemType)
    rootBlock.initialize(name)
    new BitmapBlock(physicalVolume, 881).initialize
    allocate(880) // root block
    allocate(881) // bitmap block
  }

  def writeToOutputStream(out: OutputStream) = physicalVolume.writeToOutputStream(out)
  def sizeInBytes = physicalVolume.sizeInBytes
  def apply(byteNum: Int) = physicalVolume(byteNum)

  /** This volume's boot block. */
  val bootBlock = new BootBlock(physicalVolume)

  /** This volumes's root block. */
  val rootBlock = new RootBlock(physicalVolume, RootSectorNumber)

  /**
   * Returns this volume's name.
   * @return the volume's name
   */
  def name = rootBlock.name

  /**
   * Marks the specified block as used.
   * @param blockNumber the block number to mark as used
   */
  def allocate(blockNumber: Int) {
    val bitmapBlock0 = rootBlock.bitmapBlockAt(0).get
    bitmapBlock0.allocate(blockNumber - 2)
  }

  /**
   * Retrieve the free block numbers on this logical volume.
   * @return free block numbers
   */
  def freeBlockNumbers: List[Int] = {
    val bitmapBlock0 = rootBlock.bitmapBlockAt(0).get
    bitmapBlock0.freeBlockIndexes.filter(index =>
      index < physicalVolume.numSectorsTotal - 2).map(index => index + 2)
  }

  /**
   * Retrieve the used block numbers on this logical volume.
   * @return used block numbers
   */
  def usedBlockNumbers: List[Int] = {
    val bitmapBlock0 = rootBlock.bitmapBlockAt(0).get
    bitmapBlock0.usedBlockIndexes.filter(index =>
      index < physicalVolume.numSectorsTotal - 2).map(index => index + 2)
  }
}
