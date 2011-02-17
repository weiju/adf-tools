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
   */
  def createEmptyDoubleDensityDisk = {
    val physicalVolume = PhysicalVolumeFactory.createEmptyDoubleDensityDisk
    val logicalVolume = new LogicalVolume(physicalVolume)
    logicalVolume.sector(0)(0) = 'D'
    logicalVolume.sector(0)(1) = 'O'
    logicalVolume.sector(0)(2) = 'S'
    logicalVolume.rootBlock.primaryType = BlockType.PtShort
    logicalVolume.rootBlock.secondaryType = BlockType.StRoot
    logicalVolume.rootBlock.setBitmapIsValid
    logicalVolume.rootBlock.name = "Empty"
    logicalVolume.rootBlock.hashtableSize = 0x48
    logicalVolume.rootBlock.setBitmapBlockIdAt(0, 881)
    logicalVolume.rootBlock.recomputeChecksum

    val bitmapBlock = new BitmapBlock(physicalVolume, 881)
    bitmapBlock.initialize
    logicalVolume.allocate(880) // root block
    logicalVolume.allocate(881) // bitmap block
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

  def writeToOutputStream(out: OutputStream) = physicalVolume.writeToOutputStream(out)
  def sizeInBytes = physicalVolume.sizeInBytes
  def apply(byteNum: Int) = physicalVolume(byteNum)
  def sector(sectorNum: Int) = physicalVolume.sector(sectorNum)

  /**
   * Symbolic constants for boot blocks.
   */
  object BootBlock {
    val FlagFFS              = 1
    val FlagIntlOnly         = 2
    val FlagDirCacheAndIntl  = 4
  }

  /**
   * This class represents the boot block on an Amiga volume.
   */
  protected class BootBlock extends HasChecksum with BitHelper {
    import BootBlock._

    private def flags = physicalVolume(3) & 0x07
    def filesystemType  = if (flagClear(flags, FlagFFS)) "OFS" else "FFS"
    def isInternational = flagSet(flags, FlagIntlOnly) ||
                          flagSet(flags, FlagDirCacheAndIntl)
    def useDirCache     = flagSet(flags, FlagDirCacheAndIntl)

    def rootBlockNumber = physicalVolume.int32At(8)
    def storedChecksum  = physicalVolume.int32At(4)
    def recomputeChecksum = physicalVolume.setInt32At(4, computedChecksum)

    def computedChecksum: Int = {
      import UnsignedInt32Conversions._

      var sum: UnsignedInt32 = 0
      for (i <- 0 until 1024 by 4) {
        if (i != 4) {
          sum += (physicalVolume.int32At(i) & 0xffffffffl)
          if (sum.overflowOccurred) sum += 1
        }
      }
      ~sum.intValue
    }
  }

  object RootBlock {
    val MaxBitmapBlocks = 25
  }
  /**
   * This class represents an Amiga volume's root block.
   * @constructor creates a root block instance for the specified sector
   * @param sectorNumber the sector number
   */
  protected class RootBlock(sectorNumber: Int)
  extends HeaderBlock(physicalVolume, sectorNumber)
  with DirectoryLike {
    import RootBlock._

    def highSeq         = sector.int32At(8)
    def firstData       = sector.int32At(16)

    def bitmapIsValid: Boolean = {
      (sector.int32At(sector.sizeInBytes - 200) == 0xffffffff)
    }
    def setBitmapIsValid {
      sector.setInt32At(sector.sizeInBytes - 200, 0xffffffff)
    }
    def bitmapBlockIdAt(index: Int) = {
      sector.int32At(bitmapBlockBaseOffset + index * 4)
    }
    def setBitmapBlockIdAt(index: Int, bitmapBlockId: Int) = {
      sector.setInt32At(bitmapBlockBaseOffset + index * 4, bitmapBlockId)
    }
    private def bitmapBlockBaseOffset = sector.sizeInBytes - 196
    def bitmapBlockAt(index: Int): Option[BitmapBlock] = {
      val bitmapBlockId = bitmapBlockIdAt(index)
      if (bitmapBlockId <= 0) None
      else Some(new BitmapBlock(physicalVolume, bitmapBlockId))
    }
    def bitmapBlocks: List[BitmapBlock] = {
      var result: List[BitmapBlock] = Nil
      for (i <- 0 until MaxBitmapBlocks) {
        val bitmapBlockId = bitmapBlockIdAt(i)
        if (bitmapBlockId > 0) result ::= new BitmapBlock(physicalVolume, bitmapBlockId)
      }
      result.reverse
    }
    def lastModifiedDisk: Date = {
      AmigaDosDate(sector.int32At(sector.sizeInBytes - 40),
                   sector.int32At(sector.sizeInBytes - 36),
                   sector.int32At(sector.sizeInBytes - 32)).toDate
    }
    def fsCreationTime: Date = {
      AmigaDosDate(sector.int32At(sector.sizeInBytes - 28),
                   sector.int32At(sector.sizeInBytes - 24),
                   sector.int32At(sector.sizeInBytes - 20)).toDate
    }
  }

  /** This volume's boot block. */
  val bootBlock = new BootBlock

  /** This volumes's root block. */
  val rootBlock = new RootBlock(RootSectorNumber)

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
   */
  def freeBlockNumbers: List[Int] = {
    val bitmapBlock0 = rootBlock.bitmapBlockAt(0).get
    bitmapBlock0.freeBlockIndexes.filter(index =>
      index < physicalVolume.numSectorsTotal - 2).map(index => index + 2)
  }
  def usedBlockNumbers: List[Int] = {
    val bitmapBlock0 = rootBlock.bitmapBlockAt(0).get
    bitmapBlock0.usedBlockIndexes.filter(index =>
      index < physicalVolume.numSectorsTotal - 2).map(index => index + 2)
  }
}
