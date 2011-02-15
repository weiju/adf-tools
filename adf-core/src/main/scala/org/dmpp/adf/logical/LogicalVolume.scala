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

/**
 * A logical volume based on an underlying physical volume.
 *
 * @constructor creates a logical volume instance with a physical volume
 * @param physicalVolume the physical volume the logical volume is based on
 */
class LogicalVolume(physicalVolume: PhysicalVolume) {
  import LogicalVolume._

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

    def storedChecksum  = physicalVolume.int32At(4)
    def rootBlockNumber = physicalVolume.int32At(8)

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

  /**
   * Symbolic constants for logical volumes.
   */
  object LogicalVolume {
    val RootSectorNumber   = 880
  }

  /**
   * This class represents an Amiga volume's root block.
   *
   * @constructor creates a root block instance for the specified sector
   * @param sectorNumber the sector number
   */
  protected class RootBlock(sectorNumber: Int)
  extends HeaderBlock(physicalVolume, sectorNumber)
  with DirectoryLike {

    def highSeq         = sector.int32At(8)
    def firstData       = sector.int32At(16)

    def bitmapIsValid: Boolean = {
      (sector.int32At(sector.sizeInBytes - 200) == 0xffffffff)
    }
    def bitmapBlocks: List[BitmapBlock] = {
      var result: List[BitmapBlock] = Nil
      val baseOffset = sector.sizeInBytes - 196
      for (i <- 0 until 25) {
        val bitmapBlockId = sector.int32At(baseOffset + i * 4)
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
}
