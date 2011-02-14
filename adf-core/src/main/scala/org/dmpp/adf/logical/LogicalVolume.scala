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
 * All known primary and secondary block types in AmigaDOS.
 */
object BlockType {
  val PtShort    = 2
  val PtData     = 8
  val PtList     = 16
  val PtDirCache = 33

  val StRoot     =  1
  val StUserDir  =  2
  val StSoftLink =  3
  val StLinkDir  =  4
  val StFile     = -3
  val StLinkFile = -4
}

object FileSystemBlock {
  val NameMaxChars = 30
}

abstract class FileSystemBlock(physicalVolume: PhysicalVolume,
                               val sectorNumber: Int) {
  import FileSystemBlock._

  protected val sector          = physicalVolume.sector(sectorNumber)
  protected val sectorSize      = physicalVolume.bytesPerSector

  def primaryType     = sector.int32At(0)
  def headerKey       = sector.int32At(4)
  def storedChecksum  = sector.int32At(20)
  def secondaryType   = sector.int32At(sectorSize - 4)

  def name = bcplStringAt(sectorSize - 80)
  private def bcplStringAt(offset: Int) = {
    val nameLength = scala.math.min(sector(offset),
                                    NameMaxChars)
    val builder = new StringBuilder
    for (i <- 0 until nameLength) {
      builder.append(sector(offset + 1 + i).asInstanceOf[Char])
    }
    builder.toString
  }
  def nextInHashBucket = sector.int32At(sectorSize - 16)
}

abstract class DirectoryBlock(physicalVolume: PhysicalVolume,
                              sectorNumber: Int)
extends FileSystemBlock(physicalVolume, sectorNumber) {

  def hashtableSize   = sector.int32At(12)
  def hashtableEntries: List[FileSystemBlock] = {
    var result : List[FileSystemBlock] = Nil
    val byteSize = hashtableSize * 4
    for (i <- 0 until byteSize by 4) {
      result = addToBucketRecursively(result, sector.int32At(i))
    }
    result.reverse
  }
  private def addToBucketRecursively(addTo: List[FileSystemBlock],
                                     blockNumber: Int): List[FileSystemBlock] = {
    if (isNonEmptyHashEntry(blockNumber)) {
      val block = new UnspecifiedBlock(physicalVolume, blockNumber)
      addToBucketRecursively(block :: addTo, block.nextInHashBucket)
    } else addTo
  }
  private def isNonEmptyHashEntry(entry: Int) = entry > 0
}

/**
 * A block that we can quickly wrap around a sector in order to determine
 * its type.
 */
class UnspecifiedBlock(physicalVolume: PhysicalVolume,
                       sectorNumber: Int)
extends FileSystemBlock(physicalVolume, sectorNumber)

class LogicalVolume(physicalVolume: PhysicalVolume) {
  import LogicalVolume._

  object BootBlock {
    val FlagFFS              = 1
    val FlagIntlOnly         = 2
    val FlagDirCacheAndIntl  = 4
  }

  class BootBlock extends BitHelper {
    import BootBlock._

    private def flags = physicalVolume(3) & 0x07
    def fileType = if (flagClear(flags, FlagFFS)) "OFS" else "FFS"
    def isInternational = flagSet(flags, FlagIntlOnly) ||
                          flagSet(flags, FlagDirCacheAndIntl)
    def useDirCache = flagSet(flags, FlagDirCacheAndIntl)

    def storedChecksum        = physicalVolume.int32At(4)
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

  object LogicalVolume {
    val RootSectorNumber   = 880
  }
  class RootBlock(sectorNumber: Int)
  extends DirectoryBlock(physicalVolume, sectorNumber) {
    def highSeq         = sector.int32At(8)
    def firstData       = sector.int32At(16)
    // hash table data from 24 to (<sector size> - 200)

    def bitmapIsValid: Boolean = {
      (sector.int32At(sectorSize - 200) == 0xffffffff)
    }
    def lastModifiedRoot: Date = {
      AmigaDosDate(sector.int32At(sectorSize - 92),
                   sector.int32At(sectorSize - 88),
                   sector.int32At(sectorSize - 84)).toDate
    }
    def lastModifiedDisk: Date = {
      AmigaDosDate(sector.int32At(sectorSize - 40),
                   sector.int32At(sectorSize - 36),
                   sector.int32At(sectorSize - 32)).toDate
    }
    def fsCreationTime: Date = {
      AmigaDosDate(sector.int32At(sectorSize - 28),
                   sector.int32At(sectorSize - 24),
                   sector.int32At(sectorSize - 20)).toDate
    }
    def computedChecksum: Int = {
      import UnsignedInt32Conversions._

      var sum: UnsignedInt32 = 0
      for (i <- 0 until physicalVolume.bytesPerSector by 4) {
        if (i != 20) { // ignore offset 20 (the checksum field)
          sum += (sector.int32At(i) & 0xffffffffl)
        }
      }
      -sum.intValue
    }
  }

  val bootBlock = new BootBlock
  val rootBlock = new RootBlock(RootSectorNumber)
}
