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

/**
 * An interface to indicate an object that has a checksum
 */
trait HasChecksum {

  /**
   * Returns the currently stored checksum for this block.
   *
   * @return the currently stored checksum
   */
  def storedChecksum: Int

  /**
   * Computes a checksum in this block.
   *
   * @return the checksum based on this object' data
   */
  def computedChecksum: Int
}

/**
 * A mixin trait which provides the standard checksum algorithm. This
 * algorithm can be used for all blocks except the boot block.
 */
trait SectorBasedChecksum {
  /**
   * Returns the sector this block is based on.
   *
   * @return the Sector instance
   */
  def sector: Sector

  /**
   * Returns thes sector size.
   *
   * @return sector size
   */
  def sectorSize: Int

  /**
   * Standard checksum algorithm.
   *
   * @param checksumFieldOffset the offset of the field containing the stored
   *        checksum
   * @return the checksum based on this object' data
   */
  def computeChecksum(checksumFieldOffset: Int): Int = {
    import UnsignedInt32Conversions._

    var sum: UnsignedInt32 = 0
    for (i <- 0 until sectorSize by 4) {
      if (i != checksumFieldOffset) { // ignore the checksum field
        sum += (sector.int32At(i) & 0xffffffffl)
      }
    }
    -sum.intValue
  }
}

/**
 * Abstract super class for file system blocks. File system blocks
 * have primary and secondary types.
 *
 * @constructor creates a file system block on a sector in a physical volume
 * @param physicalVolume the physical volume
 * @param sectorNumber the sector number
 */
abstract class FileSystemBlock(physicalVolume: PhysicalVolume,
                               val sectorNumber: Int)
extends HasChecksum with SectorBasedChecksum {

  val sector          = physicalVolume.sector(sectorNumber)
  def sectorSize      = physicalVolume.bytesPerSector

  /**
   * Returns the block's primary type.
   * @return the primary type
   */
  def primaryType     = sector.int32At(0)

  /**
   * Returns a pointer to this block's header.
   * @return the pointer to the header
   */
  def headerKey       = sector.int32At(4)

  /**
   * Returns this block's secondary type.
   *
   * @return the secondary type
   */
  def secondaryType   = sector.int32At(sectorSize - 4)

  def storedChecksum  = sector.int32At(20)
  def computedChecksum: Int = computeChecksum(20)
}

/**
 * Symbolic constants for file system blocks.
 */
object HeaderBlock {
  val NameMaxChars = 30
}

/**
 * Header blocks are the first block of a file or directory. They can be
 * referenced by hash tables.
 *
 * @constructor creates a header block for a sector on a volume
 * @param physicalVolume a physical volume
 * @param sectorNumber a sector number
 */
abstract class HeaderBlock(physicalVolume: PhysicalVolume,
                           sectorNumber: Int)
extends FileSystemBlock(physicalVolume: PhysicalVolume,
                        sectorNumber: Int) {
  import HeaderBlock._

  /**
   * Returns the name field stored in this block.
   *
   * @return this block's name
   */
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

  /**
   * Returns the next block in the hash bucket list.
   *
   * @return next block in hash bucket list
   */
  def nextInHashBucket = sector.int32At(sectorSize - 16)
}

/**
 * Abstract super class for directory blocks.
 * 
 * @constructor creates a directory block on a sector in a physical volume
 * @param physicalVolume the physical volume
 * @param sectorNumber the sector number
 */
abstract class DirectoryBlock(physicalVolume: PhysicalVolume,
                              sectorNumber: Int)
extends HeaderBlock(physicalVolume, sectorNumber) {

  /**
   * Returns the size of the directory's hash table.
   *
   * @return hash table size
   */
  def hashtableSize   = sector.int32At(12)

  /**
   * Returns the list of all valid header blocks contained in this directory's
   * hash table.
   *
   * @return all header blocks in the directory
   */
  def hashtableEntries: List[HeaderBlock] = {
    var result : List[HeaderBlock] = Nil
    val byteSize = hashtableSize * 4
    for (i <- 0 until byteSize by 4) {
      result = addToBucketRecursively(result, sector.int32At(i))
    }
    result.reverse
  }
  private def addToBucketRecursively(addTo: List[HeaderBlock],
                                     blockNumber: Int): List[HeaderBlock] = {
    if (isNonEmptyHashEntry(blockNumber)) {
      val block = new GenericHeaderBlock(physicalVolume, blockNumber)
      addToBucketRecursively(block :: addTo, block.nextInHashBucket)
    } else addTo
  }
  private def isNonEmptyHashEntry(entry: Int) = entry > 0
}

/**
 * A block that we can quickly wrap around a sector in order to determine
 * its type.
 */
class GenericHeaderBlock(physicalVolume: PhysicalVolume,
                         sectorNumber: Int)
extends HeaderBlock(physicalVolume, sectorNumber)

/**
 * A bitmap block stores information about free and used blocks in the
 * file system.
 *
 * @constructor creates a bitmap block in the specified sector of a volume
 * @param physicalVolume the physical volume
 * @param sectorNumber ths sector number
 */
class BitmapBlock(physicalVolume: PhysicalVolume,
                  val sectorNumber: Int)
extends HasChecksum with SectorBasedChecksum {

  val sector = physicalVolume.sector(sectorNumber)
  def sectorSize = physicalVolume.bytesPerSector

  def storedChecksum        = sector.int32At(0)
  def computedChecksum: Int = computeChecksum(0)
}

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
  class BootBlock extends HasChecksum with BitHelper {
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
  class RootBlock(sectorNumber: Int)
  extends DirectoryBlock(physicalVolume, sectorNumber) {

    def highSeq         = sector.int32At(8)
    def firstData       = sector.int32At(16)
    // hash table data from 24 to (<sector size> - 200)

    def bitmapIsValid: Boolean = {
      (sector.int32At(sectorSize - 200) == 0xffffffff)
    }
    def bitmapBlocks: List[BitmapBlock] = {
      var result: List[BitmapBlock] = Nil
      val baseOffset = sectorSize - 196
      for (i <- 0 until 25) {
        val bitmapBlockId = sector.int32At(baseOffset + i * 4)
        if (bitmapBlockId > 0) result ::= new BitmapBlock(physicalVolume, bitmapBlockId)
      }
      result.reverse
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
  }

  /** This volume's boot block. */
  val bootBlock = new BootBlock

  /** This volumes's root block. */
  val rootBlock = new RootBlock(RootSectorNumber)
}
