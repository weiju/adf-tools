/**
 * Created on February 14, 2011
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

import org.dmpp.adf.physical._
import org.dmpp.adf.util._
import java.util.Date

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

trait LogicalBlock {
  def physicalVolume: PhysicalVolume
  def sector: Sector
}

/**
 * Symbolic constants for header blocks.
 */
object HeaderBlock {
  val NameMaxChars = 30
}

/**
 * HeaderBlocks represent the first block of a file or directory, the
 * root block is a header block as well.
 *
 * @constructor creates a header block for a sector on a volume
 * @param physicalVolume a physical volume
 * @param sectorNumber a sector number
 */
abstract class HeaderBlock(val physicalVolume: PhysicalVolume,
                           val sectorNumber: Int)
extends LogicalBlock with ReadsBcplStrings with SectorBasedChecksum {
  import HeaderBlock._

  val sector          = physicalVolume.sector(sectorNumber)

  /**
   * Returns the block's primary type.
   * @return the primary type
   */
  def primaryType     = sector.int32At(0)

  /**
   * Sets the block's primary type.
   * @param newType the new type
   */
  def primaryType_=(newType: Int) = sector.setInt32At(0, newType)

  /**
   * Returns this block's secondary type.
   * @return the secondary type
   */
  def secondaryType   = sector.int32At(sector.sizeInBytes - 4)

  /**
   * Sets the block's secondary type.
   * @param newType the new type
   */
  def secondaryType_=(newType: Int) = sector.setInt32At(sector.sizeInBytes - 4,
                                                         newType)

  /**
   * Returns a pointer to this block's header.
   * @return the pointer to the header
   */
  def headerKey       = sector.int32At(4)

  /**
   * Returns the name field stored in this block.
   * @return this block's name
   */
  def name: String = bcplStringAt(sector.sizeInBytes - 80, NameMaxChars)

  /**
   * Sets the name field for this block.
   * @param newName this block's new name
   */
  def name_=(newName: String) {
    setBcplStringAt(sector.sizeInBytes - 80, NameMaxChars, newName)
  }

  /**
   * Returns the next block in the hash bucket list.
   * @return next block in hash bucket list
   */
  def nextInHashBucket = sector.int32At(sector.sizeInBytes - 16)

  /**
   * Returns the last modification date.
   * @return last modification date
   */
  def lastModified: Date = {
    AmigaDosDate(sector.int32At(sector.sizeInBytes - 92),
                 sector.int32At(sector.sizeInBytes - 88),
                 sector.int32At(sector.sizeInBytes - 84)).toDate
  }
  def storedChecksum  = sector.int32At(20)
  def computedChecksum: Int = computeChecksum(20)
  def recomputeChecksum = sector.setInt32At(20, computedChecksum)
}

/**
 * A block that we can quickly wrap around a sector in order to determine
 * its type.
 */
class DirectoryEntryBlock(physicalVolume: PhysicalVolume,
                          sectorNumber: Int)
extends HeaderBlock(physicalVolume, sectorNumber) with HasComment
with HasAccessRights {
  def isDirectory = secondaryType == BlockType.StUserDir
  def isFile      = secondaryType == BlockType.StFile
}

/**
 * A class to represent a user directory block.
 */
class UserDirectoryBlock(physicalVolume: PhysicalVolume, blockNumber: Int)
extends DirectoryEntryBlock(physicalVolume, blockNumber)
with DirectoryLike {
  def parentBlock = sector.int32At(sector.sizeInBytes - 12)
}

/**
 * A class to represent a file header block.
 */
class FileHeaderBlock(physicalVolume: PhysicalVolume, blockNumber: Int)
extends DirectoryEntryBlock(physicalVolume, blockNumber) {
  def fileSize  = sector.int32At(sector.sizeInBytes - 188)
  def dataBlocks = {
    var result: List[Int] = Nil
    var pointer = sector.sizeInBytes - 204
    var atLastBlock = fileSize == 0
    while (!atLastBlock) {
      val blocknum = sector.int32At(pointer)
      if (blocknum > 0) result ::= blocknum
      pointer -= 4
      atLastBlock = (pointer < 24) || blocknum <= 0
    }
    if (pointer < 24) {
      throw new UnsupportedOperationException("Large files not supported yet")
    }
    result.reverse
  }
}

/**
 * Exception that is thrown when allocate is called on an already allocated
 * block.
 */
class BlockAlreadyAllocated extends Exception

/**
 * A bitmap block stores information about free and used blocks in the
 * file system.
 *
 * @constructor creates a bitmap block in the specified sector of a volume
 * @param physicalVolume the physical volume
 * @param sectorNumber ths sector number
 */
class BitmapBlock(val physicalVolume: PhysicalVolume,
                  val sectorNumber: Int)
extends LogicalBlock with HasChecksum with SectorBasedChecksum
with BitHelper {

  val sector = physicalVolume.sector(sectorNumber)
  def sectorSize = physicalVolume.bytesPerSector
  def storedChecksum        = sector.int32At(0)
  def recomputeChecksum = sector.setInt32At(0, computedChecksum)
  def computedChecksum: Int = computeChecksum(0)

  def initialize {
    for (i <- 4 until sectorSize) {
      sector(i) = 0xff.asInstanceOf[Byte]
    }
    recomputeChecksum
  }

  /**
   * Marks the bit with the specified relative index as used.
   * @param relativeIndex relative index within this bitmap
   */
  def allocate(relativeIndex: Int) {
    if (isAllocated(relativeIndex)) throw new BlockAlreadyAllocated
    val bitClearMask = (~byteMaskForBit(bitNumForIndex(relativeIndex))) & 0xff
    sector(4 + byteNumForIndex(relativeIndex)) &= bitClearMask
    recomputeChecksum
  }
  private def byteMaskForBit(bitnum: Int): Byte = {
    ((1 << (7 - bitnum)) & 0xff).asInstanceOf[Byte]
  }
  def free(relativeIndex: Int) {
    val mask = byteMaskForBit(bitNumForIndex(relativeIndex))
    sector(4 + byteNumForIndex(relativeIndex)) |= mask
    recomputeChecksum
  }

  /**
   * Determine whether the block at relativeIndex is allocated.
   * @param relativeIndex the block index
   * @return true if allocated, false otherwise
   */
  def isAllocated(relativeIndex: Int): Boolean = {
    val mask = byteMaskForBit(bitNumForIndex(relativeIndex))
    (sector(4 + byteNumForIndex(relativeIndex)) & mask) == 0
  }
  /**
   * Determine whether the block at relativeIndex is free.
   * @param relativeIndex the block index
   * @return true if free, false otherwise
   */
  def isFree(relativeIndex: Int): Boolean = !isAllocated(relativeIndex)
  private def byteNumForIndex(relativeIndex: Int) = relativeIndex / 8
  private def bitNumForIndex(relativeIndex: Int) = relativeIndex % 8

  def freeBlockIndexes: List[Int] = countBitmapBitsWith(bitsSetIn _)
  def usedBlockIndexes: List[Int] = countBitmapBitsWith(bitsClearIn _)

  private def countBitmapBitsWith(countFunc: Int => List[Int]) = {
    var result: List[Int] = Nil
    for (fieldIndex <- 4 until sectorSize by 4) {
      val currentBlockOffset = (fieldIndex - 4) * (32 / 4)
      val mask = sector.int32At(fieldIndex)
      val bitsSet = countFunc(mask).map(n => n + currentBlockOffset)
      result = result ++ bitsSet
    }
    result
  }
}

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
 * @constructor creates a boot block for the given physical volume
 * @param physicalVolume a [[org.dmpp.adf.physical.PhysicalVolume]] instance.
 */
class BootBlock(physicalVolume: PhysicalVolume) extends HasChecksum with BitHelper {
  import BootBlock._

  /**
   * Initializes an empty boot block.
   */
  def initialize {
    physicalVolume(0) = 'D'
    physicalVolume(1) = 'O'
    physicalVolume(2) = 'S'
  }

  def filesystemType  = if (flagClear(flags, FlagFFS)) "OFS" else "FFS"
  def isInternational = flagSet(flags, FlagIntlOnly) ||
  flagSet(flags, FlagDirCacheAndIntl)
  def useDirCache     = flagSet(flags, FlagDirCacheAndIntl)
  private def flags = physicalVolume(3) & 0x07
  
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

/**
 * Symbolic constants for root blocks.
 */
object RootBlock {
  val MaxBitmapBlocks = 25
}

/**
 * This class represents an Amiga volume's root block.
 * @constructor creates a root block instance for the specified sector
 * @param physicalVolume the physical volumex
 * @param sectorNumber the sector number
 */
class RootBlock(physicalVolume: PhysicalVolume, sectorNumber: Int)
extends HeaderBlock(physicalVolume, sectorNumber)
with DirectoryLike {

  import RootBlock._
  
  /**
   * Initializes an empty root block.
   * @param aName the volume's name
   */
  def initialize(aName: String) {
    primaryType = BlockType.PtShort
    secondaryType = BlockType.StRoot
    setBitmapIsValid
    name = aName
    hashtableSize = 0x48
    setBitmapBlockIdAt(0, 881)
    recomputeChecksum
  }

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

  /**
   * Returns the bitmap block at the specified index.
   * @param index bitmap block index
   * @return Some(BitmapBlock) if successful, None, otherwise
   */
  def bitmapBlockAt(index: Int): Option[BitmapBlock] = {
    val bitmapBlockId = bitmapBlockIdAt(index)
    if (bitmapBlockId <= 0) None
    else Some(new BitmapBlock(physicalVolume, bitmapBlockId))
  }
  /**
   * Returns all the bitmap block of this file system.
   * @return this filesystem's bitmap blocks
   */
  def bitmapBlocks: List[BitmapBlock] = {
    var result: List[BitmapBlock] = Nil
    for (i <- 0 until MaxBitmapBlocks) {
      val bitmapBlockId = bitmapBlockIdAt(i)
      if (bitmapBlockId > 0) result ::= new BitmapBlock(physicalVolume, bitmapBlockId)
    }
    result.reverse
  }
  /**
   * Returns the last modification time of the disk.
   * @return last modification time
   */
  def lastModifiedDisk: Date = {
    AmigaDosDate(sector.int32At(sector.sizeInBytes - 40),
                 sector.int32At(sector.sizeInBytes - 36),
                 sector.int32At(sector.sizeInBytes - 32)).toDate
  }
  /**
   * Returns the creation time of the file system.
   * @return file system creation time
   */
  def fsCreationTime: Date = {
    AmigaDosDate(sector.int32At(sector.sizeInBytes - 28),
                 sector.int32At(sector.sizeInBytes - 24),
                 sector.int32At(sector.sizeInBytes - 20)).toDate
  }
}
