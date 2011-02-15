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
extends ReadsBcplStrings with SectorBasedChecksum {
  import HeaderBlock._

  val sector          = physicalVolume.sector(sectorNumber)

  /**
   * Returns the block's primary type.
   * @return the primary type
   */
  def primaryType     = sector.int32At(0)

  /**
   * Returns this block's secondary type.
   *
   * @return the secondary type
   */
  def secondaryType   = sector.int32At(sector.sizeInBytes - 4)

  /**
   * Returns a pointer to this block's header.
   * @return the pointer to the header
   */
  def headerKey       = sector.int32At(4)

  /**
   * Returns the name field stored in this block.
   *
   * @return this block's name
   */
  def name: String = bcplStringAt(sector.sizeInBytes - 80, NameMaxChars)

  /**
   * Returns the next block in the hash bucket list.
   *
   * @return next block in hash bucket list
   */
  def nextInHashBucket = sector.int32At(sector.sizeInBytes - 16)

  /**
   * Returns the last modification date.
   *
   * @return last modification date
   */
  def lastModified: Date = {
    AmigaDosDate(sector.int32At(sector.sizeInBytes - 92),
                 sector.int32At(sector.sizeInBytes - 88),
                 sector.int32At(sector.sizeInBytes - 84)).toDate
  }
  def storedChecksum  = sector.int32At(20)
  def computedChecksum: Int = computeChecksum(20)
}

/**
 * A block that we can quickly wrap around a sector in order to determine
 * its type.
 */
class DirectoryEntryBlock(physicalVolume: PhysicalVolume,
                          sectorNumber: Int)
extends HeaderBlock(physicalVolume, sectorNumber) with HasComment
with HasAccessRights 

/**
 * A class to represent a user directory block.
 */
class UserDirectoryBlock(physicalVolume: PhysicalVolume, blockNumber: Int)
extends DirectoryEntryBlock(physicalVolume, blockNumber)
with DirectoryLike

/**
 * A class to represent a file header block.
 */
class FileHeaderBlock(physicalVolume: PhysicalVolume, blockNumber: Int)
extends DirectoryEntryBlock(physicalVolume, blockNumber)

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
extends HasChecksum with SectorBasedChecksum
with BitHelper {

  val sector = physicalVolume.sector(sectorNumber)
  def sectorSize = physicalVolume.bytesPerSector

  def storedChecksum        = sector.int32At(0)
  def computedChecksum: Int = computeChecksum(0)

  def freeBlockNumbers: List[Int] = countBitmapBitsWith(bitsSetIn _)
  def usedBlockNumbers: List[Int] = countBitmapBitsWith(bitsClearIn _)
  private def countBitmapBitsWith(countFunc: Int => List[Int]) = {
    var result: List[Int] = Nil
    var currentBlockOffset = 2
    for (fieldIndex <- 4 until sectorSize by 4) {
      val mask = sector.int32At(fieldIndex)
      val bitsSet = countFunc(mask).map(n => n + currentBlockOffset)
      result = result ++ bitsSet
      currentBlockOffset += 32
    }
    result.filter(n => n < physicalVolume.numSectorsTotal)
  }
}
