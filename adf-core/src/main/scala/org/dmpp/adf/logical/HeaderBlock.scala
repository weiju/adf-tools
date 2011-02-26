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
import org.dmpp.adf.util._

/**
 * Symbolic constants for header blocks.
 */
object HeaderBlock {
  val NameMaxChars      = 30
  val OffsetPrimaryType = 0
  val OffsetHeaderKey   = 4
}

/**
 * HeaderBlocks represent the first block of a file or directory, the
 * root block is a header block as well.
 *
 * @constructor creates a header block for a sector on a volume
 * @param logicalVolume a physical volume
 * @param blockNumber a block number
 */
abstract class HeaderBlock(val logicalVolume: LogicalVolume,
                           val blockNumber: Int)
extends LogicalBlock with ReadsBcplStrings with HasChecksum with SectorBasedChecksum {
  import HeaderBlock._

  def physicalVolume      = logicalVolume.physicalVolume
  val sector              = physicalVolume.sector(blockNumber)
  def OffsetSecondaryType = sector.sizeInBytes - 4
  def OffsetName          = sector.sizeInBytes - 80
  def OffsetHashNext      = sector.sizeInBytes - 16
  def OffsetParent        = sector.sizeInBytes - 12

  /**
   * Returns the block's primary type.
   * @return the primary type
   */
  def primaryType     = sector.int32At(OffsetPrimaryType)

  /**
   * Sets the block's primary type.
   * @param newType the new type
   */
  def primaryType_=(newType: Int) = sector.setInt32At(OffsetPrimaryType, newType)

  /**
   * Returns this block's secondary type.
   * @return the secondary type
   */
  def secondaryType   = sector.int32At(OffsetSecondaryType)

  /**
   * Sets the block's secondary type.
   * @param newType the new type
   */
  def secondaryType_=(newType: Int) = sector.setInt32At(OffsetSecondaryType,
                                                        newType)

  /**
   * Returns this block's block number.
   * @return this block's block number
   */
  def headerKey       = sector.int32At(OffsetHeaderKey)

  /**
   * Changes this block's header key value.
   * @param newKey new key value
   */
  def headerKey_=(newKey: Int) = sector.setInt32At(OffsetHeaderKey, newKey)

  /**
   * Returns the name field stored in this block.
   * @return this block's name
   */
  def name: String = bcplStringAt(OffsetName, NameMaxChars)

  /**
   * Sets the name field for this block. If the length of newName exceeds
   * NameMaxChars, an IllegalArgumentException is thrown.
   * @param newName this block's new name
   */
  def name_=(newName: String) {
    if (newName.length > NameMaxChars) {
      throw new IllegalArgumentException("max. 30 characters for name")
    }
    setBcplStringAt(OffsetName, NameMaxChars, newName)
  }

  /**
   * Returns the next block in the hash bucket list.
   * @return next block in hash bucket list
   */
  def nextInHashBucket = sector.int32At(OffsetHashNext)

  /**
   * Sets the next block in the hash bucket list.
   * @param next new next pointer
   */
  def nextInHashBucket_=(next: Int) = sector.setInt32At(OffsetHashNext, next)

  /**
   * Returns the last modification time.
   * @return last access time
   */
  def lastModificationTime: Date = {
    AmigaDosDate(sector.int32At(sector.sizeInBytes - 92),
                 sector.int32At(sector.sizeInBytes - 88),
                 sector.int32At(sector.sizeInBytes - 84)).toDate
  }

  /**
   * Sets the last modification time to the current time.
   */
  def updateLastModificationTime {
    import AmigaDosDateConversions._
    val amigaDate: AmigaDosDate = new Date
    sector.setInt32At(sector.sizeInBytes - 92, amigaDate.daysSinceJan_1_78)
    sector.setInt32At(sector.sizeInBytes - 88, amigaDate.minutesPastMidnight)
    sector.setInt32At(sector.sizeInBytes - 84, amigaDate.ticksPastLastMinute)
    recomputeChecksum
  }

  def storedChecksum  = sector.int32At(20)
  def computedChecksum: Int = computeChecksum(20)
  def recomputeChecksum = sector.setInt32At(20, computedChecksum)

  def parent    = sector.int32At(OffsetParent)
  def parent_=(newParent: Int) = sector.setInt32At(OffsetParent, newParent)
}
