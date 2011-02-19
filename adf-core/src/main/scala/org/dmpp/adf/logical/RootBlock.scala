/**
 * Created on February 17, 2011
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
with UsesHashtable {

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
   * Returns the last modification time of the volume. Updated when the
   * disk contents is changed.
   * @return last modification time
   */
  def diskLastModificationTime: Date = {
    AmigaDosDate(sector.int32At(sector.sizeInBytes - 40),
                 sector.int32At(sector.sizeInBytes - 36),
                 sector.int32At(sector.sizeInBytes - 32)).toDate
  }
  /**
   * Returns the creation time of the file system. This value is only generated
   * when a volume is initialized.
   * @return file system creation time
   */
  def creationTime: Date = {
    AmigaDosDate(sector.int32At(sector.sizeInBytes - 28),
                 sector.int32At(sector.sizeInBytes - 24),
                 sector.int32At(sector.sizeInBytes - 20)).toDate
  }

  /**
   * Last modification time. In the root block, this replaces lastAccessTime.
   * @return last modification time
   */
  def lastModificationTime: Date = super.lastAccessTime

  /**
   * Throws an UnsupportedOperationException in the root block.
   * @return nothing
   */
  override def lastAccessTime: Date = {
    throw new UnsupportedOperationException("lastAccessTime not available in root block")
  }
}
