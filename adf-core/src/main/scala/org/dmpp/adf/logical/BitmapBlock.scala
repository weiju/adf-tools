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

import org.dmpp.adf.physical._
import org.dmpp.adf.util._

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

