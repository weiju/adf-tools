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

import java.util.Date
import org.dmpp.adf.physical._

/**
 * A block that we can quickly wrap around a sector in order to determine
 * its type.
 * @constructor creates an DirectoryEntryBlock instance
 * @param physicalVolume a [[org.dmpp.adf.physical.PhysicalVolume]] instance
 * @param blockNumber the block number
 */
class DirectoryEntryBlock(physicalVolume: PhysicalVolume,
                          blockNumber: Int)
extends HeaderBlock(physicalVolume, blockNumber) with HasComment
with HasAccessRights {
  def isDirectory  = secondaryType == BlockType.StUserDir
  def isFile       = secondaryType == BlockType.StFile
}

/**
 * A class to represent a user directory block.
 */
class UserDirectoryBlock(physicalVolume: PhysicalVolume, blockNumber: Int)
extends DirectoryEntryBlock(physicalVolume, blockNumber)
with DirectoryBlock {
}

/**
 * Constants for FileHeaderBlock
 */
object FileHeaderBlock {
  val OffsetBlockCount     = 8
  val OffsetFirstDataBlock = 16
}

/**
 * A class to represent a file header block.
 */
class FileHeaderBlock(physicalVolume: PhysicalVolume, blockNumber: Int)
extends DirectoryEntryBlock(physicalVolume, blockNumber) {
  import FileHeaderBlock._
  def OffsetDataBlockIndex0 = sector.sizeInBytes - 204

  /**
   * Returns the number of blocks used in this block list.
   * @return number of blocks
   */
  def blockCount           = sector.int32At(OffsetBlockCount)

  /**
   * Sets the number of blocks used in this block list.
   * @param count the number of blocks used
   */
  def blockCount_=(count: Int) = sector.setInt32At(OffsetBlockCount, count)

  def firstDataBlockNumber = sector.int32At(OffsetFirstDataBlock) 
  def firstDataBlockNumber_=(blockNumber: Int) {
    sector.setInt32At(OffsetFirstDataBlock, blockNumber)
  }
  def fileSize  = sector.int32At(sector.sizeInBytes - 188)
  def fileSize_=(size: Int) = sector.setInt32At(sector.sizeInBytes - 188, size)

  /**
   * Initializes the data occupied by this block.
   */
  def initialize(parentBlock: Int, fileName: String) {
    for (i <- 0 until sector.sizeInBytes) sector(i) = 0
    primaryType = BlockType.PtShort
    secondaryType = BlockType.StFile
    headerKey  = blockNumber
    parent     = parentBlock
    name       = fileName
  }

  def dataBlock(index: Int): Int = {
    val offset = OffsetDataBlockIndex0 - index * 4    
    if (offset < 24) {
      throw new UnsupportedOperationException("Large files not supported yet")
    } else sector.int32At(offset)
  }
  def setDataBlock(index: Int, blockNumber: Int) {
    val offset = OffsetDataBlockIndex0 - index * 4
    if (offset < 24) {
      throw new UnsupportedOperationException("Large files not supported yet")
    } else sector.setInt32At(offset, blockNumber)
  }

  def dataBlocks = {
    var result: List[Int] = Nil
    var offset = OffsetDataBlockIndex0
    var atLastBlock = fileSize == 0
    while (!atLastBlock) {
      val blocknum = sector.int32At(offset)
      if (blocknum > 0) result ::= blocknum
      offset -= 4
      atLastBlock = (offset < 24) || blocknum <= 0
    }
    if (offset < 24) {
      throw new UnsupportedOperationException("Large files not supported yet")
    }
    result.reverse
  }

  /**
   * Last modification time. In a file header block, this replaces lastAccessTime.
   * @return last modification time
   */
  def lastModificationTime: Date = super.lastAccessTime

  /**
   * Throws an UnsupportedOperationException in a file header block.
   * @return nothing
   */
  override def lastAccessTime: Date = {
    throw new UnsupportedOperationException("lastAccessTime not available in root block")
  }
}

/**
 * General interface for a data block.
 */
trait DataBlock extends LogicalBlock {
  def blockNumber: Int
  def apply(index: Int): Int
  def update(index: Int, value: Int)

  /**
   * Returns this block's data bytes.
   * @return the data bytes
   */
  def dataBytes: Array[Byte]

  def maxDataBytes: Int
  def isOFS: Boolean
  def isFFS: Boolean
}

/**
 * Symbolic constants for OfsDataBlock.
 */
object OfsDataBlock {
  val HeaderSize = 24
}

/**
 * OFS data blocks contain more information than just data.
 * Currently, that information is ignored and an OFS data block
 * only provides access to data bytes.
 * @constructor create an OfsDataBlock
 * @param physicalVolume a PhysicalVolume
 * @param blockNumber the block number of this block
 */
class OfsDataBlock(val physicalVolume: PhysicalVolume, val blockNumber: Int)
extends DataBlock with HasChecksum with SectorBasedChecksum {
  import OfsDataBlock._

  val sector = physicalVolume.sector(blockNumber)
  def primaryType   = sector.int32At(0)
  def headerKey     = sector.int32At(4)
  def seqNum        = sector.int32At(8)
  def dataSize      = sector.int32At(12)
  def nextDataBlock = sector.int32At(16)
  def nextDataBlock_=(nextBlock: Int) {
    sector.setInt32At(16, nextBlock)
  }
  def storedChecksum = sector.int32At(20)
  def computedChecksum = computeChecksum(20)
  def recomputeChecksum = sector.setInt32At(20, computedChecksum)

  def maxDataBytes = sector.sizeInBytes - HeaderSize

  def initialize(headerBlock: Int, seqnum: Int, size: Int) {
    for (i <- 0 until sector.sizeInBytes) sector(i) = 0
    sector.setInt32At(0, BlockType.PtData)
    sector.setInt32At(4, headerBlock)
    sector.setInt32At(8, seqnum)
    sector.setInt32At(12, size)
  }
  def dataBytes = {
    val result = new Array[Byte](sector.sizeInBytes - HeaderSize)
    for (i <- 0 until result.length) {
        result(i) = sector(i + HeaderSize).asInstanceOf[Byte]
    }
    result
  }
  def apply(index: Int) = sector(index + HeaderSize) & 0xff
  def update(index: Int, value: Int) = sector(index + HeaderSize) = (value & 0xff)
  def isOFS = true
  def isFFS = false
}

/**
 * FFS data blocks are plain arrays of byte values.
 * @constructor creates an FFS data block
 * @param physicalVolume a PhysicalVolume
 * @param blockNumber this block's block number
 */
class FfsDataBlock(val physicalVolume: PhysicalVolume, val blockNumber: Int)
extends DataBlock {
  val sector = physicalVolume.sector(blockNumber)

  def initialize {
    for (i <- 0 until sector.sizeInBytes) sector(i) = 0
  }
  def maxDataBytes = sector.sizeInBytes
  def dataBytes = {
    val result = new Array[Byte](sector.sizeInBytes)
    for (i <- 0 until sector.sizeInBytes) result(i) = sector(i).asInstanceOf[Byte]
    result
  }
  def apply(index: Int) = sector(index) & 0xff
  def update(index: Int, value: Int) = sector(index) = (value & 0xff)
  def isOFS = false
  def isFFS = true
}
