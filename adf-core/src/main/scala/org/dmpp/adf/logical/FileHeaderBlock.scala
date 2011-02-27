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

/**
 * Constants for FileHeaderBlock
 */
object FileHeaderBlock {
  val OffsetBlockCount     = 8
  val OffsetFirstDataBlock = 16
}

/**
 * A class to represent a file header block.
 * @constructor creates a FileHeader block
 * @param logicalVolume the containing LogicalVolume
 * @param blockNumber this block's number within the volume
 */
class FileHeaderBlock(logicalVolume: LogicalVolume, blockNumber: Int)
extends DirectoryEntryBlock(logicalVolume, blockNumber) {
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

  override def initialize(parentBlock: Int, aName: String) {
    super.initialize(parentBlock, aName)
    secondaryType = BlockType.StFile
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
}
