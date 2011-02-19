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
with UsesHashtable {
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
