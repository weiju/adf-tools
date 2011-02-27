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
 * @param logicalVolume a [[org.dmpp.adf.logical.LogicalVolume]] instance
 * @param blockNumber the block number
 */
class DirectoryEntryBlock(logicalVolume: LogicalVolume,
                          blockNumber: Int)
extends HeaderBlock(logicalVolume, blockNumber) with HasComment
with HasAccessRights {
  /**
   * Indicates whether this block is a directory block.
   * @return true if directory block, false otherwise
   */
  def isDirectory  = secondaryType == BlockType.StUserDir

  /**
   * Indicates whether this block is a file header block.
   * @return true if file header block, false otherwise
   */
  def isFile       = secondaryType == BlockType.StFile

  /**
   * Initializes the data occupied by this block.
   * @param parentBlockNumber the parent directory's block number
   * @param aName the entry's name
   */
  def initialize(parentBlockNumber: Int, aName: String) {
    for (i <- 0 until sector.sizeInBytes) sector(i) = 0
    primaryType = BlockType.PtShort
    headerKey   = blockNumber
    parent      = parentBlockNumber
    name        = aName
  }
}
