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

/**
 * Constants for DirectoryBlock class.
 */
object UsesHashtable {
  val OffsetHashtableSize = 12
  val OffsetHashtable     = 24
}

trait UsesHashtable { self : HeaderBlock =>

  import UsesHashtable._

  /**
   * Returns the size of the directory's hash table.
   * @return hash table size
   */
  def hashtableSize   = sector.int32At(OffsetHashtableSize)

  /**
   * Sets the hash table size.
   * @param newSize new hash table size
   */
  def hashtableSize_=(newSize:Int) = sector.setInt32At(OffsetHashtableSize, newSize)

  /**
   * Returns the list of all valid header blocks contained in this directory's
   * hash table.
   * @return all header blocks in the directory
   */
  def hashtableEntries: List[DirectoryEntryBlock] = {
    var result : List[DirectoryEntryBlock] = Nil
    val byteSize = hashtableSize * 4
    for (i <- 0 until byteSize by 4) {
      result = addToBucketRecursively(result, sector.int32At(OffsetHashtable + i))
    }
    result.reverse
  }

  private def addToBucketRecursively(addTo: List[DirectoryEntryBlock],
                                     blockNumber: Int): List[DirectoryEntryBlock] = {
    if (isNonEmptyHashEntry(blockNumber)) {
      val block = makeDirEntryBlock(blockNumber)
      addToBucketRecursively(block :: addTo, block.nextInHashBucket)
    } else addTo
  }
  private def isNonEmptyHashEntry(entry: Int) = entry > 0
  private def makeDirEntryBlock(blockNumber: Int) = {
    val header = new DirectoryEntryBlock(physicalVolume, blockNumber)
    header.secondaryType match {
      case BlockType.StUserDir =>
        new UserDirectoryBlock(physicalVolume, blockNumber)
      case BlockType.StFile    =>
        new FileHeaderBlock(physicalVolume, blockNumber)
      case _ =>
        throw new UnsupportedOperationException("unsupported secondary header type")
    }
  }

  /**
   * Returns a header block for a given file/directory name in this directory.
   * @param the file/directory name
   * @return the block name or 0 if not found
   */
  def blockForName(name: String): Option[DirectoryEntryBlock] = {
    val blockNumber = blockNumberForName(name)
    if (blockNumber == 0) None
    else Some(makeDirEntryBlock(blockNumber))
  }

  /**
   * Returns a block number for a given file/directory name in this directory.
   * @param the file/directory name
   * @return the block name or 0 if not found
   */
  def blockNumberForName(name: String): Int = {
    val hash = hashcodeForName(name)
    findBlockNumberForNameRecursively(name, blockAtHashtableIndex(hash))
  }
  private def blockAtHashtableIndex(index: Int): Int = {
    sector.int32At(OffsetHashtable + index * 4)
  }
  private def hashcodeForName(name: String): Int = {
    var hash = name.length
    for (i <- 0 until name.length) {
      hash *= 13
      hash += Character.toUpperCase(name(i))
      hash &= 0x7ff
    }
    hash % hashtableSize
  }
  private def findBlockNumberForNameRecursively(name: String,
                                                blocknum: Int): Int = {
    if (blocknum == 0) 0
    else {
      val current = new DirectoryEntryBlock(physicalVolume, blocknum)
      if (current.name == name) blocknum
      else {
        findBlockNumberForNameRecursively(name, current.nextInHashBucket)
      }
    }
  }
}
