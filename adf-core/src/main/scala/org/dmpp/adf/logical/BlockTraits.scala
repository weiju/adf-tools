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

import org.dmpp.adf.util._
import org.dmpp.adf.physical._

/**
 * Interface to define access to the sector and the physical
 * volume.
 */
trait SectorBased {

  /**
   * Returns the physical volume.
   * @return the underlying physical volume.
   */
  def physicalVolume: PhysicalVolume

  /**
   * Returns the sector this block is based on.
   * @return the Sector instance
   */
  def sector: Sector
}

/**
 * An interface to indicate an object that stores and computes a checksum
 */
trait HasChecksum {

  /**
   * Returns the currently stored checksum for this block.
   * @return the currently stored checksum
   */
  def storedChecksum: Int

  /**
   * Computes a checksum in this block.
   * @return the checksum based on this object' data
   */
  def computedChecksum: Int
}

/**
 * A mixin trait which provides the standard checksum algorithm. This
 * algorithm can be used for all blocks except the boot block.
 */
trait SectorBasedChecksum extends SectorBased {

  /**
   * Standard checksum algorithm.
   *
   * @param checksumFieldOffset the offset of the field containing the stored
   *        checksum
   * @return the checksum based on this object' data
   */
  def computeChecksum(checksumFieldOffset: Int): Int = {
    import UnsignedInt32Conversions._

    var sum: UnsignedInt32 = 0
    for (i <- 0 until sector.sizeInBytes by 4) {
      if (i != checksumFieldOffset) { // ignore the checksum field
        sum += (sector.int32At(i) & 0xffffffffl)
      }
    }
    -sum.intValue
  }
}

trait HasAccessRights extends SectorBased {
  def uid: Int = sector.int16At(sector.sizeInBytes - 196)
  def gid: Int = sector.int16At(sector.sizeInBytes - 194)
}

trait ReadsBcplStrings extends SectorBased {

  def bcplStringAt(offset: Int, maxChars: Int) = {
    val nameLength = scala.math.min(sector(offset),
                                    maxChars)
    val builder = new StringBuilder
    for (i <- 0 until nameLength) {
      builder.append(sector(offset + 1 + i).asInstanceOf[Char])
    }
    builder.toString
  }
}

object HasComment {
  val CommentMaxChars = 79
}

trait HasComment extends ReadsBcplStrings {

  import HasComment._

  /**
   * Returns the comment field stored in this block.
   *
   * @return this block's comment
   */
  def comment: String = bcplStringAt(sector.sizeInBytes - 184, CommentMaxChars)
}

/**
 * Constants for DirectoryBlock class.
 */
object DirectoryLike {
  val OffsetHashtableSize = 12
  val OffsetHashtable     = 24
}

trait DirectoryLike extends SectorBased {
  import DirectoryLike._

  /**
   * Returns the size of the directory's hash table.
   *
   * @return hash table size
   */
  def hashtableSize   = sector.int32At(OffsetHashtableSize)

  /**
   * Returns the list of all valid header blocks contained in this directory's
   * hash table.
   *
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
      val block = new DirectoryEntryBlock(physicalVolume, blockNumber)
      addToBucketRecursively(block :: addTo, block.nextInHashBucket)
    } else addTo
  }
  private def isNonEmptyHashEntry(entry: Int) = entry > 0

  /**
   * Returns a header block for a given file/directory name in this directory.
   *
   * @param the file/directory name
   * @return the block name or 0 if not found
   */
  def blockForName(name: String): DirectoryEntryBlock = {
    val blockNumber = blockNumberForName(name)
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
   * Returns a block number for a given file/directory name in this directory.
   *
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
