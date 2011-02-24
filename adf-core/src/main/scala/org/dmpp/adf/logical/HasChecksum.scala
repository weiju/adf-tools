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

import org.dmpp.adf.util._

/**
 * An interface to indicate an object that stores and computes a checksum
 */
trait HasChecksum {

  /**
   * Checks whether the currently stored checksum is valid
   */
  def checksumIsValid = storedChecksum == computedChecksum

  /**
   * Returns the currently stored checksum for this block.
   * @return the currently stored checksum
   */
  def storedChecksum: Int

  /** Recomputes this block's checksum. */
  def recomputeChecksum

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
trait SectorBasedChecksum { self : LogicalBlock =>

  /**
   * Standard checksum algorithm.
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
