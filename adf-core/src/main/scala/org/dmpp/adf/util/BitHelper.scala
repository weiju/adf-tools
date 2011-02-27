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
package org.dmpp.adf.util

/**
 * Mixin for common bit operations.
 */
trait BitHelper {
  /**
   * Determines whether the specified flags are cleared.
   * 
   * @param flags the value to check for cleared flags
   * @param flag a bit mask with the flags to be checked set
   */
  def flagClear(flags: Int, flag: Int) = (flags & flag) == 0

  /**
   * Determines whether the specified flags are set.
   * 
   * @param flags the value to check for set flags
   * @param flag a bit mask with the flags to be checked set
   */
  def flagSet(flags: Int, flag: Int) = (flags & flag) == flag

  /**
   * Returns a list of bit numbers that are set in a given bit mask.
   * Note that the LSB in this case is number 0 and the MSB
   * is 31.
   *
   * @param bitmask the bit mask to check
   * @return list of bit numbers that are set
   */
  def bitsSetIn(bitmask: Int): List[Int] = {
    bitsFilter(bitmask, mask => ((mask & 1) == 1))
  }

  /**
   * Returns a list of bit numbers that are set in a given bit mask.
   * Note that the LSB in this case is number 0 and the MSB
   * is 31.
   *
   * @param bitmask the bit mask to check
   * @return list of bit numbers that are set
   */
  def bitsClearIn(bitmask: Int): List[Int] = {
    bitsFilter(bitmask, mask => ((mask & 1) == 0))
  }

  private def bitsFilter(bitmask: Int, pred: Int => Boolean): List[Int] = {
    var current = bitmask
    var result: List[Int] = Nil
    for (i <- 0 until 32) {
      if (pred(current)) result ::= i
      current >>>= 1
    }
    result.reverse
  }

  /**
   * Given four byte values (from most significant to least significant),
   * join them into a 32-bit integer value.
   */
  def makeInt32(byte0: Int, byte1: Int, byte2: Int, byte3: Int) = {
    ((byte0 << 24) & 0xff000000) | ((byte1 << 16) & 0xff0000) |
      ((byte2 << 8) & 0xff00) | (byte3 & 0xff)
  }

  /**
   * Given two byte values (from most significant to least significant),
   * join them into a 16-bit unsigned integer value.
   */
  def makeInt16(byte0: Int, byte1: Int) = ((byte0 << 8) & 0xff00) | (byte1 & 0xff)
}
