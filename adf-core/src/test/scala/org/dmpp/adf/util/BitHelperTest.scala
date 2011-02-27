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

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

/**
 * Test cases for BitHelper.
 */
class BitHelperTest extends JUnit4(BitHelperSpec)
object BitHeperSpecRunner extends ConsoleRunner(BitHelperSpec)

object MyBitHelper extends BitHelper

object BitHelperSpec extends Specification {

  "BitHelper" should {
     "determine the bits set in a bit mask" in {
      MyBitHelper.bitsSetIn(0) must_== Nil
      MyBitHelper.bitsSetIn(1) must_== List(0)
      MyBitHelper.bitsSetIn(6) must_== List(1, 2)
    }
    "determine the bits not set in a bit mask" in {
      MyBitHelper.bitsClearIn(0xffffffff) must_== Nil
      MyBitHelper.bitsClearIn(0xfffffffe) must_== List(0)
      MyBitHelper.bitsClearIn(0) must_==
        List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
             17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
      MyBitHelper.bitsClearIn(0x0000ffff) must_==
        List(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
    }
    "make an unsigned 32 bit int" in {
      MyBitHelper.makeInt32(0xff, 0xfe, 0xfd, 0xfc) must_== 0xfffefdfc
      MyBitHelper.makeInt32(0x10, 0xfe, 0x20, 0xff) must_== 0x10fe20ff
    }
    "make an unsigned 16 bit int" in {
      MyBitHelper.makeInt16(0xff, 0xfe) must_== 0xfffe
      MyBitHelper.makeInt16(0x10, 0xfe) must_== 0x10fe
    }
  }
}
