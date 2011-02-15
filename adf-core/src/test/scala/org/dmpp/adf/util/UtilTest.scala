/**
 * Created on February 12, 2011
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
 * Test cases for utility classes.
 */
import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

class UtilTest extends JUnit4(UtilSpec)
object UtilSpecRunner extends ConsoleRunner(UtilSpec)

object MyBitHelper extends BitHelper


object UtilSpec extends Specification {

  "BitHelper" should {
    "determine the bits set in a bit mask" in {
      MyBitHelper.bitsSetIn(0) must_== Nil
      MyBitHelper.bitsSetIn(1) must_== List(31)
      MyBitHelper.bitsSetIn(6) must_== List(29, 30)
    }
    "determine the bits not set in a bit mask" in {
      MyBitHelper.bitsClearIn(0xffffffff) must_== Nil
      MyBitHelper.bitsClearIn(0xfffffffe) must_== List(31)
      MyBitHelper.bitsClearIn(0) must_==
        List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
             17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
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
  "UnsignedInt32" should {
    import UnsignedInt32Conversions._

    "make an UnsignedInt" in {
      val uint = UnsignedInt32(32)
      uint.value must_== 32l
    }
    "add two UnsignedInts" in {
      val uint = UnsignedInt32(32)
      val result = uint + 5
      result.value must_== 37l
      result.overflowOccurred must beFalse
    }
    "do a wrapping addition" in {
      val uint = UnsignedInt32(UnsignedInt32.MaxValue)
      val result = uint + 5
      result.value must_== 4l
      result.overflowOccurred must beTrue
    }
    "not allow negative values" in {
      UnsignedInt32(-42) must throwA[IllegalArgumentException]
    }
    "not allow values exceeding the 32 bit unsigned range" in {
      UnsignedInt32(4294967300l) must throwA[IllegalArgumentException]
    }
  }

  "AmigaDosDate" should {
    "represent its first date" in {
      formatted(AmigaDosDate(0, 0, 0).toDate) must_== "1978-01-01 00:00:00.000"
    }
    "add a couple of ticks" in {
      formatted(AmigaDosDate(0, 0, 50).toDate) must_== "1978-01-01 00:00:01.000"
      formatted(AmigaDosDate(0, 0, 102).toDate) must_== "1978-01-01 00:00:02.040"
    }
    "add a couple of minutes" in {
      formatted(AmigaDosDate(0, 42, 0).toDate) must_== "1978-01-01 00:42:00.000"
      formatted(AmigaDosDate(0, 210, 0).toDate) must_== "1978-01-01 03:30:00.000"
    }
    "add a couple of days" in {
      formatted(AmigaDosDate(3, 0, 0).toDate) must_== "1978-01-04 00:00:00.000"
      formatted(AmigaDosDate(367, 0, 0).toDate) must_== "1979-01-03 00:00:00.000"
    }
  }

  def formatted(date: Date) = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dateFormat.format(date)
  }
}
