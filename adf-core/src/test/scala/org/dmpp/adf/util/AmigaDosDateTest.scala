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

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

/**
 * Test cases for AmigaDOS dates.
 */
class AmigaDosDateTest extends JUnit4(AmigaDosDateSpec)
object AmigaDosDateSpecRunner extends ConsoleRunner(AmigaDosDateSpec)

object AmigaDosDateSpec extends Specification {

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

  "AmigaDosDateConversions" should {
    "convert the zero date" in {
      val date = dateFromString("1978-01-01 00:00:00.000")
      val amigaDate = AmigaDosDateConversions.date2AmigaDosDate(date)
      amigaDate.daysSinceJan_1_78 must_== 0
      amigaDate.minutesPastMidnight must_== 0
      amigaDate.ticksPastLastMinute must_== 0
    }
    "convert some milliseconds to ticks" in {
      val date = dateFromString("1978-01-01 00:00:00.100")
      val amigaDate = AmigaDosDateConversions.date2AmigaDosDate(date)
      amigaDate.daysSinceJan_1_78 must_== 0
      amigaDate.minutesPastMidnight must_== 0
      amigaDate.ticksPastLastMinute must_== 5
    }
    "convert some seconds" in {
      val date = dateFromString("1978-01-01 00:00:42.100")
      val amigaDate = AmigaDosDateConversions.date2AmigaDosDate(date)
      amigaDate.daysSinceJan_1_78 must_== 0
      amigaDate.minutesPastMidnight must_== 0
      amigaDate.ticksPastLastMinute must_== 2105
    }
    "convert some minutes" in {
      val date = dateFromString("1978-01-01 00:13:00.100")
      val amigaDate = AmigaDosDateConversions.date2AmigaDosDate(date)
      amigaDate.daysSinceJan_1_78 must_== 0
      amigaDate.minutesPastMidnight must_== 13
      amigaDate.ticksPastLastMinute must_== 5
    }
    "convert some days" in {
      val date = dateFromString("1978-01-06 00:13:00.100")
      val amigaDate = AmigaDosDateConversions.date2AmigaDosDate(date)
      amigaDate.daysSinceJan_1_78 must_== 5
      amigaDate.minutesPastMidnight must_== 13
      amigaDate.ticksPastLastMinute must_== 5
    }
  }

  def dateFromString(dateString: String) = {
    new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").parse(dateString)
  }
  def formatted(date: Date) = {
    new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(date)
  }
}
