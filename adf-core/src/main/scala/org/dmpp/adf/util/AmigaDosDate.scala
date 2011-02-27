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

import java.text.SimpleDateFormat
import java.util.Date

/**
 * AmigaDOS date conversions.
 */
object AmigaDosDateConversions {
  import AmigaDosDate._

  /**
   * Convert a [[java.util.Date]] into an [[org.dmpp.adf.util.AmigaDosDate]].
   * @param date a Java date
   * @return an AmigaDosDate
   */
  implicit def date2AmigaDosDate(date: Date): AmigaDosDate = {
    val millisSinceBaseTime = date.getTime - BaseMillis
    val daysSinceBaseTime =
      (millisSinceBaseTime / MillisecondsPerDay).asInstanceOf[Int]
    var remainMillis = millisSinceBaseTime - (daysSinceBaseTime * MillisecondsPerDay)
    val minutesPastMidnight =
      (remainMillis / MillisecondsPerMinute).asInstanceOf[Int]
    remainMillis -= (minutesPastMidnight * MillisecondsPerMinute)
    val ticksPastLastMinute =
      (remainMillis / MillisecondsPerTick).asInstanceOf[Int]
    AmigaDosDate(daysSinceBaseTime, minutesPastMidnight, ticksPastLastMinute)
  }
}

/**
 * Constant definitions for Amiga DOS dates.
 */
object AmigaDosDate {
  val DateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
  val MillisecondsPerTick      = 20
  val MillisecondsPerMinute    = 1000 * 60
  val MillisecondsPerDay: Long = 1000 * 60 * 60 * 24
  val BaseMillis = DateFormat.parse("1978-01-01 00:00:00.000").getTime
}

/**
 * AmigaDOS dates start at January 1st, 1978, rather than Java dates, which
 * start at January 1st, 1970.
 * Ticks are measured in 1/50 of a second.
 *
 * @constructor creates an AmigaDOS date value with the specified arguments
 * @param daysSinceJan_1_78 days since January 1st, 1978
 * @param minutesPastMidnight minutes past 00:00
 * @param ticksPastLastMinute ticks elapsed since last minute
 */
case class AmigaDosDate(daysSinceJan_1_78: Int, minutesPastMidnight: Int,
                        ticksPastLastMinute: Int) {
  import AmigaDosDate._

  /**
   * Returns the Java Date equivalent of this AmigaDOS date
   * @return a java.util.Date representing this AmigaDOS date
   */
  def toDate: Date = {
    new Date(BaseMillis +
             daysToMillis(daysSinceJan_1_78) +
             minutesToMillis(minutesPastMidnight) +
             ticksToMillis(ticksPastLastMinute))
  }

  private def daysToMillis(days: Int)       = days * MillisecondsPerDay
  private def minutesToMillis(minutes: Int) = minutes * MillisecondsPerMinute
  private def ticksToMillis(ticks: Int)     = ticks * MillisecondsPerTick

  /**
   * String representation of this date.
   * @return string representation
   */
  override def toString = DateFormat.format(toDate)
}
