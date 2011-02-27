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
 * Test cases for UnsignedInt32.
 */
class UnsignedInt32Test extends JUnit4(UnsignedInt32Spec)
object UnsignedInt32SpecRunner extends ConsoleRunner(UnsignedInt32Spec)

object UnsignedInt32Spec extends Specification {

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
}
