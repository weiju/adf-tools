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
 * Trait to decorate blocks that are reading a BCPL string field.
 */
trait ReadsBcplStrings { self : HeaderBlock =>

  /**
   * Read the BCPL string at the specified position.
   * @param offset the data offset within the data
   * @param maxChars the maximum number of characters
   */
  def bcplStringAt(offset: Int, maxChars: Int) = {
    val nameLength = scala.math.min(sector(offset),
                                    maxChars)
    val builder = new StringBuilder
    for (i <- 0 until nameLength) {
      builder.append(sector(offset + 1 + i).asInstanceOf[Char])
    }
    builder.toString
  }
  def setBcplStringAt(offset: Int, maxChars: Int, str: String) = {
    val nameLength = scala.math.min(str.length, maxChars)
    sector(offset) = nameLength
    for (i <- 0 until nameLength) {
      sector(offset + 1 + i) = str.charAt(i).asInstanceOf[Byte]
    }
  }
}
