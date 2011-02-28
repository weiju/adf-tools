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
package org.dmpp.adf.app

import java.io._
import java.util.Date
import org.dmpp.adf.logical._

/**
 * File representation.
 * @constructor creates a UserFile instance
 * @param logicalVolume a LogicalVolume
 * @param fileHeaderBlock the file header block
 */
class UserFile(val logicalVolume: LogicalVolume,
               val fileHeaderBlock: FileHeaderBlock)
extends AbstractDosFile(fileHeaderBlock) {

  def isDirectory = false
  def isFile      = true
  def size        = fileHeaderBlock.fileSize

  override def lastModificationTime: Date = {
    fileHeaderBlock.lastModificationTime
  }

  /**
   * The data contained in this file.
   * @return data bytes
   */
  def dataBytes: Array[Byte] = {
    val dataBlockNums = fileHeaderBlock.dataBlockNumbers
    val result = new Array[Byte](size)
    var currentBytesCopied = 0
    for (blockNum <- 0 until dataBlockNums.length) {
      val blockdata = logicalVolume.dataBlock(dataBlockNums(blockNum)).dataBytes
      for (i <- 0 until blockdata.length) {
        result(currentBytesCopied) = blockdata(i)
        currentBytesCopied += 1
        if (currentBytesCopied == size) {
          if (blockNum < (dataBlockNums.length - 1)) {
            throw new IllegalStateException("not all blocks were copied")
          }
          return result
        }
      }
    }
    if (currentBytesCopied < size) {
      throw new IllegalStateException("retrieved less bytes than specified !!")
    }
    result
  }

  /**
   * Write this file's data to the selected OutputStream.
   * @param out the OutputStream to write to
   */
  def writeToOutputStream(out: OutputStream) = out.write(dataBytes)
}
