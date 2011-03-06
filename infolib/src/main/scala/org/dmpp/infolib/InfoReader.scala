/**
 * Created on March 6, 2011
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
package org.dmpp.infolib

import java.nio._
import java.awt.Image
import java.awt.image._

/**
 * Data structure for an Amiga .info icon.
 * @constructor creates an instance of AmigaIcon
 * @param normalImage normal state image
 * @param highlightImage highlight state image
 */
case class AmigaIcon(normalImage: Image, highlightImage: Option[Image])

/**
 * Useful constants for the reaer.
 */
object AmigaInfoReader {
  /** The Workbench 1.x palette */
  val Palette_1_x = Array(0xff145aab, 0xffffffff, 0xff000000, 0xfff28e28)

  /** The Workbench 2.x palette */
  val Palette_2_x = Array(0xffaaaaaa, 0xff000000, 0xffffffff, 0xff6b8bb9)
}

/**
 * A reader class that can create icons based on a given Palette. A palette
 * is simply an array of integer values that represent (a, r, g, b) components.
 * When combining the icon's bitplanes, the palette is used to associate a
 * color value with an index. Such values are usually defined in the Workbench
 * preferences. The choice of palette colors is crucial to receive genuine
 * looking icons. Note that the standard Amiga Workbench resolution used to
 * be 640x240/640x256 respectively and most icons were designed with this
 * aspect ratio in mind. On today's systems these icons look "squeezed",
 * so scaling the icon by factor 2 in y-direction will usually yield a
 * look that is closer to the classic Workbench icon look.
 * The AmigaInfoReader companion object contains two standard palettes that
 * were taken from the Workbench 1.x and 2.x settings. 2.04 was the last
 * operating system version I used, so for the moment, these are the only
 * ones.
 */
class AmigaInfoReader(palette: Array[Int]) {

  /**
   * Create an icon from a byte array. Will report an error if the
   * data does not define an .info file (i.e. the magic number does not match)
   * @param data byte array containing the info data
   * @return an AmigaIcon instance generated from the data
   */
  def createIcon(data: Array[Byte]): AmigaIcon = {
    val buffer = ByteBuffer.wrap(data)
    val diskObject = readDiskObject(buffer)
    if (!diskObject.isMagicValid) {
      throw new IllegalArgumentException("invalid info file - magic number not found")
    }

    if (diskObject.hasDrawerData) {
      val drawerData = readDrawerData(buffer)
    }
    val imageHeader1 = readImageInfo(buffer)
    val img1 = readImageData(imageHeader1, buffer)
    // optional read second image
    val img2 = if (diskObject.gadget.hasHighlightImage) {
      val imageHeader2 = readImageInfo(buffer)
      Some(readImageData(imageHeader2, buffer))
    } else None
    AmigaIcon(img1, img2)
  }

  private def readByteAndSkipPad(buffer: ByteBuffer) = {
    val result = buffer.get
    buffer.get // ignore pad byte
    result
  }

  private def readDiskObject(buffer: ByteBuffer) = {
    DiskObject(magic = buffer.getChar,
               version = buffer.getShort,
               gadget  = readGadget(buffer),
               objectType  = readByteAndSkipPad(buffer),
               defaultTool = buffer.getInt,
               toolTypes   = buffer.getInt,
               currentX    = buffer.getInt,
               currentY    = buffer.getInt,
               drawerData  = buffer.getInt,
               toolWindow  = buffer.getInt,
               stackSize   = buffer.getInt
             )
  }
  private def readGadget(buffer: ByteBuffer) = {
    Gadget(next   = buffer.getInt,
           left   = buffer.getShort,
           top    = buffer.getShort,
           width  = buffer.getShort,
           height = buffer.getShort,
           flags  = buffer.getShort,
           activation    = buffer.getShort,
           gadgetType    = buffer.getShort,
           gadgetRender  = buffer.getInt,
           selectRender  = buffer.getInt,
           text          = buffer.getInt,
           mutualExclude = buffer.getInt,
           specialInfo   = buffer.getInt,
           id            = buffer.getShort,
           userData      = buffer.getInt
         )
  }

  private def readDrawerData(buffer: ByteBuffer) = {
    DrawerData(newWindow = readNewWindow(buffer),
               currentX  = buffer.getInt,
               currentY  = buffer.getInt)
  }

  private def readNewWindow(buffer: ByteBuffer) = {
    NewWindow(leftEdge    = buffer.getShort,
              topEdge     = buffer.getShort,
              width       = buffer.getShort,
              height      = buffer.getShort,
              detailPen   = buffer.get,
              blockPen    = buffer.get,
              idcmpFlags  = buffer.getInt,
              flags       = buffer.getInt,
              firstGadget = buffer.getInt,
              checkMark   = buffer.getInt,
              title       = buffer.getInt,
              screen      = buffer.getInt,
              bitmap      = buffer.getInt,
              minWidth    = buffer.getShort,
              minHeight   = buffer.getShort,
              maxWidth    = buffer.getShort,
              maxHeight   = buffer.getShort,
              screenType  = buffer.getShort
    )
  }

  private def readImageInfo(buffer: ByteBuffer) = {
    ImageInfo(leftEdge   = buffer.getShort,
              topEdge    = buffer.getShort,
              width      = buffer.getShort,
              height     = buffer.getShort,
              depth      = buffer.getShort,
              imageData  = buffer.getInt,
              planePick  = buffer.get,
              planeOnOff = buffer.get,
              nextImage  = buffer.getInt
    )
  }

  private def readImageData(image: ImageInfo,
                            buffer: ByteBuffer): BufferedImage = {
    val wordWidth = (image.width + 16) / 16
    //val numImageWords = wordWidth * image.height * image.depth
    val result = new BufferedImage(image.width, image.height,
                                   BufferedImage.TYPE_INT_ARGB)
    // make array with palette indexes
    val imgBytes = new Array[Byte](image.width * image.height)
    for (plane <- 0 until image.depth) {
      for (row <- 0 until image.height) {
        for (wordNumber <- 0 until wordWidth) {
          val currentWord: Int = buffer.getChar
          for (i <- 0 until 16) {
            val setmask = 1 << (15 - i)
            if ((currentWord & setmask) == setmask) {
              val x = wordNumber * 16 + i
              //result.setRGB(wordNumber * 16 + i, row, 0xff000000)
              imgBytes(row * image.width + x) =
                (imgBytes(row * image.width + x) |
                (1 << plane)).asInstanceOf[Byte]
            }
          }
        }
      }
    }

    // convert palette index array to rgb array
    for (y <- 0 until image.height) {
      for (x <- 0 until image.width) {
        val colorIndex = imgBytes(y * image.width + x)
        result.setRGB(x, y, palette(colorIndex))
      }
    }
    result
  }
}
