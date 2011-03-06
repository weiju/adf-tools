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

// defined in workbench.h
// Size of DiskObject header (including Gadget) = 34 + 44 = 78 bytes
object DiskObject {
  val DiskMagic    = 0xe310
  val DiskVersion  = 1
  val DiskRevision = 1

  val Disk    = 1
  val Drawer  = 2
  val Tool    = 3
  val Project = 4
  val Garbage = 5
  val Device  = 6
  val Kick    = 7
  val AppIcon = 8
}

case class DiskObject(
  magic       : Int,    // UWORD
  version     : Int,    // UWORD
  gadget      : Gadget, // struct Gadget
  objectType  : Int,    // UBYTE
  // pad byte ignored
  defaultTool : Int,    // char *
  toolTypes   : Int,    // char **
  currentX    : Int,    // LONG
  currentY    : Int,    // LONG
  drawerData  : Int,    // struct DrawerData *
  toolWindow  : Int,    // char *
  stackSize   : Int     // LONG
) {
  import DiskObject._

  def isMagicValid = magic == DiskMagic
  def typeName = {
    objectType match {
      case Disk    => "Disk"
      case Drawer  => "Drawer"
      case Tool    => "Tool"
      case Project => "Project"
      case Garbage => "Garbage"
      case Device  => "Device"
      case Kick    => "Kick"
      case AppIcon => "AppIcon"
      case _ => "unknown"
    }
  }
  def hasDrawerData: Boolean = {
    objectType == Disk || objectType == Drawer || objectType == Garbage
  }
}

// defined in workbench.h
// size = 48 + 8 = 56 bytes
case class DrawerData(
  newWindow : NewWindow,// struct NewWindow
  currentX  : Int,      // LONG
  currentY  : Int       // LONG
)

// defined in intuition.h
// size = 48 bytes
case class NewWindow(
  leftEdge    : Int,    // WORD
  topEdge     : Int,    // WORD
  width       : Int,    // WORD
  height      : Int,    // WORD
  detailPen   : Int,    // UBYTE
  blockPen    : Int,    // UBYTE
  idcmpFlags  : Int,    // ULONG
  flags       : Int,    // ULONG
  firstGadget : Int,    // struct Gadget *
  checkMark   : Int,    // struct Image *
  title       : Int,    // UBYTE *
  screen      : Int,    // struct Screen *
  bitmap      : Int,    // struct Bitmap *
  minWidth    : Int,    // WORD
  minHeight   : Int,    // WORD
  maxWidth    : Int,    // UWORD
  maxHeight   : Int,    // UWORD
  screenType  : Int     // UWORD
)

// defined in intuition.h
// size = 44 bytes
object Gadget {
  val HighlightBits  = 3
  val HighlightImage = 2
}

case class Gadget(
  next            : Int, // struct Gadget *
  left            : Int, // UWORD
  top             : Int, // UWORD
  width           : Int, // UWORD
  height          : Int, // UWORD
  flags           : Int, // UWORD
  activation      : Int, // UWORD
  gadgetType      : Int, // UWORD
  gadgetRender    : Int, // APTR
  selectRender    : Int, // APTR
  text            : Int, // struct IntuiText *
  mutualExclude   : Int, // LONG
  specialInfo     : Int, // APTR
  id              : Int, // UWORD
  userData        : Int) { // APTR
  
  import Gadget._
  def hasHighlightImage = (flags & HighlightBits) == HighlightImage
}

// defined in intuition.h
// size of Image header = 20 bytes
// describes a UWORD array of bitplane data
case class ImageInfo(
  leftEdge         : Int, // WORD
  topEdge          : Int, // WORD
  width            : Int, // WORD
  height           : Int, // WORD
  depth            : Int, // WORD
  imageData        : Int, // UWORD *
  planePick        : Int, // UBYTE
  planeOnOff       : Int, // UBYTE
  nextImage        : Int  // struct Image *
)
