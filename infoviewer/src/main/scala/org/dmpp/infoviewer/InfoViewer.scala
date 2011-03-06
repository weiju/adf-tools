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
package org.dmpp.infoviewer

import java.io._
import javax.swing._
import java.awt._
import java.awt.event._
import java.awt.image._
import org.dmpp.infolib._

/**
 * Simple viewer component that can display a single AmigaIcon instance.
 * @constructor creates an InfoCanvas instance
 * @param icon an AmigaIcon instance to display
 */
class InfoCanvas(icon: AmigaIcon)
extends JComponent {
  setPreferredSize(new Dimension(640, 480))
  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    g.setColor(new Color(0x14, 0x5a, 0xab))
    g.fillRect(0, 0, 640, 480)
    g.drawImage(icon.normalImage, 0, 20, null)
    if (icon.highlightImage != None) {
      g.drawImage(icon.highlightImage.get, 100, 20, null)
    }
  }
}

/**
 * This is a very simple demo program that expects an Amiga .info
 * file as its first parameter and attempts to display it in a frame.
 */
object InfoViewer {

  def main(args: Array[String]) {    
    println("Info Reader Version 1.0")
    if (args.length == 0) {
      println("please provide the name of a valid Amiga .info file")
    } else {
      displayIcon(args(0))
    }
  }

  private def displayIcon(path: String) {
    val file = new File(path)
    val data = new Array[Byte](file.length.asInstanceOf[Int])
    var in: FileInputStream = null
    val infoReader = new AmigaInfoReader(AmigaInfoReader.Palette_1_x)
    try {
      in = new FileInputStream(file)
      in.read(data)
      displayFrameWith(infoReader.createIcon(data))
    } finally {
      if (in != null) in.close
    }
  }

  private def displayFrameWith(icon: AmigaIcon) {
    val frame = new JFrame("Info Viewer")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val canvas = new InfoCanvas(icon)
    frame.getContentPane.add(canvas, BorderLayout.CENTER)
    frame.pack
    frame.setVisible(true)
  }
}
