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
package org.dmpp.adf.gui

import java.io._
import javax.swing._
import javax.swing.border._
import java.awt._
import java.awt.event._
import org.dmpp.adf.app._
import org.dmpp.infolib._

object PreviewPanel {
  val Palette13 = Array(0xff145aab, 0xffffffff,
                        0xff000000, 0xfff28e28)
  val Palette20 = Array(0xffaaaaaa, 0xff000000,
                        0xffffffff, 0xff6b8bb9)
}

class PreviewPanel extends JPanel {
  import PreviewPanel._

  setLayout(new BorderLayout)
  setBorder(new TitledBorder(new EtchedBorder, "Preview"))
  val infoPanel = new JPanel
  val imagePanel = new JPanel(new FlowLayout)
  val normalImageLabel = new JLabel("")
  val highlightImageLabel = new JLabel("")
  imagePanel.add(normalImageLabel)
  imagePanel.add(highlightImageLabel)

  add(infoPanel, BorderLayout.NORTH)
  add(imagePanel, BorderLayout.CENTER)

  var currentPalette = Palette13

  def selectedFile: DosFile = null
  def selectedFile_=(file: DosFile) {
    if (file == null) {
      normalImageLabel.setIcon(null)
      highlightImageLabel.setIcon(null)
    } else {
      val infoFile = if (file.isRoot) {
        file.asInstanceOf[Directory].find("Disk.info")
      } else {
        file.parentDirectory.find(file.name + ".info")
      }
      if (infoFile != None) {
        val data = infoFile.get.asInstanceOf[UserFile].dataBytes
        val infoReader = new AmigaInfoReader(currentPalette)
        val icon = infoReader.createIcon(data)
        val icon1 = new ImageIcon(icon.normalImage)
        normalImageLabel.setIcon(icon1)
        if (icon.highlightImage != None) {
          val hlIcon = new ImageIcon(icon.highlightImage.get)
          highlightImageLabel.setIcon(hlIcon)
        } else {
          highlightImageLabel.setIcon(null)
        }
      } else {
        normalImageLabel.setIcon(null)
        highlightImageLabel.setIcon(null)
      }
    }
  }
}
