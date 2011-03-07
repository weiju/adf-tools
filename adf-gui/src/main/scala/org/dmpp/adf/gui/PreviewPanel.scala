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

  private val infoPanel = new JPanel
  private val imagePanel = new JPanel(new FlowLayout)
  private val normalImageLabel = new JLabel("")
  private val highlightImageLabel = new JLabel("")
  private var stretchIcons = true
  private var currentInfoFile: UserFile = null
  private var currentIcon: AmigaIcon = null
  private var _currentPalette = Palette13

  def currentPalette = _currentPalette
  def currentPalette_=(palette: Array[Int]) {
    _currentPalette = palette
    rebuildIcons
  }

  setLayout(new BorderLayout)
  setBorder(new TitledBorder(new EtchedBorder, "Preview"))

  imagePanel.add(normalImageLabel)
  imagePanel.add(highlightImageLabel)

  add(infoPanel, BorderLayout.NORTH)
  add(imagePanel, BorderLayout.CENTER)

  def areIconsStretched = stretchIcons
  def toggleStretchIcons {
    stretchIcons = !stretchIcons
    updateImageLabels
  }

  def selectedFile: DosFile = null
  def selectedFile_=(file: DosFile) {
    resetIconImages
    if (file != null) {
      val infoFileOption =
        if (file.isRoot) file.asInstanceOf[Directory].find("Disk.info")
        else if (file.name.endsWith(".info")) Some(file)
        else file.parentDirectory.find(file.name + ".info")
      currentInfoFile = if (infoFileOption == None) null
                        else infoFileOption.get.asInstanceOf[UserFile]
      rebuildIcons
    }
  }

  private def rebuildIcons {
    if (currentInfoFile != null) setIconImages(currentInfoFile)
  }

  private def setIconImages(infoFile: UserFile) {
    try {
      val data = infoFile.dataBytes
      val infoReader = new AmigaInfoReader(currentPalette)
      currentIcon = infoReader.createIcon(data)
      updateImageLabels
    } catch {
      case e =>
        currentIcon = null
        e.printStackTrace
    }
  }

  private def updateImageLabels {
    setImageToLabel(currentIcon.normalImage, normalImageLabel)
    if (currentIcon.highlightImage != None) {
      setImageToLabel(currentIcon.highlightImage.get, highlightImageLabel)
    } else {
      highlightImageLabel.setIcon(null)
    }
  }

  private def setImageToLabel(image: Image, label: JLabel) {
    if (stretchIcons) {
      label.setIcon(new ImageIcon(image.getScaledInstance(image.getWidth(null),
                                                          image.getHeight(null) * 2,
                                                          Image.SCALE_FAST)))
    } else label.setIcon(new ImageIcon(image))
  }

  private def resetIconImages {
    normalImageLabel.setIcon(null)
    highlightImageLabel.setIcon(null)
  }
}
