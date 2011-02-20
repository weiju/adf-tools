/**
 * Created on February 18, 2011
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

import javax.swing._
import javax.swing.tree._
import javax.swing.event._
import java.awt.event._
import java.awt.BorderLayout
import java.awt.Dimension

import org.dmpp.adf.app._

/**
 * I originally wanted to use Scala Swing here, but there is no
 * official tree component (yet), which is necessary for a file viewer.
 * I just go with plain old Java Swing here.
 */
class AdfToolsFrame extends JFrame("ADF Tools") {

  var currentVolume: UserVolume = null

  val treeModel = new DirectoryTreeModel
  val tableModel = new DirectoryTableModel
  val tree = new JTree(treeModel)
  val table = new JTable(tableModel)
  val splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT)

  getContentPane.add(splitPane, BorderLayout.CENTER)
  splitPane.setPreferredSize(new Dimension(640, 480))
  splitPane.setDividerSize(3)

  makeLeftPane
  makeRightPane
  makeMenuBar
  tree.addTreeSelectionListener(new TreeSelectionListener {
    def valueChanged(e: TreeSelectionEvent) {
      val path = e.getPath
      val dir: Directory = path.getLastPathComponent match {
        case v:UserVolume => v.rootDirectory
        case d:UserDirectory => d
        case _ => null
      }
      tableModel.currentDir = dir
    }
  })

  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  addWindowListener(new WindowAdapter {
    override def windowOpened(event: WindowEvent) {
      splitPane.setDividerLocation(0.3)
    }
  })
  pack

  private def makeLeftPane {
    val scrollPane = new JScrollPane(tree)
    splitPane.setLeftComponent(scrollPane)
  }

  private def makeRightPane {
    val scrollPane = new JScrollPane(table)
    splitPane.setRightComponent(scrollPane)
  }

  private def makeMenuBar {
    val menubar = new JMenuBar
    val fileMenu = new JMenu("File")
    menubar.add(fileMenu)

    addMenuItem(fileMenu, "New Volume",
                new ActionListener {
                  def actionPerformed(e: ActionEvent) = newFfsVolume
                })
    addMenuItem(fileMenu, "Open ADF file...",
                new ActionListener {
                  def actionPerformed(e: ActionEvent) = openAdfFile
                })
    addMenuItem(fileMenu, "Save as ADF file...",
                new ActionListener {
                  def actionPerformed(e: ActionEvent) = saveVolumeAs
                })
    addMenuItem(fileMenu, "Export selected file...",
                new ActionListener {
                  def actionPerformed(e: ActionEvent) = saveVolumeAs
                })
    if (!isMacOsX) {
      addMenuItem(fileMenu, "Quit",
                  new ActionListener {
                    def actionPerformed(e: ActionEvent) = System.exit(0)
                  })
    }

    setJMenuBar(menubar)
  }

  private def addMenuItem(menu: JMenu, caption: String,
                          listener: ActionListener) {
    val item = new JMenuItem(caption)
    item.addActionListener(listener)
    menu.add(item)
  }

  private def isMacOsX: Boolean = System.getProperty("mrj.version") != null

  private def openAdfFile {
    val fileChooser = new JFileChooser
    fileChooser.setDialogTitle("Open ADF file...")
    fileChooser.setMultiSelectionEnabled(false)
    if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
      setCurrentVolume(UserVolumeFactory.readFromFile(fileChooser.getSelectedFile))
    }
  }
  private def newFfsVolume {
    setCurrentVolume(UserVolumeFactory.createEmptyDoubleDensityDisk())
  }
  private def setCurrentVolume(volume: UserVolume) {
    currentVolume         = volume
    treeModel.volume      = currentVolume
    tableModel.currentDir = null
  }
  private def saveVolumeAs {
    println("TODO")
  }
}

object Main {
  def main(args: Array[String]) {
    System.setProperty("apple.laf.useScreenMenuBar", "true")
    System.setProperty("com.apple.mrj.application.apple.menu.about.name", "ADF Tools")
    val frame = new AdfToolsFrame
    frame.setVisible(true)
  }
}
