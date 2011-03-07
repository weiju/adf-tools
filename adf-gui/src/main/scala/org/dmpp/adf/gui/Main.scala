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

import java.io._
import javax.swing._
import javax.swing.table._
import javax.swing.tree._
import javax.swing.event._
import java.awt.event._
import java.awt.{BorderLayout, Dimension, FlowLayout}

import org.dmpp.adf.app._
import org.dmpp.adf.logical._

class FileTableCellRenderer extends DefaultTableCellRenderer {
  val FolderIcon = new ImageIcon(getClass.getResource("/folder_icon.png"))
  val FileIcon = new ImageIcon(getClass.getResource("/document_icon.png"))

  override def getTableCellRendererComponent(table: JTable, value: Object,
                                             isSelected: Boolean, hasFocus: Boolean,
                                             row: Int, column: Int) = {
    super.getTableCellRendererComponent(table, value, isSelected, hasFocus,
                                        row, column)
    val file = value.asInstanceOf[DosFile]
    try {
      if (file.isDirectory) setIcon(FolderIcon) else setIcon(FileIcon)
    } catch {
      case e => e.printStackTrace
    }
    setName(file.name)
    this
  }
}

/**
 * I originally wanted to use Scala Swing here, but there is no
 * official tree component (yet), which is necessary for a file viewer.
 * I just go with plain old Java Swing here.
 */
class AdfToolsFrame extends JFrame("Opus@Scala 1.0 beta") {

  var currentVolume: UserVolume = null
  var currentDir: Directory = null
  var saveAsItem:    JMenuItem = null
  var addFileItem:   JMenuItem = null
  var addFolderItem: JMenuItem = null
  var exportItem:    JMenuItem = null
  var deleteItem:    JMenuItem = null

  var statusMessageLabel: JLabel = null
  var lastSaveDirectory: File = null
  var lastOpenDirectory: File = null
  var lastImportDirectory: File = null

  val treeModel = new DirectoryTreeModel
  val tableModel = new DirectoryTableModel(this)
  val tree = new JTree(treeModel)
  val table = new JTable(tableModel)
  val previewPanel = new PreviewPanel
  val splitPaneH = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT)
  val splitPaneV = new JSplitPane(JSplitPane.VERTICAL_SPLIT)
  splitPaneH.setRightComponent(splitPaneV)

  getContentPane.add(splitPaneH, BorderLayout.CENTER)
  splitPaneH.setPreferredSize(new Dimension(640, 480))
  splitPaneH.setDividerSize(3)
  splitPaneV.setDividerSize(3)

  makeLeftPane
  makeRightPane
  makeMenuBar
  makeStatusBar
  addEventHandlers

  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  addWindowListener(new WindowAdapter {
    override def windowOpened(event: WindowEvent) {
      splitPaneH.setDividerLocation(0.3)
      splitPaneV.setDividerLocation(0.5)
    }
  })
  pack

  private def addEventHandlers {
    tree.addTreeSelectionListener(new TreeSelectionListener {
      def valueChanged(e: TreeSelectionEvent) {
        val path = e.getPath
        val dir: Directory = path.getLastPathComponent match {
          case v:UserVolume => v.rootDirectory
          case d:UserDirectory => d
          case _ => null
        }
        setCurrentDir(dir)
      }
    })
    table.getSelectionModel.addListSelectionListener(new ListSelectionListener {
      def valueChanged(e: ListSelectionEvent) {
        if (!e.getValueIsAdjusting) {
          val selectedRow = table.getSelectedRow
          if (selectedRow >= 0 && selectedRow < tableModel.getRowCount) {
            handleTableSelection(tableModel.fileAt(selectedRow))
          }
        }
      }
    })
  }

  private def handleTableSelection(currentSelection: DosFile) {
    exportItem.setEnabled(currentSelection != null && currentSelection.isFile)
    deleteItem.setEnabled(currentSelection != null)
    previewPanel.selectedFile = currentSelection
  }

  private def makeLeftPane {
    val scrollPane = new JScrollPane(tree)
    splitPaneH.setLeftComponent(scrollPane)
  }

  private def makeRightPane {
    table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    val scrollPane = new JScrollPane(table)
    table.getColumnModel.getColumn(0).setCellRenderer(new FileTableCellRenderer)
    splitPaneV.setTopComponent(scrollPane)
    splitPaneV.setBottomComponent(previewPanel)
  }

  private def makeMenuBar {
    val menubar = new JMenuBar
    setJMenuBar(menubar)
    makeFileMenu(menubar)
    makeEditMenu(menubar)
    makeViewMenu(menubar)
  }

  private def makeFileMenu(menubar: JMenuBar) {
    val fileMenu = new JMenu("File")
    menubar.add(fileMenu)

    addMenuItem(fileMenu, "New Volume",
                new ActionListener {
                  def actionPerformed(e: ActionEvent) = newFfsVolume
                })
    addFolderItem = addMenuItem(fileMenu, "New Folder",
                                new ActionListener {
                                  def actionPerformed(e: ActionEvent) = newFolder
                                })
    addMenuItem(fileMenu, "Open ADF file...",
                new ActionListener {
                  def actionPerformed(e: ActionEvent) = openAdfFile
                })
    saveAsItem = addMenuItem(fileMenu, "Save as ADF file...",
                             new ActionListener {
                               def actionPerformed(e: ActionEvent) = saveVolumeAs
                             })
    addFileItem = addMenuItem(fileMenu, "Add file...",
                              new ActionListener {
                                def actionPerformed(e: ActionEvent) = addFile
                              })
    exportItem = addMenuItem(fileMenu, "Export selected file...",
                             new ActionListener {
                               def actionPerformed(e: ActionEvent) = exportSelectedFile
                             })
    if (!isMacOsX) {
      addMenuItem(fileMenu, "Quit",
                  new ActionListener {
                    def actionPerformed(e: ActionEvent) = System.exit(0)
                  })
    }

    saveAsItem.setEnabled(false)
    addFileItem.setEnabled(false)
    exportItem.setEnabled(false)
    addFolderItem.setEnabled(false)
  }

  private def makeEditMenu(menubar: JMenuBar) {
    val editMenu = new JMenu("Edit")
    menubar.add(editMenu)
    deleteItem = addMenuItem(editMenu, "Delete",
                             new ActionListener {
                               def actionPerformed(e: ActionEvent) = deleteSelected
                             })
    deleteItem.setEnabled(false)    
  }

  private def makeViewMenu(menubar: JMenuBar) {
    val viewMenu = new JMenu("View")
    menubar.add(viewMenu)
    val hideInfoItem = addCheckBoxMenuItem(viewMenu, "Hide .info files",
                                           tableModel.hideInfoFiles,
                                           new ActionListener {
                                             def actionPerformed(e: ActionEvent) {
                                               tableModel.toggleHideInfoFiles
                                             }
                                           })
    viewMenu.addSeparator
    val palette1xItem = addCheckBoxMenuItem(viewMenu, "Use 1.x palette", true,
                                            new ActionListener {
                                              def actionPerformed(e: ActionEvent) { }
                                            })
    val palette2xItem = addCheckBoxMenuItem(viewMenu, "Use 2.x palette", false,
                                            new ActionListener {
                                              def actionPerformed(e: ActionEvent) { }
                                            })
    val stretchIconsItem = addCheckBoxMenuItem(viewMenu, "Stretch icons", true,
                                               new ActionListener {
                                                 def actionPerformed(e: ActionEvent) { }
                                               })
  }

  private def makeStatusBar {
    val statusbar = new JPanel(new FlowLayout(FlowLayout.LEFT))
    getContentPane.add(statusbar, BorderLayout.SOUTH)
    statusMessageLabel = new JLabel("(no volume)")
    statusbar.add(statusMessageLabel)
  }

  private def addMenuItem(menu: JMenu, caption: String,
                          listener: ActionListener): JMenuItem = {
    val item = new JMenuItem(caption)
    item.addActionListener(listener)
    menu.add(item)
    item
  }
  private def addCheckBoxMenuItem(menu: JMenu, caption: String, checked: Boolean,
                          listener: ActionListener): JMenuItem = {
    val item = new JCheckBoxMenuItem(caption, checked)
    item.addActionListener(listener)
    menu.add(item)
    item
  }

  private def isMacOsX: Boolean = System.getProperty("mrj.version") != null

  private def openAdfFile {
    val fileChooser = if (lastOpenDirectory == null) new JFileChooser
                      else new JFileChooser(lastOpenDirectory)

    fileChooser.setDialogTitle("Open ADF file...")
    fileChooser.setMultiSelectionEnabled(false)
    if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
      lastOpenDirectory = fileChooser.getSelectedFile.getParentFile
      setCurrentVolume(UserVolumeFactory.readFromFile(fileChooser.getSelectedFile))
    }
  }
  private def newFfsVolume {
    setCurrentVolume(UserVolumeFactory.createEmptyDoubleDensityDisk("Empty", "OFS"))
  }
  private def setCurrentVolume(volume: UserVolume) {
    currentVolume         = volume
    treeModel.volume      = currentVolume
    setCurrentDir(null)

    saveAsItem.setEnabled(currentVolume != null)
    updateStatusbar
  }

  private def updateStatusbar {
    if (currentVolume != null) {
      statusMessageLabel.setText(("%s, %d of %d blocks used, %d bytes free, " +
                                  "%d bytes used").format(currentVolume.filesystemType,
                                                          currentVolume.numUsedBlocks,
                                                          currentVolume.numBlocksTotal,
                                                          currentVolume.numBytesAvailable,
                                                          currentVolume.numBytesUsed))
    }
  }

  private def setCurrentDir(dir: Directory) {
    currentDir = dir
    tableModel.currentDir = dir

    previewPanel.selectedFile = if (dir == null && currentVolume != null) {
      currentVolume.rootDirectory
    } else dir

    addFileItem.setEnabled(currentDir != null)
    exportItem.setEnabled(false)
    addFolderItem.setEnabled(currentDir != null)
  }

  private def saveVolumeAs {
    genericSaveAs("Save as ADF file...", out => currentVolume.writeToOutputStream(out))

  }

  private def genericSaveAs(dialogTitle: String, writeFunc : (OutputStream => Unit)) {
    val fileChooser = if (lastSaveDirectory == null) new JFileChooser
                      else new JFileChooser(lastSaveDirectory)
    fileChooser.setDialogTitle(dialogTitle)
    fileChooser.setMultiSelectionEnabled(false)
    if (fileChooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {
      val file = fileChooser.getSelectedFile
      if (!file.exists || confirmOverwrite(file)) {
        lastSaveDirectory = file.getParentFile
        var out: FileOutputStream = null
        try {
          out = new FileOutputStream(file)
          writeFunc(out)
        } finally {
          if (out != null) out.close
        }
      }
    }
  }

  private def confirmOverwrite(file: File): Boolean = {
    JOptionPane.showConfirmDialog(this, ("The file '%s' exists already. Do you " +
                                  "want to overwrite it ?").format(file.getName),
                                  "Overwrite existing file",
                                  JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION
  }

  private def addFile {
    val fileChooser = if (lastImportDirectory == null) new JFileChooser
                      else new JFileChooser(lastImportDirectory)

    fileChooser.setDialogTitle("Add file...")
    fileChooser.setMultiSelectionEnabled(false)
    if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
      val selectedFile = fileChooser.getSelectedFile
      lastImportDirectory = selectedFile.getParentFile
      val fileSize = selectedFile.length.asInstanceOf[Int]
      // TODO: normalize filename to 30 characters/Amiga encoding
      var in: FileInputStream = null
      try {
        in = new FileInputStream(selectedFile)
        val dataBytes = new Array[Byte](fileSize)
        in.read(dataBytes)
        currentDir.createFile(selectedFile.getName, dataBytes)
        tableModel.fireTableDataChanged
        updateStatusbar
      } finally {
        if (in != null) in.close
      }
    }
  }
  private def exportSelectedFile {
    val fileToExport = currentDir.list(table.getSelectedRow).asInstanceOf[UserFile]
    genericSaveAs("Export selected file...",
                  out => fileToExport.writeToOutputStream(out))
  }

  private def newFolder {
    currentDir.createDirectory(makeUniqueFolderName)
    tableModel.fireTableDataChanged
    treeModel.fireTreeStructureChanged
  }
  private def makeUniqueFolderName = {
    "New Directory"
  }

  private def deleteSelected {
    val fileToDelete = currentDir.list(table.getSelectedRow)
    if (confirmDelete(fileToDelete)) {
      fileToDelete.delete
      tableModel.fireTableDataChanged
    }
  }
  private def confirmDelete(file: DosFile): Boolean = {
    JOptionPane.showConfirmDialog(this, ("Do you want to delete the file '%s'" +
                                         " ?").format(file.name),
                                  "Overwrite existing file",
                                  JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION
  }
}

object Main {
  def main(args: Array[String]) {
    System.setProperty("apple.laf.useScreenMenuBar", "true")
    System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Opus@Scala")
    val frame = new AdfToolsFrame
    frame.setVisible(true)
  }
}
