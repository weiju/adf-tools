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
import javax.swing.table._
import org.dmpp.adf.logical.HeaderBlock
import org.dmpp.adf.app._
import java.text.SimpleDateFormat

object DirectoryTableModel {
  val DateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm")
}

class DirectoryTableModel(frame: JFrame) extends AbstractTableModel {
  import DirectoryTableModel._
  
  val view = new DirectoryView

  def currentDir = view.currentDirectory
  def currentDir_=(dir: Directory) {
    view.currentDirectory = dir
    fireTableDataChanged
  }
  def hideInfoFiles = view.hideInfoFiles
  def toggleHideInfoFiles {
    view.hideInfoFiles = !view.hideInfoFiles
    fireTableDataChanged
  }

  def fileAt(index: Int): DosFile = view.list(index)

  val Headers = Array("Name", "Size", "Last Modified", "Kind")

  override def isCellEditable(row: Int, column: Int) = column == 0

  def getRowCount = {
    if (view.currentDirectory == null) 0 else view.list.length
  }

  override def getColumnClass(col: Int): Class[_] = {
    if (col == 1) classOf[java.lang.Integer]
    else super.getColumnClass(col)
  }

  def getColumnCount = Headers.length

  override def getColumnName(col: Int) = Headers(col)

  override def setValueAt(value: Object, row: Int, col: Int) {
    if (col == 0) {
      val newName = value.toString
      if (newName.length > HeaderBlock.NameMaxChars) {
        JOptionPane.showMessageDialog(frame,
                                      "File name length must not exceed 30 characters",
                                      "Input error",
                                      JOptionPane.ERROR_MESSAGE)
      } else {
        view.list(row).name = value.toString
      }
    }
  }
  def getValueAt(row: Int, col: Int): Object = {
    if (view.currentDirectory != null) {
      getValueForColumn(view.list(row), col)
    } else ""
  }

  private def getValueForColumn(file: DosFile, column: Int): Object = {
    column match {
      case 0 => file
      case 1 =>
        if (file.isDirectory) "-" else file.asInstanceOf[UserFile].size.toString
      case 2 =>
        val date  = if (file.isDirectory) file.lastModificationTime
        else file.lastModificationTime      
        DateFormat.format(date)
      case 3 => if (file.isDirectory) "Folder" else "File"
      case _ => "???"
    }
  }
}
