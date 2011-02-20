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

import javax.swing.table._
import org.dmpp.adf.app._

class DirectoryTableModel extends AbstractTableModel {
  
  var _currentDir: Directory = null
  def currentDir = _currentDir
  def currentDir_=(dir: Directory) {
    _currentDir = dir
    fireTableDataChanged
  }
  
  val Headers = Array("Name", "Size", "Last Access/Modified", "Comment")
  def getRowCount = {
    if (_currentDir == null) 0 else _currentDir.list.length
  }
  override def getColumnClass(col: Int): Class[_] = {
    if (col == 1) classOf[java.lang.Integer]
    else super.getColumnClass(col)
  }
  def getColumnCount = Headers.length
  override def getColumnName(col: Int) = Headers(col)
  def getValueAt(row: Int, col: Int): Object = {
    if (_currentDir != null) {
      getValueForColumn(_currentDir.list(row), col)
    } else ""
  }
  private def getValueForColumn(file: DosFile, column: Int): Object = {
    column match {
      case 0 => file.name
      case 1 =>
        if (file.isDirectory) "-" else file.asInstanceOf[UserFile].size.toString
      case 2 =>
        if (file.isDirectory) file.lastAccessTime
        else file.lastModificationTime
      case 3 => file.comment
      case _ => "???"
    }
  }
}
