/**
 * Created on February 19, 2011
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

import javax.swing.tree._
import javax.swing.event._
import org.dmpp.adf.app._

class DirectoryTreeModel extends TreeModel {
  var _volume: UserVolume = null
  var listeners : List[TreeModelListener] = Nil

  def volume = _volume
  def volume_=(aVolume: UserVolume) {
    _volume = aVolume
    val path : Array[Object] = Array(_volume)
    val event = new TreeModelEvent(this, path)
    fireTreeStructureChanged(event)
  }
  def fireTreeStructureChanged(event: TreeModelEvent) {
    listeners.foreach(_.treeStructureChanged(event))
  }

  def addTreeModelListener(listener: TreeModelListener) {
    listeners ::= listener
  }
  private def toDirectory(node: Object): Directory = {
    if (node == null) null
    else {
      node match {
        case vol:UserVolume    => vol.rootDirectory
        case dir:UserDirectory => dir
        case _ => null
      }
    }
  }
  def getChild(parent: Object, index: Int): Object = {
    val dir = toDirectory(parent)
    if (dir != null) dir.listDirectories(index)
    else null
  }
  def getChildCount(parent: Object) = {
    val dir = toDirectory(parent)
    if (dir != null) dir.listDirectories.length
    else 0
  }
  def getIndexOfChild(parent: Object, child: Object) = {
    val dir = toDirectory(parent)
    if (dir != null) {
      val dirEntries = dir.listDirectories
      dirEntries.indexOf(child)
    }
    else -1
  }
  def getRoot: Object = {
    if (_volume == null) "(No volume loaded)"
    else _volume
  }
  def isLeaf(node: Object) = false
  def removeTreeModelListener(listener: TreeModelListener) {
    listeners = listeners.filter(l => l != listener)
  }
  def valueForPathChanged(path: TreePath, newValue: Object) {
    println("valueForPathChanged()")
  }
}
