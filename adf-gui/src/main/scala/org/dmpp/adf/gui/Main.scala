package org.dmpp.adf.gui

import javax.swing._
import javax.swing.tree._
import javax.swing.event._
import java.awt.event._
import java.awt.BorderLayout
import java.awt.Dimension

import org.dmpp.adf.physical._
import org.dmpp.adf.logical._

class DirectoryTreeModel extends TreeModel {
  var listeners : List[TreeModelListener] = Nil
  def addTreeModelListener(listener: TreeModelListener) {
    listeners ::= listener
  }
  def getChild(parent: Object, index: Int): Object = {
    null
  }
  def getChildCount(parent: Object) = 0
  def getIndexOfChild(parent: Object, child: Object) = 0
  def getRoot: Object = "My Root"
  def isLeaf(node: Object) = false
  def removeTreeModelListener(listener: TreeModelListener) {
    listeners = listeners.filter(l => l != listener)
  }
  def valueForPathChanged(path: TreePath, newValue: Object) {
    println("valueForPathChanged()")
  }
}

/**
 * I originally wanted to use Scala Swing here, but there is no
 * official tree component (yet), which is necessary for a file viewer.
 * I just go with plain old Java Swing here.
 */
class AdfToolsFrame extends JFrame("ADF Tools") {
  val treeModel = new DirectoryTreeModel
  val tree = new JTree(treeModel)

  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  val scrollPane = new JScrollPane(tree)
  scrollPane.setPreferredSize(new Dimension(640, 480))
  getContentPane.add(scrollPane, BorderLayout.CENTER)
  pack
}

object Main {
  def main(args: Array[String]) {
    val frame = new AdfToolsFrame
    frame.setVisible(true)
  }
}
