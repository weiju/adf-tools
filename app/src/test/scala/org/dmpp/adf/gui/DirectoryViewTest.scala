/**
 * Created on March 1, 2011
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

import java.util.Date
import org.scalatest.FlatSpec
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.dmpp.adf.app._

// we only need to mock out our files and directories since all we need
// is a view
class DosFileMock(val name: String) extends DosFile {
  def isDirectory   = false
  def isFile        = true
  def isRoot        = false
  def logicalVolume = null
  def name_=(newName: String) { }
  def comment       = ""
  def comment_=(newComment: String) { }
  def lastModificationTime = new Date
  def parentDirectory = null
  def delete { }
}

class DirectoryMock(name: String) extends DosFileMock(name) with Directory {
  override def isFile = false
  override def isDirectory = true
  def list: List[DosFile] = Nil
  def listDirectories: List[DosFile] = Nil
  def createFile(filename: String, dataBytes: Array[Byte]): UserFile = null
  def createDirectory(dirname: String): UserDirectory = null
  def find(filename: String): Option[DosFile] = None
}

@RunWith(classOf[JUnitRunner])
class DirectoryViewSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {
  var view: DirectoryView = null
  val mockDir = new DirectoryMock("mockdir") {
    override def list: List[DosFile] = {
      List(new DosFileMock("file2"),
           new DosFileMock("file1"),
           new DirectoryMock("dir1"),
           new DosFileMock("file1.info"),
           new DosFileMock("dir.info")
       )
    }
  }
  override def beforeEach {
    view = new DirectoryView
    view.currentDirectory = mockDir
    view.hideInfoFiles = false
  }

  "DirectoryView" should "wrap a UserDirectory" in {
    view.list.length should be === (mockDir.list.length)
  }
  it should "hide info files" in {
    view.hideInfoFiles = true
    val dirEntries = view.list
    dirEntries.length should be === (3)
    dirEntries.find(e => e.name == "file1") should not be (None)
    dirEntries.find(e => e.name == "file2") should not be (None)
    dirEntries.find(e => e.name == "dir1")  should not be (None)
  }
}
