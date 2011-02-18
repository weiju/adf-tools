/**
 * Created on February 17, 2011
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
package org.dmpp.adf.app

import java.io._

import org.dmpp.adf.logical._
import org.dmpp.adf.physical._


/**
 * A factory to create [[org.dmpp.adf.app.UserVolume]] instances.
 */
object UserVolumeFactory {
  /**
   * Create an empty, initialized DD disk.
   * @return empty user volume of DD size
   */
  def createEmptyDoubleDensityDisk(name: String = "Empty") = {
    new UserVolume(LogicalVolumeFactory.createEmptyDoubleDensityDisk(name))
  }

  /**
   * Reads a volume from a file.
   * @param file input file
   * @return user volume
   */
  def readFromFile(file: File): UserVolume = {
    var inputStream: FileInputStream = null
    try {
      inputStream = new FileInputStream(file)
      val physicalVolume = PhysicalVolumeFactory.readDoubleDensityDisk(inputStream)
      new UserVolume(new LogicalVolume(physicalVolume))
    } finally {
      if (inputStream != null) inputStream.close
    }
  }
}

/**
 * General interface of a file.
 */
trait DosFile {
  /**
   * Determine whether this file is a directory.
   * @return true if directory, false otherwise
   */
  def isDirectory: Boolean

  /**
   * Determine whether this file is a regular file.
   * @return true if data file, false otherwise
   */
  def isFile: Boolean

  /**
   * Returns the file name.
   * @return file name
   */
  def name: String
}
trait Directory extends DosFile {
  def isDirectory = true
  def isFile      = false
  def list: List[DosFile]
}

/**
 * A special directory class for the root directory, since it is not
 * based on a DirectoryEntryBlock.
 * @constructor creates a new RootDirectory instance
 * @param rootBlock a RootBlock instance
 */
class RootDirectory(rootBlock: RootBlock) extends Directory {
  def name = rootBlock.name
  def list: List[DosFile] = {
    val entries = rootBlock.hashtableEntries
    entries.map(e => e match {
      case file:FileHeaderBlock   => new UserFile(file)
      case dir:UserDirectoryBlock => new UserDirectory(dir)
      case unknown:Any =>
        throw new IllegalArgumentException("unknown block type: " + unknown.getClass)
    })
  }
}

/**
 * Abstract super class for directory entries.
 * @constructor creates a new AbstractDosFile instance
 * @param dirEntryBlock a DirectoryEntryBlock
 */
abstract class AbstractDosFile(dirEntryBlock: DirectoryEntryBlock) extends DosFile {
  def name = dirEntryBlock.name
}

/**
 * Disk directory representation.
 */
class UserDirectory(directoryBlock: UserDirectoryBlock)
extends AbstractDosFile(directoryBlock) with Directory {
  def list: List[DosFile] = {
    throw new UnsupportedOperationException("TODO")
  }
}

class UserFile(fileHeaderBlock: FileHeaderBlock)
extends AbstractDosFile(fileHeaderBlock) {
  def isDirectory = false
  def isFile      = true
}

/**
 * Application view to an Amiga file system.
 *
 * @constructor creates a UserVolume from a [[org.dmpp.adf.logical.LogicalVolume]]
 *   instance.
 * @param logicalVolume a LogicalVolume instance
 */
class UserVolume(logicalVolume: LogicalVolume) {
  /**
   * Return this volume's name.
   * @return this volume's name
   */
  def name = logicalVolume.rootBlock.name

  /**
   * Returns the root directory.
   * @return the root directory.
   */
  def rootDirectory: Directory = {
    new RootDirectory(logicalVolume.rootBlock)
  }

  /**
   * Given a path expression, select a list of [[org.dmpp.adf.app.DosFile]]
   * instances.
   * @param path a path string
   * @return a list of DosFile instances matching the path expression
   */
  def select(path: String): List[DosFile] = {
    throw new UnsupportedOperationException("TODO")
  }
}
