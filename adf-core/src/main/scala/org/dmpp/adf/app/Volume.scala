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
import java.util.Date

import org.dmpp.adf.logical._
import org.dmpp.adf.physical._


/**
 * A factory to create [[org.dmpp.adf.app.UserVolume]] instances.
 */
object UserVolumeFactory {
  /**
   * Create an empty, initialized DD disk.
   * @param name disk name
   * @param filesystemType either "OFS" or "FFS", default is "FFS"
   * @return empty user volume of DD size
   */
  def createEmptyDoubleDensityDisk(name: String = "Empty",
                                   filesystemType: String = "FFS") = {
    new UserVolume(LogicalVolumeFactory.createEmptyDoubleDensityDisk(name,
                                                                     filesystemType))
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
 * Application view to an Amiga file system.
 * @constructor creates a UserVolume from a [[org.dmpp.adf.logical.LogicalVolume]]
 *   instance.
 * @param logicalVolume a LogicalVolume instance
 */
class UserVolume(val logicalVolume: LogicalVolume) {
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
    new RootDirectory(logicalVolume)
  }

  /**
   * Given a path expression, select a list of [[org.dmpp.adf.app.DosFile]]
   * instances.
   * @param path a path string
   * @return a list of DosFile instances matching the path expression
   */
  def select(path: String): List[DosFile] = {
    if (path == "") rootDirectory.list
    else {
      val comps = path.split("/").toList
      val matchFiles = rootDirectory.list.filter(entry => entry.name == comps(0))
      if (matchFiles == 0) Nil
      else {
        val dir = matchFiles(0).asInstanceOf[Directory]
        dir.list
      }
    }
  }

  /**
   * Returns the creation time of this file system.
   * @return creation time
   */
  def creationTime: Date = logicalVolume.rootBlock.creationTime

  /**
   * Returns the last modification time of this file system.
   * @return last modification time
   */
  def lastModificationTime: Date = logicalVolume.rootBlock.diskLastModificationTime

  /**
   * Writes the contents of this volume to the specified [[java.io.OutputStream]].
   * @param out the OutputStream
   */
  def writeToOutputStream(out: OutputStream) = logicalVolume.writeToOutputStream(out)

  /**
   * Returns the number of free blocks.
   * @return number of free blocks
   */
  def numFreeBlocks = logicalVolume.numFreeBlocks

  /**
   * Returns the number of used blocks.
   * @return nuber of used blocks
   */
  def numUsedBlocks = logicalVolume.numUsedBlocks

  def numBlocksTotal = logicalVolume.numBlocksTotal

  def numBytesAvailable = numFreeBlocks * logicalVolume.blockSizeInBytes
  def numBytesUsed      = numUsedBlocks * logicalVolume.blockSizeInBytes

  /**
   * Returns this volume's string representation.
   * @return string representation
   */
  override def toString = name + "[%s]".format(logicalVolume.filesystemType)
}
