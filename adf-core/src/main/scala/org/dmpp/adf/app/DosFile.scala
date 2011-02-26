/**
 * Created on February 26, 2011
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

import java.util.Date
import org.dmpp.adf.logical._

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

  /**
   * Returns the comment.
   * @return comment
   */
  def comment: String

  /**
   * Last access time. This is a property of non-root directories.
   * Called on the root directory or a file, this will throw an
   * UnsupportedOperationException.
   * @return the last access time
   */
  def lastAccessTime: Date

  /**
   * Last modification time. This is a property of root directories and files.
   * Called on a non-root directory, this will throw an
   * UnsupportedOperationException.
   * @return the last modification time
   */
  def lastModificationTime: Date
}

/**
 * Abstract super class for directory entries.
 * @constructor creates a new AbstractDosFile instance
 * @param dirEntryBlock a DirectoryEntryBlock
 */
abstract class AbstractDosFile(val dirEntryBlock: DirectoryEntryBlock) extends DosFile {
  def name                 = dirEntryBlock.name
  def comment              = dirEntryBlock.comment
  def lastAccessTime       = dirEntryBlock.lastAccessTime
  def lastModificationTime: Date = {
    throw new UnsupportedOperationException("not supported")
  }
  override def toString = name
}

