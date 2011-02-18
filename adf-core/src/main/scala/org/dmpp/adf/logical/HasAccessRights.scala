/**
 * Created on February 14, 2011
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
package org.dmpp.adf.logical

/**
 * Shell and protection flags, called HSPARWED in the Guru Book.
 */
case class ProtectionFlags(flags: Int) {
  def canDelete  = (flags & 0x0001) == 0x0000
  def canExecute = (flags & 0x0002) == 0x0000
  def canWrite   = (flags & 0x0004) == 0x0000
  def canRead    = (flags & 0x0008) == 0x0000
  def isArchived = (flags & 0x0010) == 0x0010
  def isPure     = (flags & 0x0020) == 0x0020
  def isScript   = (flags & 0x0040) == 0x0040
  def hold       = (flags & 0x0080) == 0x0080
}
/**
 * Trait to decorate blocks that have access masks and user/group ids.
 */
trait HasAccessRights { self : DirectoryEntryBlock =>

  /**
   * Returns the user id.
   * @return user id
   */
  def uid: Int = sector.int16At(sector.sizeInBytes - 196)

  /**
   * Returns the group id.
   * @return group id
   */
  def gid: Int = sector.int16At(sector.sizeInBytes - 194)

  /**
   * Returns shell and protection flags.
   * @return shell and protection flags
   */
  def flags = ProtectionFlags(sector.int32At(sector.sizeInBytes - 192))
}
