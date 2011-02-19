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

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._
import java.util.Date
import org.dmpp.adf.app._

/**
 * Test cases for user volumes.
 */
class UserVolumeTest extends JUnit4(UserVolumeSpec)
object UserVolumeSpecRunner extends ConsoleRunner(UserVolumeSpec)

object UserVolumeSpec extends Specification {
  "UserVolume" should {
    val workbenchFile = new File(getClass.getResource("/wbench1.3.adf").getFile)
    var emptyDisk: UserVolume = null
    var workbenchDisk: UserVolume = null
    doBefore {
      workbenchDisk = UserVolumeFactory.readFromFile(workbenchFile)
      emptyDisk = UserVolumeFactory.createEmptyDoubleDensityDisk()
    }
    "read workbench root directory" in {
      workbenchDisk.name must_== "Workbench1.3"
      formatted(workbenchDisk.creationTime) must_== "1989-08-16 13:57:36.100"
      val rootdir = workbenchDisk.rootDirectory
      rootdir.isDirectory must beTrue
      rootdir.name must_== "Workbench1.3"
      rootdir.list.length must_== 24
/*
      for (direntry <- rootdir.list) {
        printf("File: %s dir: %b file: %b\n",
               direntry.name, direntry.isDirectory, direntry.isFile)
      }*/
    }
  }
  def formatted(date: Date) = {
    val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dateFormat.format(date)
  }
}
