/**
 * Created on February 12, 2011
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
package org.dmpp.adf.physical

import java.io._
import java.nio._

import org.dmpp.adf.util._

/**
 * A sector is a unit of data in the physical layer. It is basically
 * just a view onto the disk image memory with a defined size and position.
 * Individual bytes can be read/written by accessing it like an array of
 * bytes.
 */
class Sector(data: Array[Byte], offset: Int, sizeInBytes: Int) extends BitHelper {

  /**
   * Retrieves the data byte at position byteNum.
   *
   * @param byteNum the position to retrieve
   * @return data byte at the specified position
   */
  def apply(byteNum: Int) = {
    if (byteNum >= sizeInBytes) throw new IndexOutOfBoundsException("invalid byte num")
    data(offset + byteNum) & 0xff
  }

  /**
   * Updates the data byte at the specified position with a value.
   *
   * @param byteNum the position to update
   * @param value the new value
   */
  def update(byteNum: Int, value: Int) {
    if (byteNum >= sizeInBytes) throw new IndexOutOfBoundsException("invalid byte num")
    data(offset + byteNum) = (value & 0xff).asInstanceOf[Byte]
  }

  /**
   * Returns the 32 bit integer at the specified position.
   *
   * @param byteNum the position of the data byte
   * @return the specified data byte
   */
  def int32At(byteNum: Int) = {
    makeInt32(data(offset + byteNum),     data(offset + byteNum + 1),
              data(offset + byteNum + 2), data(offset + byteNum + 3))
  }
}

/**
 * The PhysicalVolume trait specifies the general interface of a physical volume.
 * Derived classes such as DD/HD disks or hard disks should implement this
 * interface.
 * It's data can be accessed as in a contiguous memory area, making checksum
 * calculations that span across multiple sectors simpler.
 */
trait PhysicalVolume {

  /**
   * Retrieves the data byte at position byteNum.
   *
   * @param byteNum the position to retrieve
   * @return data byte at the specified position
   */
  def apply(byteNum: Int): Byte

  /**
   * Updates the data byte at the specified position with a value.
   *
   * @param byteNum the position to update
   * @param value the new value
   */
  def update(byteNum: Int, value: Byte)

  /**
   * Retrieves a sector on this disk.
   * @param sectorNum the sector number
   * @return the specified sector
   */
  def sector(sectorNum: Int): Sector

  /**
   * Returns the number of bytes per sector.
   *
   * @return the number of bytes per sector
   */
  def bytesPerSector: Int

  /**
   * Returns the 32 bit integer at the specified position.
   *
   * @param byteNum the position of the data byte
   * @return the specified data byte
   */
  def int32At(byteNum: Int): Int
}

/**
 * Constants for double density disks.
 */
object DoubleDensityDisk {
  val BytesPerSector       = 512
  val NumSectorsPerTrack   = 11
  val NumTracksPerCylinder = 2
  val NumCylindersPerDisk  = 80
  val ImageSize = BytesPerSector * NumSectorsPerTrack *
                  NumTracksPerCylinder * NumCylindersPerDisk 
}

/**
 * This is the implementation of a Double Density disk.
 *
 * @constructor creates a DoubleDensityDisk instance from a byte array
 * @param data a byte array containing a double density ADF image
 */
class DoubleDensityDisk(data: Array[Byte]) extends PhysicalVolume with BitHelper {
  import DoubleDensityDisk._

  def apply(byteNum: Int): Byte = data(byteNum)
  def update(byteNum: Int, value: Byte) = data(byteNum) = value

  def sector(sectorNum: Int): Sector = new Sector(data, sectorNum * BytesPerSector,
                                                  BytesPerSector)
  def bytesPerSector: Int = BytesPerSector
  def int32At(byteNum: Int) = {
    makeInt32(data(byteNum),     data(byteNum + 1),
              data(byteNum + 2), data(byteNum + 3))
  }
}

/**
 * Factory to generate [[org.dmpp.adf.physical.PhysicalVolume]] instances
 * from various input sources.
 */
object PhysicalVolumeFactory {

  /**
   * Creates a physical volume from a double density ADF input stream.
   * 
   * @param input an input stream
   * @return a physical volume containing the data in the input stream
   */
  def readDoubleDensityDisk(input: InputStream): PhysicalVolume = {
    val data = new Array[Byte](DoubleDensityDisk.ImageSize)
    input.read(data)
    new DoubleDensityDisk(data)
  }
}

