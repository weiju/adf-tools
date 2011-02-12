package org.dmpp.adf

trait BitHelper {
  def flagClear(flags: Int, flag: Int) = (flags & flag) == 0
  def flagSet(flags: Int, flag: Int) = (flags & flag) == flag
  def makeLong(byte0: Int, byte1: Int, byte2: Int, byte3: Int) = {
    ((byte0 << 24) & 0xff000000) | ((byte1 << 16) & 0xff0000) |
      ((byte2 << 8) & 0xff00) | (byte3 & 0xff)
  }
}

object UnsignedIntConversions {
  implicit def uint2Long(uint: UnsignedInt): Long = uint.value
  implicit def uint2Int(uint: UnsignedInt): Int = uint.intValue
  implicit def long2Uint(value: Long): UnsignedInt = UnsignedInt(value)
  implicit def int2Uint(value: Int): UnsignedInt = UnsignedInt(value.asInstanceOf[Long])
}

/**
 * A class to emulate unsigned int behavior for addition, namely wrapping around.
 */
object UnsignedInt {
  val MaxValue = 4294967295l
}
case class UnsignedInt(value: Long) {
  import UnsignedInt._
  if (value > MaxValue) throw new IllegalArgumentException("value too large !!!")

  def +(aValue: UnsignedInt): UnsignedInt = {
    val result = (value + aValue.value) % (MaxValue + 1)
    UnsignedInt(result)
  }
  def intValue = value.asInstanceOf[Int]
}
