package org.dmpp.adf

/**
 * Test cases for utility classes.
 */
import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._

class UtilTest extends JUnit4(UtilSpec)
object UtilSpecRunner extends ConsoleRunner(UtilSpec)

object UtilSpec extends Specification {
  "UnsignedInt" should {

    import UnsignedIntConversions._

    "make an UnsignedInt" in {
      val uint = new UnsignedInt(32)
      uint.value must_== 32l
    }
    "add two UnsignedInts" in {
      val uint = new UnsignedInt(32)
      val result = uint + 5
      result.value must_== 37l
      result.overflowOccurred must beFalse
    }
    "do a wrapping addition" in {
      val uint = new UnsignedInt(UnsignedInt.MaxValue)
      val result = uint + 5
      result.value must_== 4l
      result.overflowOccurred must beTrue
    }
  }
}
