package org.dmpp.adf

/**
 * Test cases for ADF files.
 */
import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._

class PhysicalVolumeTest extends JUnit4(PhysicalVolumeSpec)
object PhysicalVolumeSpecRunner extends ConsoleRunner(PhysicalVolumeSpec)

object PhysicalVolumeSpec extends Specification {

  var adfFile: PhysicalVolume = null

  "AdfFile" should {

    doBefore {
      var workbenchFile: InputStream = null
      try {
        workbenchFile = getClass.getResourceAsStream("/wbench1.3.adf")
        adfFile = AdfFileFactory.readAdfFile(workbenchFile)
      } finally {
        if (workbenchFile != null) workbenchFile.close
      }
    }

    "data size should be correct" in {
      AdfFile.FileSize must_== 901120
    }
    "first three bytes of sector 0 must be DOS" in {
      adfFile.sector(0)(0).asInstanceOf[Char] must_== 'D'
      adfFile.sector(0)(1).asInstanceOf[Char] must_== 'O'
      adfFile.sector(0)(2).asInstanceOf[Char] must_== 'S'
    }
    "accessing sector 1759 should succeed" in {
      adfFile.sector(1759)(0) must_== 0
      adfFile.sector(1759)(511) must_== 121
    }
    "accessing byte 512 of a sector should fail" in {
      adfFile.sector(1759)(512) must throwA[IndexOutOfBoundsException]
      adfFile.sector(0)(512) must throwA[IndexOutOfBoundsException]
    }
    "accessing sector 1760 should fail" in {
      adfFile.sector(1760)(0) must throwA[IndexOutOfBoundsException]
    }
    "write a byte in a sector" in {
      adfFile.sector(3)(2) = 42
      adfFile.sector(3)(3) = 128
      adfFile.sector(3)(4) = 255

      adfFile.sector(3)(2) must_== 42
      adfFile.sector(3)(3) must_== 128
      adfFile.sector(3)(4) must_== 255
    }
  }
}
