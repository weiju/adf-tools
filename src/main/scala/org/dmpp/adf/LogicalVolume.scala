package org.dmpp.adf

object BootBlock {
  val FlagFFS              = 1
  val FlagIntlOnly         = 2
  val FlagDirCacheAndIntl  = 4
}

object LogicalVolume {
  val VolumeNameMaxChars = 30
  val RootSectorNumber   = 880
}

class LogicalVolume(physicalVolume: PhysicalVolume) {
  import LogicalVolume._

  class BootBlock extends BitHelper {
    import BootBlock._

    private val sector0 = physicalVolume.sector(0)
    private val sector1 = physicalVolume.sector(1)

    private def flags = sector0(3) & 0x07
    def fileType = if (flagClear(flags, FlagFFS)) "OFS" else "FFS"
    def isInternational = flagSet(flags, FlagIntlOnly) ||
                          flagSet(flags, FlagDirCacheAndIntl)
    def useDirCache = flagSet(flags, FlagDirCacheAndIntl)

    def rootBlockNumber = sector0.longAt(8)
    // TODO: checksums
  }

  class RootBlock {
    val sector        = physicalVolume.sector(RootSectorNumber) // hard-coded for now
    def blockType     = sector.longAt(0)
    def headerKey     = sector.longAt(4)
    def highSeq       = sector.longAt(8)
    def hashtableSize = sector.longAt(12)
    def firstData     = sector.longAt(16)
    def checksum      = sector.longAt(20)
    def diskName      = {
      val nameLength = scala.math.min(sector(physicalVolume.bytesPerSector - 80),
                                      VolumeNameMaxChars)
      val offset = physicalVolume.bytesPerSector - 79
      val builder = new StringBuilder
      var i = 0
      while (i < nameLength) {
        builder.append(sector(offset + i).asInstanceOf[Char])
        i += 1
      }
      builder.toString
    }

    def computeChecksum: Int = {
      import UnsignedIntConversions._

      var sum: UnsignedInt = 0
      for (i <- 0 until physicalVolume.bytesPerSector by 4) {
        if (i != 20) { // ignore offset 20 (the checksum field)
          sum += (sector.longAt(i) & 0xffffffffl)
        }
      }
      -sum.intValue
    }
  }

  val bootBlock = new BootBlock
  val rootBlock = new RootBlock
}
