package in.utsavgupta

import in.utsavgupta.exception.CouldNotParseInputException

class Base64Decoder {

  val symbols = ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ Vector('+', '\\')

  /**
    * Decodes a base64 encoded string to it's original form
    *
    * @param input String to be decoded
    * @return Decoded string
    */
  def decode(input: String): String = {
    if (input.isEmpty) ""
    else process(input)
  }

  private def process(input: String): String = {
    input.grouped(4).map(processChunk).reduce(_ + _)
  }

  private def getAndFmtChr(chr: Char): String = {
    if(symbols.indexOf(chr) == -1)
      throw new CouldNotParseInputException(s"Unknown symbol $chr")

    "%6s".format(symbols.indexOf(chr).toBinaryString).replace(" ", "0")
  }

  private def processChunk(input: String): String = {
    val binStr = input.foldLeft("")((acc, chr) =>
      if (chr == '=') acc else acc + getAndFmtChr(chr))

    binStr.grouped(8).filter(_.size == 8).map(bin => bstr2Int(bin).toChar).foldLeft("")(_ + _)
  }

  private def bstr2Int(str: String): Int = {
    (7 to 0 by -1).zip(str)
      .foldLeft(0d)((acc, tup) => acc + ((tup._2.toInt - 48) * Math.pow(2, tup._1)))
      .toInt
  }
}
