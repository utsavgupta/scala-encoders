package in.utsavgupta

import in.utsavgupta.exception.CouldNotParseInputException

class Base58Decoder {

  val symbols = ('1' to '9') ++ ('A' to 'H') ++ ('J' to 'N') ++ ('P' to 'Z') ++ ('a' to 'k') ++ ('m' to 'z')

  /**
    * Decodes a base58 encoded string to it's original form
    *
    * @param input String to be decoded
    * @return Decoded string
    */
  def decode(input: String): String = {
    if (input.isEmpty) ""
    else process(input)
  }

  private def process(str: String): String = {
    val lenStr: Long = (str.size - 1)

    val bigInt = (lenStr to 0 by -1).zip(str)
      .foldLeft(BigInt(0))((out, tup) => out + bigIntProduct(tup._1, tup._2))

    bigInt.toByteArray.foldLeft("")((acc, chr) => acc + chr.toChar)
  }

  private def bigIntProduct(exp: Long, strCoefficient: Char): BigInt = {
    val coefficient = symbols.indexOf(strCoefficient)

    if(coefficient < 0) throw new CouldNotParseInputException(s"Unknown symbol $strCoefficient")

    def go(acc: BigInt, e: Long): BigInt = {
      if(e > 0) go(58 * acc, e - 1)
      else acc
    }

    coefficient * go(1, exp)
  }
}
