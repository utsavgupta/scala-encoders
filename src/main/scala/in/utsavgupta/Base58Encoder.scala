package in.utsavgupta

class Base58Encoder {

  val symbols = ('1' to '9') ++ ('A' to 'H') ++ ('J' to 'N') ++ ('P' to 'Z') ++ ('a' to 'k') ++ ('m' to 'z')

  /**
    * Encodes a given string to Base58
    *
    * @param input String to be encoded
    * @return Base58 representation of the given string
    */
  def encode(input: String): String = {
    if(input.isEmpty) ""
    else process(input)
  }

  private def process(str: String): String = {
    val binStr = str.foldLeft("")((out, ch) => out + "%8s".format(ch.toBinaryString).replace(' ', '0'))

    val binStrLen = binStr.size
    val zero: BigInt = 0

    val bigInt: BigInt = ((binStrLen -1) to 0 by -1).zip(binStr)
      .foldLeft(zero)((acc, tup) => acc + ((tup._2.toInt - 48) * bigIntPower2(tup._1)))

    def tokenize(acc: String, n: BigInt): String =
      if(n > 57) tokenize(acc + symbols((n % 58).toInt), n / 58)
      else (acc + symbols((n % 58).toInt)).reverse.toString

    tokenize("", bigInt)
  }

  private def bigIntPower2(exp: BigInt): BigInt = {
    def go(acc: BigInt, e: BigInt): BigInt = {
      if(e < 1) acc
      else go(2 * acc, e - 1)
    }

    go(1, exp)
  }
}
