package in.utsavgupta

class Base64Encoder {

  val symbols = ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ Vector('+', '\\')

  /**
    * Encodes a given string to Base64
    *
    * @param input String to be encoded
    * @return Base64 representation of the given string
    */
  def encode(input: String): String = {
    if(input.isEmpty) input
    else process(input)
  }

  private def process(str: String): String = {
    val binaryString: String = str.map( ch => "%8s".format(ch.toInt.toBinaryString)
                                           .replace(" ", "0") )
                                           .mkString("")

    val chunks: Iterator[Iterator[String]] = binaryString.grouped(24).map(_.grouped(6))

    chunks.foldLeft("")((result, chunk) => result + processChunk(chunk))
  }

  private def processChunk(chunk: Iterator[String]): String = {

    val idxSeq = chunk.toIndexedSeq

    idxSeq.size match {
      case 0 => ""
      case 2 => "" + symbols(bstr2Int(idxSeq(0))) + symbols(bstr2Int(idxSeq(1))) + "=="
      case 3 => "" + symbols(bstr2Int(idxSeq(0))) + symbols(bstr2Int(idxSeq(1))) + symbols(bstr2Int(idxSeq(2))) + "="
      case 4 => "" + symbols(bstr2Int(idxSeq(0))) + symbols(bstr2Int(idxSeq(1))) + symbols(bstr2Int(idxSeq(2))) + symbols(bstr2Int(idxSeq(3)))
    }
  }

  private def bstr2Int(str: String): Int = {
    val mstr = if (str.size == 6) str else "%-8s".format(str).replace(" ", "0")

    (5 to 0 by -1).zip(mstr)
      .foldLeft(0d)((acc, tup) => acc + ((tup._2.toInt - 48) * Math.pow(2, tup._1)))
      .toInt
  }
}