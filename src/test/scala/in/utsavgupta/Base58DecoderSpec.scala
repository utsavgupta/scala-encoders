package in.utsavgupta

import in.utsavgupta.exception.CouldNotParseInputException
import org.scalatest.{FlatSpec, Matchers}

class Base58DecoderSpec extends FlatSpec with Matchers {

  val base58Decoder = new Base58Decoder

  "A Base58Decoder" should "return an empty string when the input is empty" in {
    assert(base58Decoder.decode("") == "")
  }

  it should "return abc when the input is ZiCa" in {
    assert(base58Decoder.decode("ZiCa") == "abc")
  }

  it should "return hello when the input is Cn8eVZg" in {
    assert(base58Decoder.decode("Cn8eVZg") == "hello")
  }

  it should "return Hello World when the input is JxF12TrwUP45BMd" in {
    assert(base58Decoder.decode("JxF12TrwUP45BMd") == "Hello World")
  }

  it should "return any carnal pleasure. when the input is 2MjF7Xjy2CRG18GGG65SPAwKgC7F" in {
    assert(base58Decoder.decode("2MjF7Xjy2CRG18GGG65SPAwKgC7F") == "any carnal pleasure.")
  }

  it should "throw an exception if the input contains invalid characters" in {
    a [CouldNotParseInputException] should be thrownBy {
      base58Decoder.decode("=")
    }

    a [CouldNotParseInputException] should be thrownBy {
      base58Decoder.decode("/")
    }

    a [CouldNotParseInputException] should be thrownBy {
      base58Decoder.decode("+")
    }

    a [CouldNotParseInputException] should be thrownBy {
      base58Decoder.decode("0")
    }

    a [CouldNotParseInputException] should be thrownBy {
      base58Decoder.decode("O")
    }

    a [CouldNotParseInputException] should be thrownBy {
      base58Decoder.decode("l")
    }
  }
}
