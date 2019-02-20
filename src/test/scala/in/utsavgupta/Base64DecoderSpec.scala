package in.utsavgupta

import in.utsavgupta.exception.CouldNotParseInputException
import org.scalatest.{FlatSpec, Matchers}

class Base64DecoderSpec extends FlatSpec with Matchers {

  val base64Decoder = new Base64Decoder()

  "A Base64 decoder" should "return an empty string in case an empty string is provided as input" in {
    assert(base64Decoder.decode("").equals(""))
  }

  it should "return Man for TWFu" in {
    assert(base64Decoder.decode("TWFu") == "Man")
  }

  it should "return Ma for TWE=" in {
    assert(base64Decoder.decode("TWE=") == "Ma")
  }

  it should "return M for TQ==" in {
    assert(base64Decoder.decode("TQ==") == "M")
  }

  it should "return Hello World for SGVsbG8gV29ybGQ=" in {
    assert(base64Decoder.decode("SGVsbG8gV29ybGQ=") == "Hello World")
  }

  it should "return any carnal pleasure. for YW55IGNhcm5hbCBwbGVhc3VyZS4=" in {
    assert(base64Decoder.decode("YW55IGNhcm5hbCBwbGVhc3VyZS4=") == "any carnal pleasure.")
  }

  it should "return any carnal pleasure for YW55IGNhcm5hbCBwbGVhc3VyZQ==" in {
    assert(base64Decoder.decode("YW55IGNhcm5hbCBwbGVhc3VyZQ==") == "any carnal pleasure")
  }

  it should "return any carnal pleasur for YW55IGNhcm5hbCBwbGVhc3Vy" in {
    assert(base64Decoder.decode("YW55IGNhcm5hbCBwbGVhc3Vy") == "any carnal pleasur")
  }

  it should "return any carnal pleasu for YW55IGNhcm5hbCBwbGVhc3U=" in {
    assert(base64Decoder.decode("YW55IGNhcm5hbCBwbGVhc3U=") == "any carnal pleasu")
  }

  it should "return any carnal pleas for YW55IGNhcm5hbCBwbGVhcw==" in {
    assert(base64Decoder.decode("YW55IGNhcm5hbCBwbGVhcw==") == "any carnal pleas")
  }

  it should "throw an exception if the input contains invalid characters" in {
    a [CouldNotParseInputException] should be thrownBy {
      base64Decoder.decode("AA23()__")
    }
  }
}
