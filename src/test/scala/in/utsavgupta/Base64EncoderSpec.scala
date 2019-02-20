package in.utsavgupta

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class Base64EncoderSpec extends FlatSpec with Matchers with BeforeAndAfter {

  val base64Encoder = new Base64Encoder()

  "A Base64Encoder" should " return empty if the provided input is empty" in {
    assert(base64Encoder.encode("") == "")
  }

  it should "return TWFu for Man" in {
    assert(base64Encoder.encode("Man") == "TWFu")
  }

  it should "return TWE= for Ma" in {
    assert(base64Encoder.encode("Ma") == "TWE=")
  }

  it should "return TQ== for M" in {
    assert(base64Encoder.encode("M") == "TQ==")
  }

  it should "return SGVsbG8gV29ybGQ= for Hello World" in {
    assert(base64Encoder.encode("Hello World") == "SGVsbG8gV29ybGQ=")
  }

  it should "return YW55IGNhcm5hbCBwbGVhc3VyZS4= for any carnal pleasure." in {
    assert(base64Encoder.encode("any carnal pleasure.") == "YW55IGNhcm5hbCBwbGVhc3VyZS4=")
  }

  it should "return YW55IGNhcm5hbCBwbGVhc3VyZQ== for any carnal pleasure" in {
    assert(base64Encoder.encode("any carnal pleasure") == "YW55IGNhcm5hbCBwbGVhc3VyZQ==")
  }

  it should "return YW55IGNhcm5hbCBwbGVhc3Vy for any carnal pleasur" in {
    assert(base64Encoder.encode("any carnal pleasur") == "YW55IGNhcm5hbCBwbGVhc3Vy")
  }

  it should "return YW55IGNhcm5hbCBwbGVhc3U= for any carnal pleasu" in {
    assert(base64Encoder.encode("any carnal pleasu") == "YW55IGNhcm5hbCBwbGVhc3U=")
  }

  it should "return YW55IGNhcm5hbCBwbGVhcw== for any carnal pleas" in {
    assert(base64Encoder.encode("any carnal pleas") == "YW55IGNhcm5hbCBwbGVhcw==")
  }
}
