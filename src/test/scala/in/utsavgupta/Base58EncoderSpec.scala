package in.utsavgupta

import org.scalatest.{FlatSpec, Matchers}

class Base58EncoderSpec extends FlatSpec with Matchers {

  val base58Encoder = new Base58Encoder()

  "A Base58Encoder" should "return an empty string when the input is empty" in {
    assert(base58Encoder.encode("").isEmpty)
  }

  it should "return ZiCa when the input is abc" in {
    assert(base58Encoder.encode("abc") == "ZiCa")
  }

  it should "return Cn8eVZg when the input is hello" in {
    assert(base58Encoder.encode("hello") == "Cn8eVZg")
  }

  it should "return JxF12TrwUP45BMd when the input is Hello World" in {
    assert(base58Encoder.encode("Hello World") == "JxF12TrwUP45BMd")
  }

  it should "return 2MjF7Xjy2CRG18GGG65SPAwKgC7F when the input is any carnal pleasure." in {
    assert(base58Encoder.encode("any carnal pleasure.") == "2MjF7Xjy2CRG18GGG65SPAwKgC7F")
  }
}
