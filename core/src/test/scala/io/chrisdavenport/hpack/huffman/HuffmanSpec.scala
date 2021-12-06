package io.chrisdavenport.hpack

import munit.ScalaCheckSuite 
import io.chrisdavenport.hpack.huffman.Huffman
import java.nio.charset.StandardCharsets
import org.scalacheck._
import org.scalacheck.Prop._

class HuffmanSpec extends ScalaCheckSuite {

  private val tchars =
    Set('!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~') ++
      ('0' to '9') ++ ('A' to 'Z') ++ ('a' to 'z')

  val genTchar: Gen[Char] = Gen.oneOf(tchars)

  val genToken: Gen[String] =
    Gen.nonEmptyListOf(genTchar).map(_.mkString)

  case class TestString(s: String)
  implicit val http4sTestingArbitraryForRawHeader: Arbitrary[TestString] =
    Arbitrary {
      for {
        token <- Gen.frequency(
          8 -> genToken,
          1 -> Gen.nonEmptyBuildableOf[String, Char](Gen.asciiChar),
        )
      } yield TestString(token)
    }

  // TODO Custom Gen
  property("Traverse Succesfully") {
    forAll{ (testS: TestString) => {
      val test = testS.s
      val encoded = Huffman.encode(test.getBytes(StandardCharsets.ISO_8859_1))
      val decoded = Huffman.decode(encoded)
      val out = new String(decoded, StandardCharsets.ISO_8859_1)
      assertEquals(test, out)
    }}
  }

}
