package io.chrisdavenport.hpack.huffman

import scala.collection.immutable.ArraySeq
import java.io.OutputStream
import java.io.ByteArrayOutputStream

class HuffmanEncoder(
  private val codes: ArraySeq[Int],
  private val lengths: ArraySeq[Byte]
){

  
  def encode(data: Array[Byte]): Array[Byte] = {
    val ios = new ByteArrayOutputStream()
    encode(ios, data)
    ios.toByteArray
  }
    /**
   * Compresses the input string literal using the Huffman coding.
   * @param  out  the output stream for the compressed data
   * @param  data the string literal to be Huffman encoded
   * @throws IOException if an I/O error occurs.
   * @see    com.twitter.hpack.HuffmanEncoder#encode(OutputStream, byte[], int, int)
   */
  private def encode(out: OutputStream, data: Array[Byte]): Unit = {
    encode(out, data, 0, data.length)
  }

  /**
   * Compresses the input string literal using the Huffman coding.
   * @param  out  the output stream for the compressed data
   * @param  data the string literal to be Huffman encoded
   * @param  off  the start offset in the data
   * @param  len  the number of bytes to encode
   */
  private def encode(out: OutputStream, data: Array[Byte], off: Int, len: Int) : Unit = {
    if (off < 0 || len < 0 || (off + len) < 0 || off > data.length || (off + len) > data.length) {
      throw new IndexOutOfBoundsException();
    } else if (len == 0) {
      ()
    } else {
      var current: Long = 0
      var n: Int = 0
      (0 until len).foreach{ i => 
        val b: Int = data(off + i) & 0xFF
        val code: Int = codes(b)
        val nbits: Int = lengths(b).toInt

        current <<= nbits
        current |= code
        n += nbits

        while (n >= 8) {
          n -= 8
          out.write((current >> n).toInt)
        }
      }

      if (n > 0) {
        current <<= (8 - n)
        current |= (0xFF >>> n) // this should be EOS symbol
        out.write(current.toInt)
      }
    }
  }
}

object HuffmanEncoder {
  def apply(
    codes: ArraySeq[Int],
    lengths: ArraySeq[Byte]
  ): HuffmanEncoder = new HuffmanEncoder(codes, lengths)
}