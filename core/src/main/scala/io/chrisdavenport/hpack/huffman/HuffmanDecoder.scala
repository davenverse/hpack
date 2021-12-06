package io.chrisdavenport.hpack.huffman

import java.io.IOException
import HuffmanDecoder.Node
import java.io.ByteArrayOutputStream
import scala.collection.immutable.ArraySeq

class HuffmanDecoder private (
  private val root: Node
){

    /**
   * Decompresses the given Huffman coded string literal.
   * @param  buf the string literal to be decoded
   * @return the output stream for the compressed data
   * @throws IOException if an I/O error occurs. In particular,
   *         an <code>IOException</code> may be thrown if the
   *         output stream has been closed.
   */
  def decode(buf: Array[Byte]): Array[Byte] = {
    val baos = new ByteArrayOutputStream()

    var node: Node = root
    var current = 0
    var bits = 0
    (0 until buf.length).foreach{ i => 
      val b: Int = buf(i) & 0xFF
      current = (current << 8) | b
      bits += 8
      while (bits >= 8) {
        val c = (current >>> (bits - 8)) & 0xFF
        node = node.children(c)
        bits -= node.bits;
        if (node.isTerminal) {
          if (node.symbol == HuffmanDecoder.HUFFMAN_EOS) {
            throw HuffmanDecoder.EOS_DECODED
          }
          baos.write(node.symbol);
          node = root;
        }
      }
    }

    val breaks = new scala.util.control.Breaks()
    breaks.breakable{
      while (bits > 0) {
        val c = (current << (8 - bits)) & 0xFF
        node = node.children(c)
        if (node.isTerminal && node.bits <= bits) {
          bits -= node.bits;
          baos.write(node.symbol);
          node = root;
        } else {
          breaks.break()
        }
      }
    }

    // Section 5.2. String Literal Representation
    // Padding not corresponding to the most significant bits of the code
    // for the EOS symbol (0xFF) MUST be treated as a decoding error.
    val mask = (1 << bits) - 1;
    if ((current & mask) != mask) {
      throw HuffmanDecoder.INVALID_PADDING
    }

    baos.toByteArray()
  }

}

object HuffmanDecoder {

  def apply(
    codes: ArraySeq[Int],
    lengths: ArraySeq[Byte]
  ): HuffmanDecoder = {
    require(codes.length == 257 && codes.length == lengths.length, "invalid Huffman coding") 
    val node = Node.buildTree(codes, lengths)
    new HuffmanDecoder(node)
  }

  private val HUFFMAN_EOS = 256

  private class Node(
    val symbol: Int,       // terminal nodes have a symbol
    val bits: Int,        // number of bits matched by the node
    private[HuffmanDecoder] val children: Array[Node] // internal nodes have children (val but mutable)
  ){
    def isTerminal: Boolean = children == null
  } 
  private object Node {
    def apply(): Node = new Node(
      symbol = 0,
      bits = 8,
      children = new Array(256)
    )
    def apply(symbol: Int, bits: Int): Node = {
      assert(bits > 0 && bits <= 8)
      new Node(
        symbol,
        bits,
        null
      )
    }

    def buildTree(codes: ArraySeq[Int],lengths: ArraySeq[Byte]): Node = {
      val root = Node()
      (0 until codes.length).foreach{i => 
        insert(root, i, codes(i), lengths(i).toInt)
      }
      root
    }
    

    // Node is mutable and updated
    def insert(root: Node, symbol: Int, code: Int, lengthI: Int): Unit = {
      // traverse tree using the most significant bytes of code
      var length = lengthI
      var current = root
      while (length > 8) {
        if (current.isTerminal) {
          throw new IllegalStateException("invalid Huffman code: prefix not unique");
        }
        length -= 8
        val i: Int = (code >>> length) & 0xFF
        if (current.children(i) == null) {
          current.children(i) = Node()
        }
        current = current.children(i)
      }

      val terminal: Node = Node(symbol, length)
      val shift: Int = 8 - length
      val start: Int = (code << shift) & 0xFF
      val end: Int = 1 << shift
      (start until start + end).foreach{ i =>
        current.children(i) = terminal
      }
    }
  }

  def EOS_DECODED = new IOException("EOS Decoded")
  def INVALID_PADDING = new IOException("Invalid Padding")

}