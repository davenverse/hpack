package io.chrisdavenport.hpack

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import java.nio.charset.StandardCharsets

case class HeaderField(
  name: ArraySeq[Byte],
  value: ArraySeq[Byte]
){
  val size = name.size + value.size + HeaderField.overhead

  override def toString: String = {
    val arrayB = new ArrayBuffer[Byte](name.size + value.size + 1)
    arrayB.addAll(name)
    arrayB.addOne(':'.toByte)
    arrayB.addAll(value)
    new String(arrayB.toArray)
  }
}

object HeaderField {
  val overhead = 32
  def apply(name: String, value: String): HeaderField = {
    HeaderField(
      ArraySeq.unsafeWrapArray(name.getBytes(StandardCharsets.ISO_8859_1)),
      ArraySeq.unsafeWrapArray(value.getBytes(StandardCharsets.ISO_8859_1))
    )
  }
}