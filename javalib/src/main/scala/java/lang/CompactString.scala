// See: https://github.com/scala-native/scala-native/issues/846
package java.lang

import java.nio.charset.Charset

import scalanative.runtime.Intrinsics._
import scalanative.runtime.{Array => RuntimeArray, _}
import scalanative.unsafe._
import scalanative.unsigned._

sealed trait StringContainer extends CharSequence {
  def length(): scala.Int

  //def getBytes(): Array[scala.Byte] // for internal use only
  def getChars(start: Int, end: Int, buffer: Array[Char], index: Int): Unit = {
    var i = start
    var j = index
    while (i < end) {
      //println("'"+this.charAt(i)+"'")
      buffer(j) = this.charAt(i)
      i += 1
      j += 1
    }
  }

  @inline protected def checkSubSequenceRange(
    start: scala.Int,
    end: scala.Int
  ): Unit = {
    if (start < 0 || start > end || end > length())
      throw new StringIndexOutOfBoundsException(
        s"String index out of range (start=$start end=$end) while length=$length"
      )
  }

  // TODO: implement from string4s/LazySplit
  def indicesOfSplit(): Iterable[(Int,Int)] = {
    null
  }

  def toCompactString(): CompactString = {
    new CompactString(this)
  }

  override def toString(): String = {
    val nChars = this.length()
    val chars = new Array[Char](this.length())
    this.getChars(0, nChars, chars, 0)

    //println(chars.toList)
    new java.lang._String(0, nChars, chars).asInstanceOf[String]
    //chars.toList.toString
  }
}

object EmptyStringContainer extends StringContainer {
  @inline def length(): scala.Int = 0
  def charAt(index: scala.Int): scala.Char = {
    throw new StringIndexOutOfBoundsException(
      "String index out of range: " + index
    )
  }
  def subSequence(start: scala.Int, end: scala.Int): CharSequence = {
    this.checkSubSequenceRange(start, end)
    this
  }

  //def getBytes(): Array[scala.Byte] = Array.emptyByteArray

  override def toString(): String = ""
}

// Compact representation of Latin1 Strings
// with all bytes being used (no offset/count data)
final class StringByteStruct1(
  byte0: scala.Byte
) extends StringContainer {

  @inline def length(): scala.Int = 1

  def charAt(index: scala.Int): scala.Char = {
    if (index == 0) (byte0 & 0xFF).toChar
    else throw new StringIndexOutOfBoundsException(
      "String index out of range: " + index
    )
  }
  def subSequence(start: scala.Int, end: scala.Int): CharSequence = {
    this.checkSubSequenceRange(start, end)

    if (start == end) EmptyStringContainer
    this
  }

  //def getBytes(): Array[scala.Byte] = Array(byte0)
}

final class StringByteStruct2(
  byte0: scala.Byte,
  byte1: scala.Byte
) extends StringContainer {

  @inline def length(): scala.Int = 2

  def charAt(index: scala.Int): scala.Char = {
    index match {
      case 0 => (byte0 & 0xFF).toChar
      case 1 => (byte1 & 0xFF).toChar
      case _ => throw new StringIndexOutOfBoundsException(
        "String index out of range: " + index
      )
    }
  }
  def subSequence(start: scala.Int, end: scala.Int): CharSequence = {
    this.checkSubSequenceRange(start, end)

    val nBytes = end - start
    nBytes match {
      case 0 => EmptyStringContainer
      case 1 => new StringByteStruct1(byte0)
      case 2 => this
    }
  }

  //def getBytes(): Array[scala.Byte] = Array(byte0, byte1)
}

//final class StringByteStruct3() extends StringContainer
//final class StringByteStruct4() extends StringContainer

/*sealed trait StringByteArrayContainer extends StringContainer {

}*/

// Compact representation of Latin1 Strings
// with all bytes being used (no offset/count data)
object StringByteArray {

  private val _init = new StringByteArray()
  private val _objSize = 8L

  def apply(length: Int): StringByteArray = {
    val objCls = classOf[StringByteArray]

    // FIXME: this should be determined automatically
    val objSizeAsInt = 8
    val objSize = objSizeAsInt.toUSize

    //val arrSize = (16 + 1 * length).toULong
    val arrSize = (4 + 1 * length).toUSize // 4 => length field (number of Bytes)

    // Allocate memory for Object + Array data
    val varLenObjRawPtr = GC.alloc_atomic(objCls, objSize + arrSize)
    //storeInt(elemRawPtr(arrRawPtr, objSizeAsInt), length)

    val varLenObjPtr: Ptr[scala.Byte] = fromRawPtr(varLenObjRawPtr)
    // FIXME: why StringByteArray._objSize doesn't work?
    val lenPtr = varLenObjPtr + objSizeAsInt
    storeInt(toRawPtr(lenPtr), length)

    // Prepare array structure
    /*val arrCls  = classOf[ByteArray]
    val arrRtti = castObjectToRawPtr(arrCls)
    storeRawPtr(elemRawPtr(metaArr, objSizeAsLong), arrRtti)
    storeInt(elemRawPtr(metaArr, objSizeAsLong + 8), length)
    storeInt(elemRawPtr(metaArr, objSizeAsLong + 12), 1)
    */

    castRawPtrToObject(varLenObjRawPtr).asInstanceOf[StringByteArray]
  }

  /*
  def apply(bytes: Array[Byte]) = {

  }*/
}

final class StringByteArray private() extends StringContainer {

  // FIXME: we need another procedure
  //override def clone(): StringByteArray = super.clone().asInstanceOf[StringByteArray]

  /*
  @inline def length(): scala.Int = backingArray.length

  private[lang] def backingArray: ByteArray = {
    val ptr = castObjectToRawPtr(this)
    val arrPtr = elemRawPtr(ptr, 8L) // FIXME: why StringByteArray._objSize doesn't work?
    castRawPtrToObject(arrPtr).asInstanceOf[ByteArray]
  }*/

  @inline def length(): scala.Int = {
    val rawPtr = castObjectToRawPtr(this)
    val ptr: Ptr[scala.Byte] = fromRawPtr(rawPtr)
    val lenPtr = ptr + 8 // FIXME: why StringByteArray._objSize doesn't work?
    loadInt(toRawPtr(lenPtr))
  }

  @inline private[lang] def backingBytePtr: Ptr[scala.Byte] = {
    val rawPtr = castObjectToRawPtr(this)
    //val arrPtr = elemRawPtr(ptr, 8L + 4)
    //arrPtr
    val ptr: Ptr[scala.Byte] = fromRawPtr(rawPtr)
    val offset = 8 + 4 // FIXME: why StringByteArray._objSize doesn't work?
    val arrPtr = ptr + offset
    arrPtr
  }

  @inline private[lang] def byteAt(index: scala.Int) = {
    //loadByte(elemRawPtr(backingBytePtr, index))
    backingBytePtr(index)
  }

  /*private[lang] def update(index: Int, byte: scala.Byte): Unit = {
    backingArray(index) = byte
  }*/

  def charAt(index: scala.Int): scala.Char = {
    if (index < 0 || index >= length) {
      throw new StringIndexOutOfBoundsException()
    }

    val byte = byteAt(index)
    (byte & 0xFF).toChar
  }

  def subSequence(start: scala.Int, end: scala.Int): CharSequence = {
    this.checkSubSequenceRange(start, end)

    val nBytes = end - start
    nBytes match {
      case 0 => EmptyStringContainer
      case 1 => new StringByteStruct1(this.byteAt(0))
      case 2 => new StringByteStruct2(this.byteAt(0), this.byteAt(1))
      case _ if this.length <= 128 => new SubStringSmallByteArray(
        this,
        start.toByte,
        nBytes.toByte
      )
      case _ => new SubStringSmallByteArray(
        this,
        start.toByte,
        nBytes.toByte
      )
    }
  }

}
/*final class StringByteArray(
  value: Array[scala.Byte]
) extends StringContainer {

  @inline def length(): scala.Int = value.length

  def charAt(index: scala.Int): scala.Char = {
    if (index < 0 || index >= length) {
      throw new StringIndexOutOfBoundsException()
    }

    (value(index) & 0xFF).toChar
  }

  def subSequence(start: scala.Int, end: scala.Int): CharSequence = {
    this.checkSubSequenceRange(start, end)

    val nBytes = end - start
    nBytes match {
      case 0 => EmptyStringContainer
      case 1 => new StringByteStruct1(value(0))
      case 2 => new StringByteStruct2(value(0), value(1))
      case _ if value.length <= 128 => new SubStringSmallByteArray(
        value,
        start.toByte,
        nBytes.toByte
      )
      case _ => new SubStringSmallByteArray(
        value,
        start.toByte,
        nBytes.toByte
      )
    }
  }

  //def getBytes(): Array[scala.Byte] = value
}*/

/*
sealed trait SubStringContainer extends StringContainer {

}*/

// Compact representation of small Latin1 Strings (max 128 bytes)
// with only a range of bytes being used (offset/count data)
final class SubStringSmallByteArray(
  parent: StringByteArray,
  offset: scala.Byte,
  count: scala.Byte
) extends StringContainer {
  require(
    parent.length <= 128,
    "invalid array length, can't be larger than 128"
  )

  @inline def length(): scala.Int = count

  def charAt(index: scala.Int): scala.Char = {
    if (index < 0 || index >= count) {
      throw new StringIndexOutOfBoundsException()
    }

    parent.charAt(offset + index)
    //(value(offset + index) & 0xFF).toChar
  }

  def subSequence(start: scala.Int, end: scala.Int): CharSequence = {
    this.checkSubSequenceRange(start, end)

    val nBytes = end - start
    nBytes match {
      case 0 => EmptyStringContainer
      case 1 => new StringByteStruct1(parent.byteAt(0))
      case 2 => new StringByteStruct2(
        parent.byteAt(0),
        parent.byteAt(1)
      )
      case _ if nBytes <= 128 => new SubStringSmallByteArray(
        parent,
        (offset + start).toByte,
        (count - start).toByte
      )
      case _ => new SubStringByteArray(
        parent,
        offset + start,
        count - start
      )
    }
  }

  //def getBytes(): Array[scala.Byte] = value
}

// Compact representation of Latin1 Strings (min 129 bytes)
// with only a range of bytes being used (offset/count data)
final class SubStringByteArray(
  parent: StringByteArray,
  offset: Int,
  count: Int
) extends StringContainer {
  require(
    parent.length >= 129,
    "length < 129, SubStringSmallByteArray must be used"
  )

  @inline def length(): scala.Int = count

  def charAt(index: scala.Int): scala.Char = {
    if (index < 0 || index >= count) {
      throw new StringIndexOutOfBoundsException()
    }

    parent.charAt(offset + index)
    //(value(offset + index) & 0xFF).toChar
  }

  def subSequence(start: scala.Int, end: scala.Int): CharSequence = {
    this.checkSubSequenceRange(start, end)

    val nBytes = end - start
    nBytes match {
      case 0 => EmptyStringContainer
      case 1 => new StringByteStruct1(parent.byteAt(0))
      case 2 => new StringByteStruct2(
        parent.byteAt(0),
        parent.byteAt(1)
      )
      case _ if nBytes <= 128 => new SubStringSmallByteArray(
        parent,
        (offset + start).toByte,
        (count - start).toByte
      )
      case _ => new SubStringByteArray(
        parent,
        offset + start,
        count - start
      )
    }
  }

  //def getBytes(): Array[scala.Byte] = value
}

// Standard representation of UTF16 Strings
// with all chars being used (no offset/count data)
final class StringCharArray(
  value: Array[scala.Char]
) extends StringContainer {

  @inline def length(): scala.Int = value.length

  def charAt(index: scala.Int): scala.Char = {
    if (index < 0 || index >= length) {
      throw new StringIndexOutOfBoundsException()
    }

    value(index)
  }

  def subSequence(start: scala.Int, end: scala.Int): CharSequence = {
    this.checkSubSequenceRange(start, end)

    val nBytes = end - start
    nBytes match {
      case 0 => EmptyStringContainer
      case _ if value.length <= 128 => new SubStringSmallCharArray(
        value,
        start.toByte,
        nBytes.toByte
      )
      case _ => new SubStringCharArray(
        value,
        start.toByte,
        nBytes.toByte
      )
    }
  }

  //def getBytes(): Array[scala.Byte] = value

  override def toString(): String = {
    new java.lang._String(0, value.length, value).asInstanceOf[String]
  }
}

// Standard representation of small UTF16 Strings (max 128 chars)
// with only a range of bytes being used (offset/count data)
final class SubStringSmallCharArray(
  val value: Array[scala.Char], // UTF16 <-> current String implementation
  val offset: scala.Byte,
  val count: scala.Byte
) extends StringContainer {
  require(
    value.length >= 129,
    "length < 129, SubStringSmallByteArray must be used"
  )

  @inline def length(): scala.Int = count

  def charAt(index: scala.Int): scala.Char = {
    if (index < 0 || index >= count) {
      throw new StringIndexOutOfBoundsException()
    }

    value(offset + index)
  }

  def subSequence(start: scala.Int, end: scala.Int): CharSequence = {
    this.checkSubSequenceRange(start, end)

    val nChars = end - start
    nChars match {
      case 0 => EmptyStringContainer
      case _ if nChars <= 128 => new SubStringSmallCharArray(
        value,
        (offset + start).toByte,
        (count - start).toByte
      )
      case _ => new SubStringCharArray(
        value,
        offset + start,
        count - start
      )
    }
  }

  //def getBytes(): Array[scala.Byte] = value

  override def toString(): String = {
    new java.lang._String(offset, count, value).asInstanceOf[String]
  }
}

// Standard representation of UTF16 Strings (min 129 chars)
// with only a range of bytes being used (offset/count data)
final class SubStringCharArray(
  val value: Array[scala.Char], // UTF16 <-> current String implementation
  val offset: Int,
  val count: Int
) extends StringContainer {
  require(
    value.length >= 129,
    "length < 129, SubStringSmallByteArray must be used"
  )

  @inline def length(): scala.Int = count

  def charAt(index: scala.Int): scala.Char = {
    if (index < 0 || index >= count) {
      throw new StringIndexOutOfBoundsException()
    }

    value(offset + index)
  }

  def subSequence(start: scala.Int, end: scala.Int): CharSequence = {
    this.checkSubSequenceRange(start, end)

    val nChars = end - start
    nChars match {
      case 0 => EmptyStringContainer
      case _ if nChars <= 128 => new SubStringSmallCharArray(
        value,
        (offset + start).toByte,
        (count - start).toByte
      )
      case _ => new SubStringCharArray(
        value,
        offset + start,
        count - start
      )
    }
  }

  //def getBytes(): Array[scala.Byte] = value

  override def toString(): String = {
    new java.lang._String(offset, count, value).asInstanceOf[String]
  }
}

final class CStringLatin1Wrapper(
  value: CString,
  val length: Int
  //charset: Charset // TODO: create other implementation with charset
) extends StringContainer {

  def charAt(index: scala.Int): scala.Char = {
    if (index < 0 || index >= length) {
      throw new StringIndexOutOfBoundsException()
    }

    value(index).toChar
  }

  def subSequence(start: scala.Int, end: scala.Int): CharSequence = {
    this.checkSubSequenceRange(start, end)

    val nBytes = end - start
    nBytes match {
      case 0 => EmptyStringContainer
      case _ => null // FIXME
      /*case _ if length <= 128 => new SubStringSmallCharArray(
        value,
        start.toByte,
        nBytes.toByte
      )*/
      /*case _ => new SubStringCharArray(
        value,
        start.toByte,
        nBytes.toByte
      )*/
    }
  }

  //def getBytes(): Array[scala.Byte] = value

  override def toString(): String = {
    // FIXME: reimplement fromCString here
    //new java.lang._String(0, length, fromCString(value)).asInstanceOf[String]
    fromCString(value)
  }
}

final class CompactString(
    var container: StringContainer
) extends StringContainer with Serializable { // with Comparable[String]

  private[lang] var cachedHashCode: Int = _

  /*def this(
      data: Array[scala.Byte],
      start: Int,
      length: Int
  ) = {

  }*/
  def this(string: String) = {
    this(StringContainerFactory.createStringContainer(string))
  }

  /*
  def this(
      data: Array[scala.Byte]
  ) = {
    this(StringContainerFactory.createLatin1StringContainer(data.clone()))
  }*/

  // Proxy methods
  def length(): scala.Int = container.length()
  def charAt(index: scala.Int): scala.Char = container.charAt(index)
  def subSequence(start: scala.Int, end: scala.Int): CharSequence = {
    container.subSequence(start, end)
  }
  //def getBytes(): Array[scala.Byte] = container.getBytes()

  override def toCompactString(): CompactString = this

  // FIXME: should return this when CompactString replaces String
  override def toString(): String = container.toString()
}

/** StringContainerFactory creates the appropriate StringContainer according to:
 *  1. the different String constructors
 *  2. the characteristics of the String (charset / length)
 *  This class is a helper for the String class itself, but could be used by
 *  AbstractStringBuilder as well.
 */
object StringContainerFactory {

  /*
  def createLatin1StringContainer(bytes: Array[scala.Byte]): StringContainer = {
    val nBytes = bytes.length
    //println("nBytes="+ nBytes)
    nBytes match {
      case 0 => EmptyStringContainer
      case 1 => new StringByteStruct1(bytes(0))
      case 2 => new StringByteStruct2(bytes(0), bytes(1))
      case _ => new StringByteArray(bytes)
    }
  }*/

  def createUTF16StringContainer(chars: Array[scala.Char]): StringContainer = {
    val nChars = chars.length
    nChars match {
      case 0 => EmptyStringContainer
      case _ => new StringCharArray(chars)
    }
  }

  /*
  def createStringContainer(chars: Array[scala.Char]): StringContainer = {
    val nChars = chars.length

    val bytes = new Array[scala.Byte](nChars)
    var i = 0
    while (i < nChars) {
      bytes(i) = (chars(i) >> 8).toByte
      i += 1
    }

    createLatin1StringContainer(bytes)
  }*/

  def createLatin1StringContainer(csq: CharSequence): StringContainer = {
    val nChars = csq.length()
    nChars match {
      case 0 => EmptyStringContainer
      case 1 => new StringByteStruct1(csq.charAt(0).toByte)
      case 2 => new StringByteStruct2(
        csq.charAt(0).toByte,
        csq.charAt(1).toByte
      )
      case _ => {
        val sba = StringByteArray(nChars)
        val bytesPtr = sba.backingBytePtr

        var i = 0
        while (i < nChars) {
          val byte = csq.charAt(i).toByte
          bytesPtr(i) = byte
          //storeByte(elemRawPtr(bytesPtr, i), byte)
          i += 1
        }

        sba
      }
    }

    /*val bytes = new Array[scala.Byte](nChars)

    var i = 0
    while (i < nChars) {
      bytes(i) = csq.charAt(i).toByte
      i += 1
    }

    createLatin1StringContainer(bytes)*/
  }
  def createUTF16StringContainer(csq: CharSequence): StringContainer = {
    val nChars = csq.length()
    val chars = new Array[scala.Char](nChars)

    var i = 0
    while (i < nChars) {
      chars(i) = csq.charAt(i)
      i += 1
    }

    createUTF16StringContainer(chars)
  }

  def createStringContainer(csq: CharSequence): StringContainer = {
    val nChars = csq.length

    var isLatin1 = true
    var i = 0
    while (isLatin1 && i < nChars) {
      if ((csq.charAt(i) >> 8) != 0) {
        isLatin1 = false
      } else i += 1
    }
    //println(BytesUtils.bytes2HexString(bytes))

    if (isLatin1)
      createLatin1StringContainer(csq)
    else
      createUTF16StringContainer(csq)
  }

}