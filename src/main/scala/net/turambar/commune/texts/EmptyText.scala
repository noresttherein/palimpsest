package net.turambar.commune.texts

import java.nio.charset.Charset
import java.util.Locale

import scala.collection.immutable.IndexedSeq
import scala.collection.{IndexedSeqOptimized, immutable, mutable}

/**
  * @author Marcin Mościcki
  */
class EmptyText extends IndexedSeqOptimized[Char, IndexedText] with IndexedText {
	@inline private final def outOfBounds = throw new StringIndexOutOfBoundsException("EmptyText")
	
	override def apply(v1: Int): Char = outOfBounds
	
	override def compare(that: Text): Int =
		if (that.isEmpty) 0
		else -1
	
	def length=0
	
	override def isEmpty = true
	
	
	override def charAt(index: Int): Char = outOfBounds
	
	override def subSequence(start: Int, end: Int): CharSequence =
		if (start==end && start==0) this
		else outOfBounds
	
	override def codePointAt(index: Int): Int = outOfBounds
	
	override def codePointBefore(index: Int): Int = outOfBounds
	
	override def codePointCount(beginIndex: Int, endIndex: Int): Int =
		if (beginIndex==0 && endIndex==0) 0
		else outOfBounds
	
	override def offsetByCodePoints(index: Int, codePointOffset: Int): Int =
		if (index==0 && codePointOffset==0) 0
		else outOfBounds
	
	override def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit = ()
	
	override def getBytes(charsetName: String): Array[Byte] = Array.emptyByteArray
	
	override def getBytes(charset: Charset): Array[Byte] = Array.emptyByteArray
	
	override def getBytes: Array[Byte] = Array.emptyByteArray
	
	override def contentEquals(sb: StringBuffer): Boolean = sb.length==0
	
	override def contentEquals(sb: CharSequence): Boolean = sb.length==0
	
	override def equalsIgnoreCase(anotherString: String): Boolean = anotherString.length==0
	
	override def equalsIgnoreCase(other: Text): Boolean = other.isEmpty
	
	override def compareTo(anotherString: String): Int =
		if (anotherString.length>0) -1
		else 0
	
	override def regionMatches(toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
		if (len==0 && toffset==0 && ooffset<=other.length) true
		else outOfBounds
	
	override def regionMatches(toffset: Int, other: Text, ooffset: Int, len: Int): Boolean =
		if (len==0 && toffset==0 && ooffset<=other.length) true
		else outOfBounds
	
	override def regionMatches(ignoreCase: Boolean, toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
		if (len==0 && toffset==0 && ooffset<=other.length) true
		else outOfBounds
		
	override def regionMatches(ignoreCase: Boolean, toffset: Int, other: Text, ooffset: Int, len: Int): Boolean =
		if (len==0 && toffset==0 && ooffset<=other.length) true
		else outOfBounds
	
	override def startsWith(prefix: String, toffset: Int): Boolean =
		toffset==0 && prefix.length==0
	
	override def startsWith(prefix: Text, toffset: Int): Boolean =
		toffset==0 && prefix.isEmpty
	
	override def startsWith(prefix: String): Boolean = prefix.length==0
	
	override def startsWith(prefix: Text): Boolean = prefix.isEmpty
	
	override def endsWith(suffix: String): Boolean = suffix.length==0
	
	override def endsWith(suffix: Text): Boolean = suffix.isEmpty
	
	override def indexOf(ch: Int): Int = -1
	
	
	override def indexOf(ch: Int, fromIndex: Int): Int = -1
	
	override def lastIndexOf(ch: Int): Int = -1
	
	override def lastIndexOf(ch: Int, fromIndex: Int): Int = -1
	
	override def indexOf(str: String): Int =
		if (str.length==0) 0
		else -1
	
	override def indexOf(text: Text): Int =
		if (text.isEmpty) 0
		else -1
	
	override def indexOf(str: String, fromIndex: Int): Int =
		if (fromIndex<=0 && str.length==0) 0
		else -1
	
	override def indexOf(str: Text, fromIndex: Int): Int =
		if (fromIndex<=0 && str.isEmpty) 0
		else -1

	override def lastIndexOf(str: String): Int =
		if (str.length==0) 0
		else -1
	
	override def lastIndexOf(text: Text): Int =
		if (text.isEmpty) 0
		else -1
	
	override def lastIndexOf(str: String, fromIndex: Int): Int =
		if (fromIndex>=0 && str.length==0) 0
		else -1
	
	override def lastIndexOf(str: Text, fromIndex: Int): Int =
		if (fromIndex>=0 && str.isEmpty) 0
		else -1
	
	override def substring(beginIndex: Int): String =
		if (beginIndex==0) ""
		else outOfBounds
	
	override def subtext(beginIndex: Int): Text =
		if (beginIndex==0) this
		else outOfBounds
	
	override def substring(beginIndex: Int, endIndex: Int): String =
		if (beginIndex==0 && endIndex==0) ""
		else outOfBounds
	
	override def subtext(beginIndex: Int, endIndex: Int): Text =
		if (beginIndex==0 && endIndex==0) ""
		else outOfBounds
	
	override def concat(str: String): String = str
	
	override def concat(str: Text): Text = str
	
	override def replace(oldChar: Char, newChar: Char): Text = this
	
	override def matches(regex: String): Boolean =
		regex.length==0 || "".matches(regex)
	
	override def matches(regex: Text): Boolean =
		regex.isEmpty || "".matches(regex)
	
	override def contains(s: CharSequence): Boolean = s.length==0
	
	override def replaceFirst(regex: String, replacement: String): String = "".replaceFirst(regex, replacement)
	
	override def replaceFirst(regex: Text, replacement: Text): Text =
		if (matches(regex)) replacement
		else this
	
	override def replaceAll(regex: String, replacement: String): String = "".replaceAll(regex, replacement)

	//todo: verify what patterns actually match empty strings and how/where
	override def replaceAll(regex: Text, replacement: Text): Text = replaceAll(regex.toString, replacement.toString)
	
	override def replace(target: CharSequence, replacement: CharSequence): String =
		if (target.length==0) replacement.toString
		else ""
	
	override def split(regex: String, limit: Int): Array[String] = "".split(regex, limit)

	override def split(regex: Text, limit: Int): Array[Text] = "".split(regex, limit).map(Text(_))
	
	override def split(regex: String): Array[String] = "".split(regex)
	
	override def split(regex: Text): Array[Text] = "".split(regex).map(Text(_))
	
	override def toLowerCase: Text = this
	
	override def toLowerCase(locale: Locale): Text = this
	
	override def toUpperCase: Text = this
	
	override def toUpperCase(locale: Locale): Text = this
	
	override def trim: Text = this
	
	override def toCharArray: Array[Char] = Array.emptyCharArray
	
	override def toString = ""
	
	override def hashCode = 0
	
	override def equals(that :Any) = that match {
		case t :Text => t.isEmpty
		case _ => false
	}
}


object EmptyText extends EmptyText

