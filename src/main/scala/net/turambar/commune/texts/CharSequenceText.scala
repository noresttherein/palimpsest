package net.turambar.commune.texts

import java.nio.CharBuffer
import java.nio.charset.Charset
import java.util.Locale

import scala.collection.IndexedSeqOptimized

import net.turambar.commune.texts.Text.CharSequenceIterator

/**
  * @author Marcin Mościcki
  */
trait CharSequenceText extends IndexedSeqOptimized[Char, IndexedText] with IndexedText {
	def toCharSequence :CharSequence
	
	override def toString = toCharSequence.toString
	
	override protected def at(i :Int) = toCharSequence.charAt(i)
	
	override def iterator = new CharSequenceIterator(toCharSequence)
	
	
	
	/* Seq methods. */
	
	override def length = toCharSequence.length
	
	override def apply(index: Int): Char = toCharSequence charAt index
	
	
	/* Ordered[Text] methods */
	
	override def compare(that: Text): Int = that match {
		case s :CharSequenceText => CharSequence.
		case _ => ??? //todo
	}
	
	
	
	
	override def compareTo(anotherString: String): Int = {
		
	}
	
	

	
	override def charAt(index: Int): Char = toCharSequence.charAt(index)
	
	
	override def subSequence(start: Int, end: Int): CharSequence = //todo my own slice?
		toCharSequence.subSequence(start, end)
	
	/* StringInterface methods */
	
	override def codePointAt(index: Int): Int = toCharSequence.codePointAt(index)
	
	override def codePointBefore(index: Int): Int = toCharSequence.codePointBefore(index)
	
	override def codePointCount(beginIndex: Int, endIndex: Int): Int =
		toCharSequence.codePointCount(beginIndex, endIndex)
	
	override def offsetByCodePoints(index: Int, codePointOffset: Int): Int =
		toCharSequence.offsetByCodePoints(index, codePointOffset)
	
	
	
	override def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit =
		CharBuffer.wrap(toCharSequence, srcBegin, srcEnd-srcBegin).get(dst, dstBegin, srcEnd-srcBegin)
	
	override def toCharArray: Array[Char] = {
		val chars = toCharSequence
		val res = new Array[Char](chars.length)
		CharBuffer.wrap(chars).get(res)
		res
	}
	
	
	override def getBytes(charsetName: String): Array[Byte] =
		getBytes(Charset.forName(charsetName))
	
	override def getBytes(charset: Charset): Array[Byte] = {
		val buff = charset.encode(CharBuffer.wrap(toCharSequence))
		if (buff.hasArray && buff.position==0 && buff.array.length==buff.limit)
			buff.array
		else {
			val copy = new Array[Byte](buff.limit-buff.position)
			buff.get(copy)
			copy
		}
	}
	
	override def getBytes: Array[Byte] = getBytes(Charset.defaultCharset)
	
		
	
	
	
	
	override def indexOf(ch: Int): Int = toCharSequence.indexOf(ch)
	
	override def indexOf(ch: Int, fromIndex: Int): Int = toCharSequence.indexOf(ch, fromIndex)
	
	override def lastIndexOf(ch: Int): Int = toCharSequence.lastIndexOf(ch)
	
	override def lastIndexOf(ch: Int, fromIndex: Int): Int = toCharSequence.lastIndexOf(ch, fromIndex)
	
	override def indexOf(str: String): Int = toCharSequence.indexOf(str)
	
	override def indexOf(text: Text): Int = text match {
		case s :CharSequenceText => toCharSequence.indexOf(s.toCharSequence)
		case _ => ??? //todo
	}
	
	override def indexOf(str: String, fromIndex: Int): Int = toCharSequence.indexOf(str, fromIndex)
	
	override def indexOf(str: Text, fromIndex: Int): Int = str match {
		case s :CharSequenceText => toCharSequence.indexOf(s.toCharSequence, fromIndex)
		case _ => ??? //todo
	}
	
	override def lastIndexOf(str: String): Int = toCharSequence.lastIndexOf(str)
	
	override def lastIndexOf(text: Text): Int = text match {
		case s :CharSequenceText => toCharSequence.lastIndexOf(s.toCharSequence)
		case _ => ??? //todo
	}
	
	override def lastIndexOf(str: String, fromIndex: Int): Int = toCharSequence.lastIndexOf(str, fromIndex)
	
	override def lastIndexOf(str: Text, fromIndex: Int): Int = str match {
		case s :CharSequenceText => str.lastIndexOf(s.toCharSequence, fromIndex)
		case _ => ??? //todo
	}
	
	
	
	
	override def startsWith(prefix: String, toffset: Int): Boolean =
		toCharSequence.startsWith(prefix, toffset)
	
	override def startsWith(prefix: Text, toffset: Int): Boolean = prefix match {
		case s :CharSequenceText => toCharSequence.startsWith(s.toCharSequence, toffset)
		case _ => ??? //todo
	}
	
	override def startsWith(prefix: String): Boolean = toCharSequence.startsWith(prefix)
	
	override def startsWith(prefix: Text): Boolean = prefix match {
		case s :CharSequenceText => toCharSequence.startsWith(prefix.toCharSequence)
		case _ => ??? //todo
	}
	
	override def endsWith(suffix: String): Boolean = toCharSequence.endsWith(suffix)
	
	override def endsWith(suffix: Text): Boolean = suffix match {
		case s :CharSequenceText => toCharSequence.endsWith(s.toCharSequence)
		case _ => ??? //todo
	}
	
	
	
	
	
	override def contentEquals(sb: StringBuffer): Boolean = contentEquals(sb :CharSequence)
	
	override def contentEquals(sb: CharSequence): Boolean = {
		val chars = toCharSequence
		val len = sb.length
		chars.length==len && {
			var i = 0
			while (i<len && sb.charAt(i)==chars.charAt(i))
				i+=1
			i==len
		}
	}
	
	override def equalsIgnoreCase(anotherString: String): Boolean = anotherString.equals
	
	override def equalsIgnoreCase(other: Text): Boolean = other match {
		case s :CharSequenceText => toCharSequence.equalsIgnoreCase(s.toCharSequence)
		case _ => ??? //todo
	}
	
	override def regionMatches(toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
		toCharSequence.regionMatches(toffset, other, ooffset, len)
	
	override def regionMatches(toffset: Int, other: Text, ooffset: Int, len: Int): Boolean =
		other match {
			case s :CharSequenceText => toCharSequence.regionMatches(toffset, s.toCharSequence, ooffset, len)
			case _ => ??? //todo
		}
	
	override def regionMatches(ignoreCase: Boolean, toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
		toCharSequence.regionMatches(ignoreCase, toffset, other, ooffset, len)
	
	override def regionMatches(ignoreCase: Boolean, toffset: Int, other: Text, ooffset: Int, len: Int): Boolean =
		other match {
			case s :CharSequenceText => toCharSequence.regionMatches(ignoreCase, toffset, s.toCharSequence, ooffset, len)
			case _ => ??? //todo
		}
	

	
	override def substring(beginIndex: Int): String = toCharSequence.substring(beginIndex)
	
	override def subtext(beginIndex: Int): Text = ???
	
	override def substring(beginIndex: Int, endIndex: Int): String = toCharSequence.substring(beginIndex, endIndex)
	
	override def subtext(beginIndex: Int, endIndex: Int): Text = ???
	
	override def concat(str: String): String = toCharSequence.concat(str)
	
	override def concat(str: Text): Text = ???
	
	override def replace(oldChar: Char, newChar: Char): Text = toCharSequence.replace(oldChar, newChar)
	
	override def matches(regex: String): Boolean = toCharSequence.matches(regex)

	override def matches(regex: Text): Boolean = ???
	
	override def contains(s: CharSequence): Boolean = toCharSequence.contains(s)
	
	override def replaceFirst(regex: String, replacement: String): String =
		toCharSequence.replaceFirst(regex, replacement)

	override def replaceFirst(regex: Text, replaement: Text): Text = ???
	

	override def replaceAll(regex: String, replacement: String): String =
		toCharSequence.replaceAll(regex, replacement)

	override def replaceAll(regex: Text, replacement: Text): Text = ???
	
	override def replace(target: CharSequence, replacement: CharSequence): String =
		toCharSequence.replace(target, replacement)
	

	override def split(regex: String, limit: Int): Array[String] = toCharSequence.split(regex, limit)

	override def split(regex: Text, limit: Int): Array[Text] = ???
	
	override def split(regex: String): Array[String] = toCharSequence.split(regex)
	
	override def split(regex: Text): Array[Text] = ???
	
	override def toLowerCase: Text = toCharSequence.toLowerCase
	
	override def toLowerCase(locale: Locale): Text = toCharSequence.toLowerCase(locale)
	
	override def toUpperCase: Text = toCharSequence.toUpperCase
	
	override def toUpperCase(locale: Locale): Text = toCharSequence.toUpperCase(locale)
	
	override def trim: Text = toCharSequence.trim
	

	

	
	
	
	
	override def equals(that :Any) = that match {
		case that :CharSequenceText => toCharSequence == that.toCharSequence
		case that :Text => contentEquals(that)
		case _ => false
	}
	
	
	
	@volatile private[this] var hash = 0
	
	override def hashCode = {
		if (hash==0 && toCharSequence.length!=0)
			hash = toCharSequence.hashCode
		hash
	}
	
}


class CharSequenceAdapterText(final override val toCharSequence :CharSequence) extends CharSequenceText

class LazyCharSequenceText(text : => CharSequence) extends CharSequenceText {
	@volatile private[this] var initializer = text _
	@volatile private[this] var chars :CharSequence = _
	
	override def toCharSequence :CharSequence = {
		if (chars==null) synchronized {
			if (chars==null) {
				chars = initializer()
				initializer = null
			}
		}
		chars
	}
	
}

object CharSequenceText {
	def apply(text :CharSequence) :IndexedText = new CharSequenceAdapterText(text)
	
	def lzy(text : =>CharSequence) :IndexedText = new LazyCharSequenceText(text)
	
	def unapply(text :Text) = text match {
		case t :CharSequenceText => Some(t.toCharSequence)
		case _ => None
	}
		
		
}



