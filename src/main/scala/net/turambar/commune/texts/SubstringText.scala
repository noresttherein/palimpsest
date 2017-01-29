package net.turambar.commune.texts

import java.nio.CharBuffer
import java.nio.charset.Charset
import java.util.Locale

import scala.collection.immutable.IndexedSeq
import scala.collection.{IndexedSeqOptimized, immutable, mutable}

import net.turambar.commune.texts.StringText.StringIterator
import net.turambar.commune.texts.Text.CharIterator

/**
  * @author Marcin Mościcki
  */


class SubstringText private[commune] (content :String, offset :Int, override final val length :Int)
	extends IndexedSeqOptimized[Char, IndexedText] with IndexedText
{
	@inline private final def whole = content
	@inline private final def firstIndex = offset
	
	
	override protected def at(index: Int): Char = content.charAt(offset+index)
	
	override def compact = content.substring(offset, offset+length)
	
	/* Seq methods */
	
	
	
	override def apply(index: Int): Char =
		if (index<0 || index>=length) throw new StringIndexOutOfBoundsException(index)
		else content.charAt(offset+index)
	
	override def iterator: CharIterator = new StringIterator(content, offset, offset+length)
	
	
	
	override def newBuilder: mutable.Builder[Char, IndexedText] = StringText.newBuilder
	
	override def genericBuilder[B]: mutable.Builder[B, IndexedSeq[B]] = StringText.genericBuilder[B]
	
	
	/* CharSequence methods */
	
	
	override def charAt(index: Int): Char =
		if (index<0 || index>=length) throw new StringIndexOutOfBoundsException(index)
		else content.charAt(offset+index)
	
	override def subSequence(start: Int, end: Int): CharSequence =
		if (start>end || start<0 || end>length)
			throw new StringIndexOutOfBoundsException
		else content.subSequence(offset+start, offset+end)
	
	/* StringInterface methods */

	override def codePointAt(index: Int): Int =
		if (index<0 || index>=length) throw new StringIndexOutOfBoundsException(index)
		else content.codePointAt(offset+index)
	
	override def codePointBefore(index: Int): Int =
		if (index<1 || index>length) throw new StringIndexOutOfBoundsException(index)
		else content.codePointBefore(offset+index)
	
	override def codePointCount(beginIndex: Int, endIndex: Int): Int =
		if (beginIndex>endIndex || beginIndex<0 || endIndex>length)
			throw new StringIndexOutOfBoundsException
		else content.codePointCount(offset+beginIndex, offset+endIndex)
	
	override def offsetByCodePoints(index: Int, codePointOffset: Int): Int =
		if (index<0 || index>length) throw new StringIndexOutOfBoundsException
		else offset + content.offsetByCodePoints(offset+index, codePointOffset) //todo: fishy
	
	override def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit =
		if (srcBegin>srcEnd || srcBegin<0 || srcEnd>length) throw new StringIndexOutOfBoundsException
		else content.getChars(offset+srcBegin, offset+srcEnd, dst, dstBegin)
	
	override def getBytes(charsetName: String): Array[Byte] =
		getBytes(Charset.forName(charsetName))
	
	override def getBytes(charset: Charset): Array[Byte] = {
		val buff = charset.encode(CharBuffer.wrap(content, offset, offset + length))
		if (buff.hasArray && buff.position==0 && buff.array.length==buff.limit)
			buff.array
		else {
			val copy = new Array[Byte](buff.limit-buff.position)
			buff.get(copy)
			copy
		}
	}
	
	override def getBytes: Array[Byte] = getBytes(Charset.defaultCharset)
	
	override def contentEquals(sb: StringBuffer): Boolean =
		length==sb.length && {
			var i=0
			while(i<length && content.charAt(offset+i)==sb.charAt(i))
				i+=1
			i==length
		}
		    
	override def contentEquals(sb: CharSequence): Boolean =
		length==sb.length && {
			var i = 0
			while(i<length && content.charAt(offset+i)==sb.charAt(i))
				i+=1
			i==length
		}
	
	override def equalsIgnoreCase(anotherString: String): Boolean =
		length == anotherString.length &&
			content.regionMatches(true, offset, anotherString, 0, length)
		
	
	override def equalsIgnoreCase(other: Text): Boolean = other match {
		case s :ToStringText => equalsIgnoreCase(s.toString)
		case s :SubstringText =>
			(s eq this) || s.length==length && content.regionMatches(true, offset, s.whole, s.firstIndex, length)
		case _ => ??? //todo
	}
	
	override def compareTo(anotherString: String): Int = {
		var i = 0; val end = length min anotherString.length
		while(i<end && content.charAt(offset+i)==anotherString(i))
			i += 1
		if (i==end)
			length - anotherString.length
		else content.charAt(offset+i) - anotherString.charAt(i)
	}
	
	
	override def regionMatches(toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
		len <=0 || toffset>=0 && toffset<=length-len && content.regionMatches(offset + toffset, other, ooffset, len)
	
	override def regionMatches(toffset: Int, other: Text, ooffset: Int, len: Int): Boolean = ???
	
	override def regionMatches(ignoreCase: Boolean, toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
		len <=0 || toffset>=0 && toffset<=length-len && content.regionMatches(ignoreCase, offset + toffset, other, ooffset, len)
	
	override def regionMatches(ignoreCase: Boolean, toffset: Int, other: Text, ooffset: Int, len: Int): Boolean = ???
	
	override def startsWith(prefix: String, toffset: Int): Boolean =
		toffset>=0 && toffset<=length && content.startsWith(prefix, offset+toffset)
	
	override def startsWith(prefix: Text, toffset: Int): Boolean = ???
	
	override def startsWith(prefix: String): Boolean = content.startsWith(prefix, offset)
	
	override def startsWith(prefix: Text): Boolean = ???
	
	override def endsWith(suffix: String): Boolean = content.startsWith(suffix, length-suffix.length)
	
	override def endsWith(suffix: Text): Boolean = ???
	
	override def indexOf(ch: Int): Int = content.indexOf(ch, offset) match {
		case n if n<0 || n>=offset+length => -1
		case n => n-offset
	}
	
	override def indexOf(ch: Int, fromIndex: Int): Int =
		if (fromIndex>=length) -1
		else content.indexOf(ch, if (fromIndex<0) offset else offset+fromIndex) match {
			case n if n<0 || n>=offset+length => -1
			case n => n-offset
		}
	
	override def lastIndexOf(ch: Int): Int = content.lastIndexOf(ch, offset+length) match {
		case n if n<offset => -1
		case n => n-offset
	}
	
	override def lastIndexOf(ch: Int, fromIndex: Int): Int =
		if (fromIndex<0) -1
		else content.lastIndexOf(ch, if (fromIndex>=length) offset+length else offset+fromIndex) match {
			case n if n<offset => -1
			case n => n-offset
		}
	
	override def indexOf(str: String): Int = content.indexOf(str, offset) match {
		case -1 => -1
		case n if n+str.length > offset+length => -1
		case n => n-offset
	}
	
	override def indexOf(text: Text): Int = ???
	
	override def indexOf(str: String, fromIndex: Int): Int =
		if (fromIndex>=length-str.length) -1
		else content.indexOf(str, if (fromIndex<0) offset else offset+fromIndex) match {
			case n if n<0 || n>=length-str.length => -1
			case n => n-offset
		}
	
	override def indexOf(str: Text, fromIndex: Int): Int = ???
	
	override def lastIndexOf(str: String): Int =
		content.lastIndexOf(str, offset+length-str.length) match {
			case n if n<offset => -1
			case n => n-offset
		}
	
	override def lastIndexOf(text: Text): Int = ???
	
	override def lastIndexOf(str: String, fromIndex: Int): Int =
		if (fromIndex<str.length) -1
		else content.lastIndexOf(str, if (fromIndex>length-str.length) offset+length-str.length else offset+fromIndex) match {
			case n if n<offset => -1
			case n => n-offset
		}
	
	override def lastIndexOf(str: Text, fromIndex: Int): Int = ???
	
	override def substring(beginIndex: Int): String =
		if (beginIndex<0 || beginIndex>length) throw new StringIndexOutOfBoundsException(beginIndex)
		else content.substring(offset+beginIndex)
	
	override def subtext(beginIndex: Int): Text = ???
	
	override def substring(beginIndex: Int, endIndex: Int): String =
		if (endIndex<beginIndex || beginIndex<0 || endIndex>length) throw new StringIndexOutOfBoundsException
		else content.substring(offset+beginIndex, offset+endIndex)
	
	override def subtext(beginIndex: Int, endIndex: Int): Text = ???
	
	override def concat(str: String): String =
		if (length==0) str
		else {
			val res = new StringBuilder(length+str.length)
			var i = offset; val end = offset+length
			while(i<end) {
				res.append(charAt(i))
				i+=1
			}
			res.append(str)
			res.toString
		}
	
	override def concat(str: Text): Text = ???
	
	override def replace(oldChar: Char, newChar: Char): Text = ???

	override def matches(regex: String): Boolean = ???
	
	override def matches(regex: Text): Boolean = ???
	
	override def contains(s: CharSequence): Boolean = ???
	
	override def replaceFirst(regex: String, replacement: String): String = ???
	
	override def replaceFirst(regex: Text, replaement: Text): Text = ???

	override def replaceAll(regex: String, replacement: String): String = ???

	override def replaceAll(regex: Text, replacement: Text): Text = ???
	
	override def replace(target: CharSequence, replacement: CharSequence): String = ???
	
	override def split(regex: String, limit: Int): Array[String] = ???

	override def split(regex: Text, limit: Int): Array[Text] = ???
	
	override def split(regex: String): Array[String] = ???

	override def split(regex: Text): Array[Text] = ???
	
	override def toLowerCase: Text = ???
	
	override def toLowerCase(locale: Locale): Text = ???
	
	override def toUpperCase: Text = ???
	
	override def toUpperCase(locale: Locale): Text = ???
	
	override def trim: Text = ???
	
	override def toCharArray: Array[Char] = ???
	
	
	
	
	override def compare(that: Text): Int = ???
	
//	override protected[this] def newBuilder: mutable.Builder[Char, IndexedText] =
//		(new StringBuilder).mapResult(StringText(_))
	

}
