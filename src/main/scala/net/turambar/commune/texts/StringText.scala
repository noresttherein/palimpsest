package net.turambar.commune.texts

import java.nio.charset.Charset
import java.util.Locale

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.collection.{IndexedSeqOptimized, immutable, mutable}

import net.turambar.commune.texts.StringText.StringIterator
import net.turambar.commune.texts.Text.CharIterator

/**
  * @author Marcin Mościcki
  */
trait ToStringText extends IndexedSeqOptimized[Char, IndexedText] with IndexedText {
	
	/* Seq methods. */
	
	override def length = toString.length
	
	override def apply(index: Int): Char = toString charAt index
	
	override protected def at(index: Int): Char = toString charAt index
	
	
	override def iterator: CharIterator = new StringIterator(toString)
	
	
	
	
	/* CharSequence methods. */
	
	override def charAt(index: Int): Char = toString.charAt(index)
	
	override def subSequence(start: Int, end: Int): CharSequence =
		toString.subSequence(start, end)
	
	/* StringInterface methods */
	
	override def codePointAt(index: Int): Int = toString.codePointAt(index)
	
	override def codePointBefore(index: Int): Int = toString.codePointBefore(index)
	
	override def codePointCount(beginIndex: Int, endIndex: Int): Int =
		toString.codePointCount(beginIndex, endIndex)
	
	override def offsetByCodePoints(index: Int, codePointOffset: Int): Int =
		toString.offsetByCodePoints(index, codePointOffset)
	
	override def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit =
		toString.getChars(srcBegin, srcEnd, dst, dstBegin)
	
	override def getBytes(charsetName: String): Array[Byte] = toString.getBytes(charsetName)
	
	override def getBytes(charset: Charset): Array[Byte] = toString.getBytes(charset)
	
	override def getBytes: Array[Byte] = toString.getBytes
	
	override def contentEquals(sb: StringBuffer): Boolean = toString.contentEquals(sb)
	
	override def contentEquals(sb: CharSequence): Boolean = toString.contentEquals(sb)
	
	override def equalsIgnoreCase(anotherString: String): Boolean = toString.equalsIgnoreCase(anotherString)
	
	override def equalsIgnoreCase(other: Text): Boolean = other match {
		case s :ToStringText => toString.equalsIgnoreCase(s.toString)
		case _ => ??? //todo
	}
	
	override def compareTo(anotherString: String): Int = toString.compareTo(anotherString)
	
	override def regionMatches(toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
		toString.regionMatches(toffset, other, ooffset, len)
	
	override def regionMatches(toffset: Int, other: Text, ooffset: Int, len: Int): Boolean =
		other match {
			case s :ToStringText => toString.regionMatches(toffset, s.toString, ooffset, len)
			case _ => ??? //todo
		}
	
	override def regionMatches(ignoreCase: Boolean, toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
		toString.regionMatches(ignoreCase, toffset, other, ooffset, len)
	
	override def regionMatches(ignoreCase: Boolean, toffset: Int, other: Text, ooffset: Int, len: Int): Boolean =
		other match {
			case s :ToStringText => toString.regionMatches(ignoreCase, toffset, s.toString, ooffset, len)
			case _ => ??? //todo
		}
	
	override def startsWith(prefix: String, toffset: Int): Boolean =
		toString.startsWith(prefix, toffset)
	
	override def startsWith(prefix: Text, toffset: Int): Boolean = prefix match {
		case s :ToStringText => toString.startsWith(s.toString, toffset)
		case _ => ??? //todo
	}
	
	override def startsWith(prefix: String): Boolean = toString.startsWith(prefix)
	
	override def startsWith(prefix: Text): Boolean = prefix match {
		case s :ToStringText => toString.startsWith(prefix.toString)
		case _ => ??? //todo
	}
	
	override def endsWith(suffix: String): Boolean = toString.endsWith(suffix)
	
	override def endsWith(suffix: Text): Boolean = suffix match {
		case s :ToStringText => toString.endsWith(s.toString)
		case _ => ??? //todo
	}
	
	override def indexOf(ch: Int): Int = toString.indexOf(ch)
	
	override def indexOf(ch: Int, fromIndex: Int): Int = toString.indexOf(ch, fromIndex)
	
	override def lastIndexOf(ch: Int): Int = toString.lastIndexOf(ch)
	
	override def lastIndexOf(ch: Int, fromIndex: Int): Int = toString.lastIndexOf(ch, fromIndex)
	
	override def indexOf(str: String): Int = toString.indexOf(str)
	
	override def indexOf(text: Text): Int = text match {
		case s :ToStringText => toString.indexOf(s.toString)
		case _ => ??? //todo
	}
	
	override def indexOf(str: String, fromIndex: Int): Int = toString.indexOf(str, fromIndex)
	
	override def indexOf(str: Text, fromIndex: Int): Int = str match {
		case s :ToStringText => toString.indexOf(s.toString, fromIndex)
		case _ => ??? //todo
	}
	
	override def lastIndexOf(str: String): Int = toString.lastIndexOf(str)
	
	override def lastIndexOf(text: Text): Int = text match {
		case s :ToStringText => toString.lastIndexOf(s.toString)
		case _ => ??? //todo
	}
	
	override def lastIndexOf(str: String, fromIndex: Int): Int = toString.lastIndexOf(str, fromIndex)
	
	override def lastIndexOf(str: Text, fromIndex: Int): Int = str match {
		case s :ToStringText => str.lastIndexOf(s.toString, fromIndex)
		case _ => ??? //todo
	}
	
	override def substring(beginIndex: Int): String = toString.substring(beginIndex)
	
	override def subtext(beginIndex: Int): Text = ???
	
	override def substring(beginIndex: Int, endIndex: Int): String = toString.substring(beginIndex, endIndex)
	
	override def subtext(beginIndex: Int, endIndex: Int): Text = ???
	
	override def concat(str: String): String = toString.concat(str)
	
	override def concat(str: Text): Text = ???
	
	override def replace(oldChar: Char, newChar: Char): Text = toString.replace(oldChar, newChar)
	
	override def matches(regex: String): Boolean = toString.matches(regex)

	override def matches(regex: Text): Boolean = ???
	
	override def contains(s: CharSequence): Boolean = toString.contains(s)
	
	override def replaceFirst(regex: String, replacement: String): String =
		toString.replaceFirst(regex, replacement)

	override def replaceFirst(regex: Text, replaement: Text): Text = ???
	

	override def replaceAll(regex: String, replacement: String): String =
		toString.replaceAll(regex, replacement)

	override def replaceAll(regex: Text, replacement: Text): Text = ???
	
	override def replace(target: CharSequence, replacement: CharSequence): String =
		toString.replace(target, replacement)
	

	override def split(regex: String, limit: Int): Array[String] = toString.split(regex, limit)

	override def split(regex: Text, limit: Int): Array[Text] = ???
	
	override def split(regex: String): Array[String] = toString.split(regex)
	
	override def split(regex: Text): Array[Text] = ???
	
	override def toLowerCase: Text = toString.toLowerCase
	
	override def toLowerCase(locale: Locale): Text = toString.toLowerCase(locale)
	
	override def toUpperCase: Text = toString.toUpperCase
	
	override def toUpperCase(locale: Locale): Text = toString.toUpperCase(locale)
	
	override def trim: Text = toString.trim
	
	override def toCharArray: Array[Char] = toString.toCharArray
	
	
	
	/* Ordered[Text] methods */
	
	override def compare(that: Text): Int = that match {
		case s :ToStringText => toString compareTo s.toString
		case _ => ??? //todo
	}
	
	
	override def equals(that :Any) = that match {
		case that :ToStringText => toString==that.toString
		case that :Text => toString.contentEquals(that)
		case _ => false
	}
	
	override def hashCode = toString.hashCode
}



class StringText(override final val toString :String) extends ToStringText

class LazyStringText(lazystring : =>String) extends ToStringText {
	@volatile private[this] var initer = lazystring _
	@volatile private[this] var text :String = _
	
	override def toString = {
		if (text == null) synchronized {
			if (text==null) {
				text = initer()
				initer = null
			}
		}
		text
	}
}


object StringText {
	def apply(string :String) :StringText = new StringText(string)
	
	def lzy(string : =>String) :ToStringText = new LazyStringText(string)
	
	def unapply(text :Text) = text match {
		case s :ToStringText => Some(s.toString)
		case _ => None
	}
	
	
	def newBuilder :mutable.Builder[Char, StringText] = (new StringBuilder).mapResult(StringText(_))
	
	//todo: verify by specialization if T =:= Char
	def genericBuilder[T] :mutable.Builder[T, IndexedSeq[T]] = IndexedSeq.newBuilder[T]
	
/*
	object CanBuildStringText extends CanBuildFrom[Text, Char, StringText] {
		override def apply(from: Text): mutable.Builder[Char, StringText] = from.newBuilder
		
		override def apply(): mutable.Builder[Char, StringText] = (new StringBuilder).mapResult(StringText.apply)
	}
*/
	
	
	class StringIterator(text :String, start :Int, end :Int) extends CharIterator {
		def this(content :String, start :Int=0) = this(content, start, content.length)
		
		@inline private final def content :String = text
		@inline private final def index = i
		
		private[this] var i=start
		
		
		override def head: Char = text.charAt(i)
		
		override def next(): Char = { val res = text.charAt(i); i+=1; res }
		
		override def hasNext: Boolean = i<end
		
		override def size :Int = { val diff = end-i; if (diff<=0) 0 else diff }
		
		override def hasFastSize: Boolean = true
		
		override def hasDefiniteSize = true
		
		override def take(n: Int): CharIterator =
			if (n >= end-i) this
			else new StringIterator(text, i, if (n >= end-i) end else i+n)
		
		override def drop(n: Int): Iterator[Char] =
			if (n<=0) this
			else new StringIterator(text, i+n, end)
		
		override def slice(from: Int, until: Int): Iterator[Char] =
			new StringIterator(text,
				if (from<=0) i else i+from,
				if (until >= end-i) end else i+until
			)
		
		override def toSeq: Seq[Char] = new SubstringText(text, i, 0 max end-i)
		
		override def toIndexedSeq: IndexedSeq[Char] = new SubstringText(text, i, 0 max end-i)
		
		override def sameElements(that: Iterator[_]): Boolean = that match {
			case si :StringIterator => size==si.size &&
				((text eq si.content) && i==si.index || super.sameElements(that))
			case ci :CharIterator if ci.hasFastSize =>
				size==ci.size && super.sameElements(that)
			case _ => super.sameElements(that)
		}
		
	}
	
}