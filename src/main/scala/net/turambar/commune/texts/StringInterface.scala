package net.turambar.commune.texts

import java.nio.charset.Charset
import java.util.Locale

/**
  * @author Marcin Mościcki
  */
trait StringInterface extends CharSequence { self :Text =>
	//	/**
	//	  * @see [[CharSequence#length]]
	//	  */
	//	def length: Int
	//
	//	/**
	//	  * @see [[CharSequence#charAt]]
	//	  */
	//	def charAt(index: Int): Char
	//
	//	/**
	//	  * @see [[CharSequence#subSequence(Int, Int)]]
	//	  */
	//	def subSequence(beginIndex: Int, endIndex: Int): CharSequence
	
	
	
	/**
	  * @see [[String#codePointAt]]
	  */
	def codePointAt(index :Int) :Int
	
	/**
	  * @see [[String#codePointBefore]]
	  */
	def codePointBefore(index: Int): Int
	
	/**
	  * @see [[String#codePointCount]]
	  */
	def codePointCount(beginIndex: Int, endIndex: Int): Int
	
	/**
	  * @see [[String#offsetByCodePoints]]
	  */
	def offsetByCodePoints(index: Int, codePointOffset: Int): Int
	
	/**
	  * @see [[String#getChars]]
	  */
	def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int) :Unit
	
	/**
	  * @see [[String#getBytes(String)]]
	  */
	def getBytes(charsetName: String): Array[Byte]
	
	/**
	  * @see [[String#getBytes(Charset)]]
	  */
	def getBytes(charset: Charset): Array[Byte]
	
	/**
	  * @see [[String#getBytes()]]
	  */
	def getBytes: Array[Byte]
	
	/**
	  * @see [[String#contentEquals(StringBuffer)]]
	  */
	def contentEquals(sb: StringBuffer): Boolean
	
	/**
	  * @see [[String#contentEquals(CharSequence)]]
	  */
	def contentEquals(sb: CharSequence): Boolean
	
	/**
	  * @see [[String#equalsIgnoreCase]]
	  */
	def equalsIgnoreCase(anotherString: String): Boolean
	
	def equalsIgnoreCase(other :Text) :Boolean
	
	/**
	  * @see [[String#compareTo]]
	  */
	def compareTo(anotherString: String): Int
	
	def compareTo(another :Text) :Int
	
	/**
	  * @see [[String#compareToIgnoreCase]]
	  */
	def compareToIgnoreCase(str: String): Int = compareToIgnoreCase(Text(str))
	
	def compareToIgnoreCase(other :Text) = Text.CaseInsensitiveOrdering.compare(this, other)
	
	/**
	  * @see [[String#regionMatches(Int, String, Int, Int]]
	  */
	def regionMatches(toffset: Int, other: String, ooffset: Int, len: Int): Boolean
	
	def regionMatches(toffset: Int, other: Text, ooffset: Int, len: Int) :Boolean
	
	/**
	  * @see [[String#regionMatches(Boolean, Int, String, Int, Int]]
	  */
	def regionMatches(ignoreCase: Boolean, toffset: Int, other: String, ooffset: Int, len: Int): Boolean
	
	def regionMatches(ignoreCase :Boolean, toffset :Int, other :Text, ooffset :Int, len :Int):Boolean
	
	/**
	  * @see [[String#startsWith(String, Int)]]
	  * @param prefix
	  * @param toffset
	  * @return
	  */
	def startsWith(prefix: String, toffset: Int): Boolean
	
	def startsWith(prefix: Text, toffset: Int): Boolean
	
	/**
	  * @see [[String#startsWith(String)]]
	  */
	def startsWith(prefix: String): Boolean
	
	def startsWith(prefix :Text) :Boolean
	
	/**
	  * @see [[String#endsWith(String)]]
	  */
	def endsWith(suffix: String): Boolean
	
	def endsWith(suffix :Text) :Boolean
	
	/**
	  * @see [[String#hashCode]]
	  */
	override def hashCode: Int
	
	/**
	  * @see [[String#indexOf(Int)]]
	  */
	def indexOf(ch: Int): Int
	
	/**
	  * @see [[String#indexOf(Int, Int)]]
	  */
	def indexOf(ch: Int, fromIndex: Int): Int
	
	/**
	  * @see [[String#lastIndexOf(Int)]]
	  */
	def lastIndexOf(ch: Int): Int
	
	/**
	  * @see [[String#lastIndexOf(Int, Int)]]
	  */
	def lastIndexOf(ch: Int, fromIndex: Int): Int
	
	/**
	  * @see [[String#indexOf(String)]]
	  */
	def indexOf(str: String): Int
	
	def indexOf(text :Text) :Int
	
	/**
	  * @see [[String#indexOf(String, Int)]]
	  */
	def indexOf(str: String, fromIndex: Int): Int
	
	def indexOf(str :Text, fromIndex :Int) :Int
	
	/**
	  * @see [[String#lastIndexOf(String)]]
	  */
	def lastIndexOf(str: String): Int
	
	def lastIndexOf(text :Text) :Int
	
	/**
	  * @see [[String#lastIndexOf(String, Int)]]
	  */
	def lastIndexOf(str :String, fromIndex :Int) :Int
	
	/**
	  * @see [[String#lastIndexOf(String, Int)]]
	  */
	def lastIndexOf(str :Text, fromIndex :Int) :Int
	
	/**
	  * @see [[String#substring(Int)]]
	  */
	def substring(beginIndex: Int): String
	
	def subtext(beginIndex :Int) :Text
	
	/**
	  * @see [[String#substring(Int, Int)]]
	  */
	def substring(beginIndex: Int, endIndex: Int): String
	
	def subtext(beginIndex :Int, endIndex :Int) :Text
	
	/**
	  * @see [[String#concat]]
	  */
	@deprecated("use concat(Text) or +/++", "StringInterface")
	def concat(str: String): String
	
	def concat(str: Text) :Text
	
	/** Replaces all occurrences of `oldChar` with `newChar` just as in [[String#replace]], but returns a [[Text]] instance instead.
	  * @see [[String#replace(Char, Char)]]
	  */
	def replace(oldChar: Char, newChar: Char): Text
	
	/**
	  * @see [[String#matches(String)]]
	  */
	def matches(regex: String): Boolean
	
	def matches(regex :Text) :Boolean
	
	/**
	  * @see [[String#contains(CharSequence)]]
	  */
	def contains(s: CharSequence): Boolean
	
	/**
	  * @see [[String#replaceFirst(String, String)]]
	  */
	@deprecated("use replaceFirst(Text, Text)", "StringInterface")
	def replaceFirst(regex: String, replacement: String): String
	
	def replaceFirst(regex :Text, replacement :Text) :Text
	
	/**
	  * @see [[String#replaceAll(String, String)]]
	  */
	@deprecated("use replaceAll(Text, Text)", "StringInterface")
	def replaceAll(regex: String, replacement: String): String
	
	def replaceAll(regex :Text, replacement :Text) :Text
	
	/**
	  * @see [[String#replace(CharSequence, CharSequence)]]
	  */
	@deprecated("use replace(Text, Text)", "StringInterface")
	def replace(target: CharSequence, replacement: CharSequence): String
	
	/**
	  * @see [[String#split(String, Int)]]
	  */
	@deprecated("use replace(Text, Int)", "StringInterface")
	def split(regex: String, limit: Int): Array[String]
	
	def split(regex :Text, limit :Int) :Array[Text]
	
	/**
	  * @see [[String#split(String)]]
	  */
	@deprecated("use split(Text)", "StringInterface")
	def split(regex: String): Array[String]
	
	def split(regex :Text) :Array[Text]
	
	
	/**
	  * @see [[String#toLowerCase()]]
	  * @return
	  */
	def toLowerCase: Text
	
	/**
	  * @see [[String#toLowerCase(Locale)]]
	  * @param locale
	  * @return
	  */
	def toLowerCase(locale: Locale) :Text
	
	/**
	  * @see [[String#toUpperCase()]]
	  */
	def toUpperCase :Text
	
	/**
	  * @see [[String#toUpperCase(Locale)]]
	  */
	def toUpperCase(locale: Locale): Text
	
	/**
	  * @see [[String#trim]]
	  */
	def trim :Text
	
	/**
	  * @see [[String#toCharArray]]
	  */
	def toCharArray: Array[Char]
	
	
	
	def intern: Text = toString.intern
}
