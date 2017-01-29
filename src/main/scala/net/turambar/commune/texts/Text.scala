package net.turambar.commune.texts

import java.nio.charset.Charset
import java.util.Locale

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.{GenIterable, GenSeq, GenTraversableOnce, IndexedSeqLike, SeqLike, immutable, mutable}
import scala.collection.immutable.{IndexedSeq, Seq}

import net.turambar.commune.texts.CodePoint.EqualCodePoint
import net.turambar.commune.texts.StringText.StringIterator
import net.turambar.commune.texts.Text.CharIterator



trait Textable {
	def toText :Text
}


/**
  * @author Marcin Mościcki
  */
trait Text extends StringInterface with Seq[Char] with SeqLike[Char, Text] with Ordered[Text] with Textable {
	
	override def toText :Text = this
	
	def compact :Text = this
	
	def iterator :CharIterator
	
//	protected def between(from :Int, until :Int) :Text
	
	/* Seq methods */
	
	override def newBuilder: mutable.Builder[Char, Text] = Text.newBuilder
	
	override def genericBuilder[B]: mutable.Builder[B, Seq[B]] = Text.genericBuilder
	
//	override def slice(from: Int, until: Int): Text = subtext(from, until)
	
	
	
	
	
	override def indexOf[B >: Char](elem: B): Int = elem match {
		case c :Char => indexOf(c :Int)
		case EqualCodePoint(c) => indexOf(c)
		case _ => -1
	}
	
	override def indexOf[B >: Char](elem: B, from: Int): Int = elem match {
		case c :Char => indexOf(c :Int, from)
		case EqualCodePoint(c) => indexOf(c, from)
		case _ => -1
	}
	
	override def lastIndexOf[B >: Char](elem: B): Int = elem match {
		case c :Char => lastIndexOf(c :Int)
		case EqualCodePoint(c) => lastIndexOf(c)
		case _ => -1
	}
	
	override def lastIndexOf[B >: Char](elem: B, end: Int): Int = elem match {
		case c :Char => lastIndexOf(c :Int, end)
		case EqualCodePoint(c) => last
	}
	
	override def contains[A1 >: Char](elem: A1): Boolean = elem match {
		case char :Char => indexOf(char :Int) >= 0
		case EqualCodePoint(c) => indexOf(c) >= 0
		case _ => false
	}
	
	/*
	
		
		/*  These can't be specialized for Char as GenSeq[B] is not specialized and neither the method results are
		 *  we could at most check if argument seq is not in fact Text (or our own nice specialized collections),
		 *  but is it really worth the effort?
		 */
		override def startsWith[@specialized(Char) B](that: GenSeq[B], offset: Int): Boolean = super.startsWith(that, offset)
		
		override def endsWith[@specialized(Char) B](that: GenSeq[B]): Boolean = super.endsWith(that)
		
		override def indexOfSlice[@specialized(Char) B >: Char](that: GenSeq[B]): Int = super.indexOfSlice(that)
		
		override def indexOfSlice[@specialized(Char) B >: Char](that: GenSeq[B], from: Int): Int = super.indexOfSlice(that, from)
		
		override def lastIndexOfSlice[@specialized(Char) B >: Char](that: GenSeq[B]): Int = super.lastIndexOfSlice(that)
		
		override def lastIndexOfSlice[@specialized(Char) B >: Char](that: GenSeq[B], end: Int): Int = super.lastIndexOfSlice(that, end)
		
		override def containsSlice[@specialized(Char) B](that: GenSeq[B]): Boolean = super.containsSlice(that)
	*/
	
	
//	override def corresponds[@specialized(Char) B](that: GenSeq[B])(p: (Char, B) => Boolean): Boolean = super.corresponds(that)(p)
	
	
/*
	//no point in specializing these as Function1 is not specialized for Char
	override def segmentLength(p: (Char) => Boolean, from: Int): Int = super.segmentLength(p, from)
	
	override def indexWhere(p: (Char) => Boolean, from: Int): Int = super.indexWhere(p, from)
	
	override def lastIndexWhere(p: (Char) => Boolean, end: Int): Int = super.lastIndexWhere(p, end)
*/
	
	
	override def +:[@specialized(Char) B >: Char, That](elem: B)(implicit bf: CanBuildFrom[Text, B, That]): That = super.+:(elem)
	
	override def :+[@specialized(Char) B >: Char, That](elem: B)(implicit bf: CanBuildFrom[Text, B, That]): That = super.:+(elem)
	
	override def ++[B >: Char, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Text, B, That]): That = super.++(that)
	
	override def ++:[B >: Char, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[Text, B, That]): That = super.++:(that)
	
	override def ++:[B >: Char, That](that: Traversable[B])(implicit bf: CanBuildFrom[Text, B, That]): That = super.++:(that)
	
	
	override def padTo[@specialized(Char) B >: Char, That](len: Int, elem: B)(implicit bf: CanBuildFrom[Text, B, That]): That = super.padTo(len, elem)
	

	
	override def take(n: Int): Text = super.take(n)
	
	override def drop(n: Int): Text = super.drop(n)
	
	override def takeWhile(p: (Char) => Boolean): Text = super.takeWhile(p)
	
	override def grouped(size: Int): Iterator[Text] = super.grouped(size)
	
	override def sliding(size: Int): Iterator[Text] = super.sliding(size)
	
	override def sliding(size: Int, step: Int): Iterator[Text] = super.sliding(size, step)
	
	override def takeRight(n: Int): Text = super.takeRight(n)
	
	override def dropRight(n: Int): Text = super.dropRight(n)
	
	override def copyToArray[B >: Char](xs: Array[B], start: Int, len: Int): Unit = super
		.copyToArray(xs, start, len)
	

	
	
	
	override def tail: Text = subtext(1)
	
	override def init: Text = subtext(0, length-1)
	
	override def dropWhile(p: (Char) => Boolean): Text = super.dropWhile(p)
	
	override def splitAt(n: Int): (Text, Text) = super.splitAt(n)
	
//	override def tails: Iterator[Text] = super.tails
//
//	override def inits: Iterator[Text] = super.inits
	
	
	
	
	
	
	
	
	
	/* StringInterface with CharSequence methods */
	
	

	
	override def charAt(index: Int): Char = apply(index)
	
	
	
	override def substring(beginIndex: Int): String = ???
	
	override def subtext(beginIndex: Int): Text = ???
	
	override def substring(beginIndex: Int, endIndex: Int): String = ???
	
	override def subtext(beginIndex: Int, endIndex: Int): Text = ???
	
	override def subSequence(start: Int, end: Int): CharSequence = subtext(start, end)
	

	
	
	override def indexOf(ch: Int): Int = toString.indexOf
	
	/**
	  * @see [[String#indexOf(Int, Int)]]
	  */
	override def indexOf(ch: Int, fromIndex: Int): Int = ???
	
	override def lastIndexOf(ch: Int): Int = ???
	
	override def lastIndexOf(ch: Int, fromIndex: Int): Int = ???
	
	override def indexOf(str: String): Int = ???
	
	override def indexOf(text: Text): Int = ???
	
	override def indexOf(str: String, fromIndex: Int): Int = ???
	
	override def indexOf(str: Text, fromIndex: Int): Int = ???
	
	override def lastIndexOf(str: String): Int = ???
	
	override def lastIndexOf(text: Text): Int = ???
	
	override def lastIndexOf(str: String, fromIndex: Int): Int = ???
	
	override def lastIndexOf(str: Text, fromIndex: Int): Int = ???
	
	
	
	override def startsWith(prefix: String): Boolean = startsWith(new StringIterator(prefix))
	
	override def startsWith(prefix: Text): Boolean = startsWith(prefix.iterator)
	
	@inline protected final def startsWith(prefix :CharIterator) :Boolean = {
		val ti = iterator
		while (ti.hasNext && prefix.hasNext && ti.next==prefix.next)
			()
		!prefix.hasNext
	}
	
	
	override def startsWith(prefix: String, toffset: Int): Boolean =
		regionMatches(toffset, prefix, 0, prefix.length)
	
//	override def startsWith(prefix: Text, toffset: Int): Boolean
//	override def endsWith(suffix: String): Boolean = ???
//	override def endsWith(suffix: Text): Boolean = ???
	

	override def regionMatches(toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
		regionMatches(false, toffset, other, ooffset, len)
	
	override def regionMatches(toffset: Int, other: Text, ooffset: Int, len: Int): Boolean =
		regionMatches(false, toffset, other, ooffset, len)
	
	override def compareToIgnoreCase(str: String): Int = compareToIgnoreCase(new StringText(str))
	
	override def compareToIgnoreCase(other: Text): Int = Text.CaseInsensitiveOrdering.compare(this, other)
	
	override def compareTo(anotherString: String): Int = compare(new StringText(anotherString))
	
	override def compare(other: Text): Int = Text.CaseSensitiveOrdering.compare(this, other)
//	override def compareTo(that: Text): Int = super.compareTo(that)
	
	
	override def equalsIgnoreCase(anotherString: String): Boolean =
		regionMatches(true, 0, anotherString, 0, anotherString.length)
	
	override def equalsIgnoreCase(other: Text): Boolean =
		Text.CaseInsensitiveOrdering.equiv(this, other)
		
	
	
	def contentEquals(s :String) :Boolean = contentEquals(s :CharSequence)
	
	override def contentEquals(sb: StringBuffer): Boolean = contentEquals(sb :CharSequence)
		
	override def contentEquals(sb: CharSequence): Boolean = {
		val it = iterator; var i = 0; val len = sb.length
		while (i<len && it.hasNext && it.next==sb.charAt(i))
			i += 1
		i==len && !it.hasNext
	}
	
	override def sameElements[B >: Char](that: GenIterable[B]): Boolean = that match {
		case t :Text =>
		case _ => iterator sameElements that.iterator
						
	}
	
	
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Text]
	
	override def equals(that :Any) :Boolean = that match {
		case text :Text if text.canEqual(this) =>
			iterator sameElements text.iterator
		case _ => false
	}
	
	
	override def hashCode: Int = {
		if (isEmpty) 0
		else {
			var hash=0; var i = iterator
			while (i.hasNext)
				hash = 31 * i.next
			hash
		}
	}
	

	
	
}




object Text extends (String=>IndexedText) {
	def apply(value :String) :IndexedText = new StringText(value)
	
	def apply(chars :CharSequence) :Text = ???
	
	def valueOf(obj: Any): Text = obj match {
		case null => "null"
		case t :Textable => t.toText
		case _ => new StringText(obj.toString)
	}
	
	def valueOf(chars :Array[Char]) :Text = String.valueOf(chars)
	
	def valueOf(data: Array[Char], offset: Int, count: Int): Text = String.valueOf(data, offset, count)
	
	def copyValueOf(chars :Array[Char]) :Text = valueOf(chars)
	
	def copyValueOf(data: Array[Char], offset: Int, count: Int): Text = valueOf(data, offset, count)
	
	def valueOf(x :Boolean) :Text = String.valueOf(x)
	def valueOf(x :Byte) :Text = String.valueOf(x)
	def valueOf(x :Char) :Text = String.valueOf(x)
	def valueOf(x :Short) :Text = String.valueOf(x)
	def valueOf(x :Int) :Text = String.valueOf(x)
	def valueOf(x :Long) :Text = String.valueOf(x)
	def valueOf(x :Float) :Text = String.valueOf(x)
	def valueOf(x :Double) :Text = String.valueOf(x)
	
//	/**
//	  * @see [[String#format(String, Array[Any])]]
//	  */
//	@deprecated("use format(Locale, Text, Any*)", "StringInterface")
//	def format(format: String, args: Any*): Text = String.format(format, args:_*)
	
	def format(format :Text, args :AnyRef*) :Text = String.format(format, args:_*)
	
//	/**
//	  * @see [[String#format(Locale, String, Array[Any])]]
//	  */
//	@deprecated("use format(Locale, Text, Any*)", "StringInterface")
//	def format(l: Locale, format: String, args: Any*): String
	
	def format(locale :Locale, format :Text, args :AnyRef*) :Text = String.format(locale, format, args:_*)
//	/**
//	  * @see [[String#join(CharSequence, Array[CharSequence])]]
//	  */
//	@deprecated("use join(Text, Text*)")
//	def join(delimiter: CharSequence, elements: CharSequence*): String = String.join(delimiter, elements:_*)
//
	def join(delimiter :Text, elements :Text*) :Text = ???
//
//	/**
//	  * @see [[String#join(CharSequence, Iterable)]]
//	  */
//	@deprecated("use join(Text, Iterable)")
//	def join(delimiter: CharSequence, elements: java.lang.Iterable[_ <: CharSequence]): String =
//
	def join(delimiter :Text, elements :Iterable[CharSequence]) :Text = ???
	
	
	
	@inline final implicit def TextFromCharSequence(chars :CharSequence) :Text = apply(chars)
	@inline final implicit def TextFromString(string :String) :Text = apply(string)
	@inline final implicit def TextToString(text :Text) :String = text.toString
	
	
	def newBuilder :mutable.Builder[Char, Text] = StringText.newBuilder
	
	def genericBuilder[T] :mutable.Builder[T, IndexedSeq[T]] = IndexedSeq.newBuilder[T]
	
	
	implicit object CanBuildText extends CanBuildFrom[Text, Char, Text] {
		override def apply(from: Text): mutable.Builder[Char, Text] = from.newBuilder
		
		override def apply(): mutable.Builder[Char, Text] = Text.newBuilder
	}
	
	
	object CaseInsensitiveOrdering extends Ordering[Text] {
		override def compare(x: Text, y: Text): Int = ???
	}
	
	
	
	implicit object CaseSensitiveOrdering extends Ordering[Text] {
		override def compare(x: Text, y: Text): Int =
			if (x.isEmpty)
				if (y.isEmpty) 0 else -1
			else if (x.isEmpty) 1
			else {
				val xi=x.iterator; val yi=y.iterator
				while (xi.hasNext && yi.hasNext) (xi.next, yi.next) match {
					case (x, y) if x==y => ()
					case (x, y) =>
						return x-y
				}
				if (xi.hasNext) -1
				else if (yi.hasNext) 1
				else 0
			}
		
	}
	
	
	trait CharIterator extends BufferedIterator[Char] {
		override def head: Char
		
		override def next(): Char
		
		def hasFastSize :Boolean
		
		override def sameElements(that: Iterator[_]): Boolean = that match {
			case i :CharIterator if i eq this => true
			
			case i :CharIterator if i.hasFastSize && hasFastSize =>
				size==i.size && {
					while (hasNext && that.hasNext) //overriden for Char specialisation
						if (next != that.next)
							return false
					!hasNext && !that.hasNext
				}
			case _ => super.sameElements(that)
		}
	}
	
	
	class CharSequenceIterator(text :CharSequence, start :Int, end :Int) extends CharIterator {
		def this(content :CharSequence, start :Int=0) = this(content, start, content.length)
		
		@inline private final def content :CharSequence = text
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
			else new CharSequenceIterator(text, i, if (n >= end-i) end else i+n)
		
		override def drop(n: Int): Iterator[Char] =
			if (n<=0) this
			else new CharSequenceIterator(text, i+n, end)
		
		override def slice(from: Int, until: Int): Iterator[Char] =
			new CharSequenceIterator(text,
				if (from<=0) i else i+from,
				if (until >= end-i) end else i+until
			)
		
		override def toSeq: Seq[Char] = new SubstringText(text, i, 0 max end-i)
		
		override def toIndexedSeq: IndexedSeq[Char] = new SubstringText(text, i, 0 max end-i)
		
		override def sameElements(that: Iterator[_]): Boolean = that match {
			case si :CharSequenceIterator => size==si.size &&
				((text eq si.content) && i==si.index || super.sameElements(that))
			case ci :CharIterator if ci.hasFastSize =>
				size==ci.size && super.sameElements(that)
			case _ => super.sameElements(that)
		}
		
		
	}
	

	
	
	private[texts] object Constants {
		final val LF = 0x0A
		final val FF = 0x0C
		final val CR = 0x0D
		final val SU = 0x1A
		
		
		final val True :Text = "true"
		final val False :Text = "false"
//		final val Byte = Array.tabulate(256)(_.toString :Text)
	}
	
}






