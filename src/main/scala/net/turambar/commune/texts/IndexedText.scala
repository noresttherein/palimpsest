package net.turambar.commune.texts

import scala.collection.immutable.{IndexedSeq, Seq}
import scala.collection.{GenIterable, GenSeq, IndexedSeqLike, mutable}

import net.turambar.commune.texts.IndexedText.IndexedTextIterator
import net.turambar.commune.texts.Text.CharIterator


trait IndexedText extends Text with IndexedSeq[Char] with IndexedSeqLike[Char, IndexedText] {
	
	override def compact :IndexedText = this
	
	protected def at(index :Int) :Char
	
	protected def between(start :Int, end :Int) :IndexedText
	
    /* Seq methods */
	
	override def iterator :CharIterator = new IndexedTextIterator(this)
	
	
	override def newBuilder: mutable.Builder[Char, IndexedText] = IndexedText.newBuilder
	
	override def genericBuilder[B]: mutable.Builder[B, IndexedSeq[B]] = IndexedText.genericBuilder
	
	
	override def slice(from: Int, until: Int): IndexedText = {
		val start = from max 0 min length
		val end = until max start min length
		between(start, end)
	}
	
	override def segmentLength(p: (Char) => Boolean, from: Int): Int = {
		val len = length
		val start = from max 0 min len
		var i = start
		while(i<len && p(at(i)))
			i+=1
		i-start
	}
	
	
	
	override def indexWhere(p: (Char) => Boolean, from: Int): Int = {
		var i = math.max(0, from); val end = length
		while(i<end && !p(at(i)))
			i+=1
		if (i<end) i else -1
	}
	
	override def lastIndexWhere(p: (Char) => Boolean, end: Int): Int = {
		var i = math.min(end, length)
		while (i>=0 && !p(at(i)))
			i-=1
		i
	}
	
	
	
	
	/* StringInterface methods */

	
	override def endsWith(suffix: String): Boolean = startsWith(suffix, length-suffix.length)
	
	override def endsWith(suffix: Text): Boolean = startsWith(suffix, length-suffix.length)
	
	
	override def startsWith(prefix: String, toffset: Int): Boolean = {
		val len = length; val end = toffset + prefix.length
		toffset >= 0 && toffset <= len-prefix.length && {
			var i = 0
			while (i<end && at(toffset+i)==prefix.charAt(i))
				i += 1
			i == end
		}
	}
	
	override def startsWith(prefix: Text, toffset: Int): Boolean = prefix match {
		case text :IndexedText =>
			val len = length; val end = toffset + text.length
			toffset >= 0 && toffset <= len-text.length && {
				var i = 0
				while (i<end && at(toffset+i)==text.at(i))
					i += 1
				i == end
			}
		case _ =>
			val len = length
			toffset >= 0 && {
				var i = toffset; val pattern = prefix.iterator
				while (i < len && pattern.hasNext && at(i) == pattern.next())
					i += 1
				!pattern.hasNext
			}
	}
	
	override def startsWith(prefix: String): Boolean = {
		val end = prefix.length
		end == 0 || end <= length && {
			var i = 0
			while (i<end && prefix.charAt(i)==at(i))
				i += 1
			i==end
		}
	}
	
	override def startsWith(prefix: Text): Boolean = prefix match {
		case text :IndexedText =>
			val end = prefix.length
			end == 0 || end <= length && {
				var i = 0
				while (i<end && text.at(i)==at(i))
					i += 1
				i==end
			}
		case _ => super.startsWith(prefix)
	}
	
	
	override def indexOf(ch: Int): Int = {
		"".indexOf(ch)
	}
	
	/**
	  * @see [[String#indexOf(Int, Int)]]
	  */
	override def indexOf(ch: Int, fromIndex: Int): Int = ???
	
	/**
	  * @see [[String#lastIndexOf(Int)]]
	  */
	override def lastIndexOf(ch: Int): Int = ???
	
	/**
	  * @see [[String#lastIndexOf(Int, Int)]]
	  */
	override def lastIndexOf(ch: Int, fromIndex: Int): Int = ???
	
	/**
	  * @see [[String#indexOf(String)]]
	  */
	override def indexOf(str: String): Int = ???
	
	override def indexOf(text: Text): Int = ???
	
	/**
	  * @see [[String#indexOf(String, Int)]]
	  */
	override def indexOf(str: String, fromIndex: Int): Int = ???
	
	override def indexOf(str: Text, fromIndex: Int): Int = ???
	
	/**
	  * @see [[String#lastIndexOf(String)]]
	  */
	override def lastIndexOf(str: String): Int = ???
	
	override def lastIndexOf(text: Text): Int = ???
	
	/**
	  * @see [[String#lastIndexOf(String, Int)]]
	  */
	override def lastIndexOf(str: String, fromIndex: Int): Int = ???
	
	/**
	  * @see [[String#lastIndexOf(String, Int)]]
	  */
	override def lastIndexOf(str: Text, fromIndex: Int): Int = ???
	
	override def subtext(beginIndex: Int): IndexedText =
		if (beginIndex<0 || beginIndex > length)
			throw new StringIndexOutOfBoundsException(beginIndex)
		else between(beginIndex, length)
	
	override def subtext(beginIndex: Int, endIndex: Int): IndexedText =
		if (beginIndex<0 || endIndex>length || endIndex<beginIndex)
			throw new StringIndexOutOfBoundsException
		else between(beginIndex, endIndex)
	
	
	
	
	
	override def equalsIgnoreCase(anotherString: String): Boolean =
		length==anotherString.length && regionMatches(true, 0, anotherString, 0, anotherString.length)
	
	override def equalsIgnoreCase(other: Text): Boolean =
		(this eq other) || length==other.length && regionMatches(true, 0, other, 0, length)
	
	
	
	override def contentEquals(sb: StringBuffer): Boolean =
		length==sb.length && {
			var i = length-1
			while (i>0 && at(i)==sb.charAt(i))
				i -= 1
			i < 0
		}
	
	override def contentEquals(sb: CharSequence): Boolean = sb match {
		case t :IndexedText => equals(t)
		case _ =>
			val len = length
			len == sb.length && {
				var i = 0
				while (i<len && at(i)==sb.charAt(i))
					i += 1
				i == len
			}
	}
	
	
	
	override def compare(that: Text): Int = that match {
		case other :IndexedText => IndexedText.CaseSensitiveOrdering.compare(this, other)
		
		case _ => super.compare(that)
	}
	
	
	
	
	
	
	override def equals(that :Any) :Boolean = that match {
		case t :IndexedText =>
			(t eq this) || (t canEqual this) && t.length==length && {
				var i=length-1
				while(i>=0 && at(i) == t.at(i))
					i -= 1
				i < 0
			}
		case t :Text if t canEqual this =>
			t.iterator sameElements iterator
		
		case _ => false
	}
	
	override def hashCode :Int = {
		var hash = 0; var i=0; val end = length
		while (i<length) {
			hash = 31 * hash + at(i)
			i += 1
		}
		hash
	}
}




object IndexedText {
	
	def apply(text :String) :IndexedText = new StringText(text)
	
	def lzy(text : =>String) :IndexedText = new LazyStringText(text)
	
	def newBuilder :mutable.Builder[Char, IndexedText] = StringText.newBuilder
	
	def genericBuilder[T] :mutable.Builder[T, IndexedSeq[T]] = IndexedSeq.newBuilder
	
	
	object CaseInsensitiveOrdering extends Ordering[IndexedText] {
		override def compare(x: IndexedText, y: IndexedText): Int = ???
	}
	
	implicit object CaseSensitiveOrdering extends Ordering[IndexedText] {
		override def compare(x: IndexedText, y: IndexedText): Int =
			if (x.isEmpty)
				if (y.isEmpty) 0 else -1
			else if (y.isEmpty)
				1
			else {
				var i =0; val end = x.length min y.length; var cmp = 0
				while (i < end && cmp==0) {
					cmp = x.at(i) - y.at(i)
					i += 1
				}
				
				if (cmp!=0) cmp
				else x.length - y.length
			}
		
	}
	
	
	class IndexedTextIterator(text :IndexedText, start :Int, end :Int) extends CharIterator {
		
		def this(text :IndexedText, start :Int=0) = this(text, start, text.length)
		
		@inline final protected def content :Text = text
		@inline final protected def index :Int = start
		private[this] var i = start
		
		override def head: Char = text.at(i)
		
		override def next(): Char = { val res = text.at(i); i+=1; res }
		
		override def hasNext: Boolean = i < end
		
		override def size: Int = { val diff = end-i; if (diff < 0) 0 else diff }
		
		override def hasDefiniteSize: Boolean = true
		
		override def hasFastSize = true
		
		override def toSeq :IndexedText = text.slice(i, end)
		
		override def toIndexedSeq :IndexedText = text.slice(i, end)
		
		override def take(n: Int): CharIterator =
			if (n>=end-i) this
			else new IndexedTextIterator(text, i, i+n)
		
		override def drop(n: Int): CharIterator =
			if (n<=0) this
			else new IndexedTextIterator(text, i+n, end)
		
		override def slice(from: Int, until: Int): CharIterator =
			new IndexedTextIterator(
				text,
				if (from<=0) i else i+from,
				if (until >=end-i) end else i+until
			)
		
		override def sameElements(that: Iterator[_]): Boolean = that match {
			case i :IndexedTextIterator =>
				(i eq this) || (i.content eq content) && i.index==this.i || super.sameElements(that)
			case _ => super.sameElements(that)
		}
	}
	
}