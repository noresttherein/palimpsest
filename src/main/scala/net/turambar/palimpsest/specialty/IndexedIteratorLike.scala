package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.IndexedIteratorLike.AbstractFunctionIterator

/** Base mixin `Iterator` trait for implementations iterating over indexed sequences.
  * Re-implements several `Iterator` methods based on the assumption that [[IndexedIteratorLike#index]]
  * points at the current element (`head`) and that modifying it will change the position of the iterator.
 *
  * @tparam I self-type of this iterator (type returned by `TraversableOnce` methods)
  * @author Marcin Mościcki
  */
trait IndexedIteratorLike[+E, +I<:Iterator[E]]
	extends BufferedIterator[E]
{ this :I =>
	
	/** The index of the current element (one returned by `head`). */
	protected[this] var index :Int
	
	/** The index after the last element of this iterator (exclusive limit), `end>=index` */
	protected[this] var end :Int
	
	override def hasNext: Boolean = index < end
	
	override def next :E = { val res = head; index += 1; res }
	
	override def size = if (end<index) 0 else end-index
	
	override def hasDefiniteSize = true
	
	
	
	override def drop(n: Int): I = {
		if (n>0)
			index += n
		this
	}
	
	override def take(n: Int): I = {
		if (index+n<end)
			end = index+n
		this
	}
	
	override def slice(from: Int, until: Int): I = {
		if (index + until < end)
			end = index + until
		if (from>0)
			index += from
		this
	}

	override def toString = s"$stringPrefix#$index>>"
	
	def stringPrefix :String = "Iterator"
}



object IndexedIteratorLike {
	
	
	abstract class AbstractFunctionIterator[+E](protected[this] final var index :Int, protected[this] final var end :Int)(f :Int=>E)
		extends BufferedIterator[E]
	{
		override def head = f(index)
	}

	class IndexedFunctionIterator[+E](start :Int, end :Int)(f :Int=>E)
		extends AbstractFunctionIterator[E](start, end)(f) with IndexedIteratorLike[E, Iterator[E]]
}




/** Base mixin `Iterator` trait for implementations iterating over indexed sequences in reverse.
  * Re-implements several `Iterator` methods based on the assumption that [[IndexedIteratorLike#index]]
  * points at the current element (`head`) and that modifying it will change the position of the iterator.
  * @tparam I self-type of this iterator (type returned by `TraversableOnce` methods)
  * @author Marcin Mościcki
  */
trait ReverseIndexedIteratorLike[+E, +I<:Iterator[E]] extends BufferedIterator[E] { this :I =>
	
	/** The index of the current element. */
	protected[this] var index :Int
	/** The index of the last element of this iterator (inclusive), lower than `index`. */
	protected[this] var end :Int
	
	override def hasNext: Boolean = index >= end
	
	override def next :E = { val res = head; index -= 1; res }
	
	override def size = if (end>index) 0 else index-end +1
	
	override def hasDefiniteSize = true
	
	
	
	override def drop(n: Int): I = {
		if (n>0)
			index -= n
		this
	}
	
	override def take(n: Int): I = {
		if (index-n > end)
			end = index-n
		this
	}
	
	override def slice(from: Int, until: Int): I = {
		if (index - until > end)
			end = index - until
		if (from>0)
			index -= from
		this
	}
	
	override def toString = s"$stringPrefix#$index<<"
	
	def stringPrefix :String = "Iterator"
	
}



object ReverseIndexedIteratorLike {
	
	class ReverseIndexedFunctionIterator[+E](start :Int, end :Int)(f :Int=>E)
		extends AbstractFunctionIterator[E](start, end)(f) with ReverseIndexedIteratorLike[E, Iterator[E]]
}
