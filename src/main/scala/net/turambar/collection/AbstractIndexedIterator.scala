package net.turambar.collection

import net.turambar.collection.AbstractIndexedIterator.AbstractFunctionIterator

/**
  * @author Marcin Mościcki
  */
trait AbstractIndexedIterator[+E]
	extends BufferedIterator[E]
{
	/** The index of the current element (one returned by `head`). */
	protected[this] var index :Int
	/** The index after the last element of this iterator (exclusive limit), `end>=index` */
	protected[this] var end :Int
	
	override def hasNext: Boolean = index < end
	
	override def next :E = { val res = head; index += 1; res }
	
	override def size = if (end<index) 0 else end-index
	
	override def hasDefiniteSize = true
	
	
	
	override def drop(n: Int): this.type = {
		if (n>0)
			index += n
		this
	}
	
	override def take(n: Int): this.type = {
		if (index+n<end)
			end = index+n
		this
	}
	
	override def slice(from: Int, until: Int): this.type = {
		if (index + until < end)
			end = index + until
		if (from>0)
			index += from
		this
	}

	override def toString = stringPrefix + "#" + index
	
	def stringPrefix :String = "Iterator"
}



object AbstractIndexedIterator {
	
	
	abstract class AbstractFunctionIterator[+E](protected[this] final var index :Int, protected[this] final var end :Int)(f :Int=>E)
		extends BufferedIterator[E]
	{
		override def head = f(index)
	}

	class IndexedFunctionIterator[+E](start :Int, end :Int)(f :Int=>E)
		extends AbstractFunctionIterator[E](start, end)(f) with AbstractIndexedIterator[E]
}





trait AbstractReverseIndexedIterator[+E] extends BufferedIterator[E] {
	/** The index of the current element. */
	protected[this] var index :Int
	/** The index of the last element of this iterator (inclusive), lower than `index`. */
	protected[this] var end :Int
	
	override def hasNext: Boolean = index >= end
	
	override def next :E = { val res = head; index -= 1; res }
	
	override def size = if (end>index) 0 else index-end +1
	
	override def hasDefiniteSize = true
	
	
	
	override def drop(n: Int): this.type = {
		if (n>0)
			index -= n
		this
	}
	
	override def take(n: Int): this.type = {
		if (index-n > end)
			end = index-n
		this
	}
	
	override def slice(from: Int, until: Int): this.type = {
		if (index - until > end)
			end = index - until
		if (from>0)
			index -= from
		this
	}
	
	override def toString = stringPrefix + "#" + index
	
	def stringPrefix :String = "ReverseIterator"
	
}



object AbstractReverseIndexedIterator {
	
	class ReverseIndexedFunctionIterator[+E](start :Int, end :Int)(f :Int=>E)
		extends AbstractFunctionIterator[E](start, end)(f) with AbstractReverseIndexedIterator[E]
}
