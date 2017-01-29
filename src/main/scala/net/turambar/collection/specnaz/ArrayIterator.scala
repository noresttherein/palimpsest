package net.turambar.collection.specnaz

import net.turambar.collection.specnaz.SpecIterator.{ReverseSpecSeqIterator, SpecSeqIterator}

/**
  * @author Marcin Mościcki
  */
class ArrayIterator[@specialized(Reified) +E](array :Array[E], final protected[this] var index :Int, final protected[this] var end :Int)
	extends SpecSeqIterator[E]
{
	@inline final override def head: E = array(index)
	
	@inline final override def next(): E = { val res = array(index); index += 1; res }
	
	@inline final override def hasNext = index < end

	
	@inline final override def copyToArray[B >: E](xs: Array[B], start: Int, len :Int): Unit =
		Array.copy(array, index, xs, start, len min (xs.length-start) min size)
	
	@inline final override def toIndexedSeq = toSeq.toIndexedSeq
	
	@inline final override def toSeq :SpecSeq[E] = ArraySlice.subseq[E](array, index, size)
	
	override def sameElements(that: Iterator[_]): Boolean = that match {
		case i :ArrayIterator[_] if i.sameViewAs(array, index, end) => true
		case _ => super.sameElements(that)
	}
	
	@inline final def sameViewAs(array :Array[_], start :Int, end :Int) =
		(this.array eq array) && index==start && this.end==end
}



class ReverseArrayIterator[@specialized(Reified) +E](array :Array[E], final protected[this] var index :Int, final protected[this] var end :Int)
	extends ReverseSpecSeqIterator[E]
{
	override final def head: E = array(index)
	
	override final def next() :E = { val res = array(index); index-=1; res }
	
	override final def hasNext = index >= end
	
	override final def toSeq: SpecSeq[E] = new ReversedSeq[E](ArraySlice.subseq[E](array, end, size))
	
	override final def toIndexedSeq = toSeq.toIndexedSeq
}