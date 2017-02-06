package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.FitIterator.{IndexedIterator, ReverseIndexedIterator}





/** All purpose iterator going up an array element by element.
  * @author Marcin Mościcki
  */
class ArrayIterator[@specialized(Elements) +E](array :Array[E], from :Int, until :Int)
	extends IndexedIterator[E](from, until) with FitIterator[E]
{
	@inline final override def head: E = array(index)
	
	override def next(): E = { val res = array(index); index += 1; res }
	
	override def foreach[@specialized(Unit) U](f: (E) => U): Unit =
		while(index<end) { f(array(index)); index+=1 }

	
	@inline final override def copyToArray[B >: E](xs: Array[B], start: Int, len :Int): Unit =
		Array.copy(array, index, xs, start, len min (xs.length-start) min size)
	
	@inline final override def toIndexedSeq = toSeq.toIndexedSeq
	
	@inline final override def toSeq :FitSeq[E] = SharedArray.view[E](array, index, size)
	
	override def sameElements(that: Iterator[_]): Boolean = that match {
		case i :ArrayIterator[_] if i.sameViewAs(array, index, end) => true
		case _ => super.sameElements(that)
	}
	
	@inline final def sameViewAs(array :Array[_], start :Int, end :Int) =
		(this.array eq array) && index==start && this.end==end
}




/** Iterator going down an array in the direction of decreasing indices. */
class ReverseArrayIterator[@specialized(Elements) +E](array :Array[E], from :Int, downto :Int)
	extends ReverseIndexedIterator[E](from, downto) with FitIterator[E]
{
	override def head: E = array(index)
	
	override def next() :E = { val res :E = array(index); index-=1; res }
	
	override def foreach[@specialized(Unit) U](f: (E) => U): Unit =
		while(index>=end) { f(array(index)); index-=1 }
	
	override final def toSeq: FitSeq[E] = new ReverseSeq[E](SharedArray.view[E](array, end, size))
	
	override final def toIndexedSeq = toSeq.toIndexedSeq
}