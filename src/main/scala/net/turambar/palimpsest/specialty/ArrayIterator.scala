package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.FitIterator.{IndexedIterator, ReverseIndexedIterator}
import net.turambar.palimpsest.specialty.seqs.{FitSeq, ReverseSeq, SharedArray}





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



object ArrayIterator {
	def apply[E](array :Array[E]) :FitIterator[E] = Wrap(array)
	
	def apply[E](array :Array[E], start :Int, length :Int) :FitIterator[E] =
		Wrap(array).drop(start).take(length)
	
	def apply[E](from :Int, array :Array[E], until :Int) :FitIterator[E] =
		Wrap(array).take(until).drop(from)
	
	def reverse[E](array :Array[E]) :FitIterator[E] = Reversed(array)
	
	def reverse[E](array :Array[E], start :Int, length :Int) :FitIterator[E] =
		Reversed(array).drop(array.length-start).take(length)
	
	def reverse[E](from :Int, array :Array[E], downto :Int) :FitIterator[E] =
		Reversed(array).drop(array.length-from).take(from-downto)
		
	
	private[this] final val Wrap = new Specialize.With[ArrayIterator, Array] {
		override def specialized[@specialized E : Specialized](param: Array[E]): ArrayIterator[E] =
			new ArrayIterator[E](param, 0, param.length)
	}
	
	private[this] final val Reversed = new Specialize.With[ReverseArrayIterator, Array] {
		override def specialized[@specialized E : Specialized](param: Array[E]): ReverseArrayIterator[E] =
			new ReverseArrayIterator[E](param, param.length-1, 0)
	}
}