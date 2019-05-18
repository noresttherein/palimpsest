package net.turambar.palimpsest.specialty.iterators

import net.turambar.palimpsest.specialty.{Elements, RuntimeType, Specialize}
import net.turambar.palimpsest.specialty.seqs.{FitSeq, ReverseSeq, SharedArray}
import net.turambar.palimpsest.specialty.seqs.ArrayView.UnknownArrayView





/** All purpose iterator going up an array element by element.
  * @param array the array with elements to be iterated.
  * @param from start index for the iteration, pointing to the head element of the new iterator.
  * @param until index of the element ending the iteration; initial size equals `until-from`.
  * @author Marcin Mościcki
  */
class ArrayIterator[@specialized(Elements) +E](array :Array[E], from :Int, until :Int)
	extends IndexedIterator[E](from, until) with FitIterator[E]
{


	@inline final override def head: E = array(index)
	
	override def next(): E = { val res = array(index); index += 1; res }
	
	override def foreach[@specialized(Unit) U](f: E => U): Unit =
		while(index<end) { f(array(index)); index+=1 }

	
	@inline final override def copyToArray[B >: E](xs: Array[B], start: Int, len :Int): Unit =
		Array.copy(array, index, xs, start, len min (xs.length-start) min size)
	
	@inline final override def toIndexedSeq = toSeq.toIndexedSeq

	@inline final override def toSeq :FitSeq[E] = new UnknownArrayView(array, index, end-index)
	
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





class MutableArrayIterator[@specialized(Elements) E](array :Array[E], from :Int, until :Int)
	extends IndexedIterator[E](from, until) with MutableIterator[E]
{
	override def head :E = array(index)

	override def head_=(elem :E) :Unit =
		if (index < end) array(index) = elem
		else throw new NoSuchElementException("update on an empty iterator")

	override def next() :E = { val res = array(index); index += 1; res }

	override def next_=(elem :E) :Unit = {
		val i = index   //read once so index doesn't concurrently move past the end. end is never increased
		if (i < end) {  //and decreased only in take, which doesn't violate the permissions of this iterator as created
			array(i) = elem; index = i + 1
		} else
			throw new NoSuchElementException("update on an empty iterator")
	}
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
		override def specialized[@specialized E : RuntimeType](param: Array[E]): ArrayIterator[E] =
			new ArrayIterator[E](param, 0, param.length)
	}
	
	private[this] final val Reversed = new Specialize.With[ReverseArrayIterator, Array] {
		override def specialized[@specialized E : RuntimeType](param: Array[E]): ReverseArrayIterator[E] =
			new ReverseArrayIterator[E](param, param.length-1, 0)
	}
}