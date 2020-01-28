package net.turambar.palimpsest.specialty.ordered

import net.turambar.palimpsest.specialty.iterables.{CloneableIterable, FitIterable, IterableSpecialization}
import net.turambar.palimpsest.specialty.{?, ItemTypes, FitBuilder}
import net.turambar.palimpsest.specialty.iterators.FitIterator

import scala.collection.GenTraversableOnce


/** A template for collections ordered directly by its elements. The difference from [[OrderedBy]]
  * is that the keys here are actually whole elements of the collection. This allows it to declare
  * methods for modifying the collection by adding or removing keys.
  * @tparam K element type of `This`.
  * @tparam This produced collection type (not enforced here)
  */
trait OrderedAs[@specialized(ItemTypes) K, +This <: OrderedAs[K, This]]
	extends IterableSpecialization[K, This] with OrderedBy[This, K] with CloneableIterable[K, This]
{


	def +(elem :K) :This
	def +(elem1 :K, elem2 :K, elems :K*) :This// = this + elem1 + elem2 ++ elems

	def ++(elems :GenTraversableOnce[K]) :This //= {
//		var res = repr
//		elems foreach { e :E => res = res + e }
//		res
//	}

	def -(elem :K) :This
	def -(elem1 :K, elem2 :K, elems :K*) :This //= this - elem1 - elem2 -- elems

	def --(elems :GenTraversableOnce[K]) :This // = {
//		var res = repr
//		elems foreach { e :E => res = res - e }
//		res
//	}


	def iteratorFrom(start :K) :FitIterator[K] = keysIteratorFrom(start)

	override def keysIterator :FitIterator[K] = iterator
//	override protected[this] def newBuilder :FitBuilder[K, This] =
//		FitBuffer.newBuilder.mapResult { buffer :FitBuffer[K] => empty ++ buffer }
}



/*
object OrderedAs {
	@deprecated
	trait EmptyOrderedTemplate[E, +Vals<:OrderedVals[E] with OrderedAs[E, Vals]] extends OrderedAs[E, Vals] with EmptyIterableTemplate[E, Vals] { self :Vals =>
//		type This = Vals
//		override def empty: This = this

		override def keyAt(n: Int): E = throw new IndexOutOfBoundsException(s"empty $stringPrefix: rank($n)")

		override def reverseIterator: FitIterator[E] = FitIterator.empty
		override def keysIteratorFrom(start: E): FitIterator[E] = FitIterator.empty

		//todo: specialize these
		override def from(from: E) :Vals = this
		override def until(until: E) :Vals = this
		override def range(from: E, until: E) :Vals = this
		override def to(to: E) :Vals = this
		override def rangeImpl(from: Option[E], until: Option[E]): Vals = this
		override def rangeImpl(from: ?[E], until: ?[E]) :Vals = this
	}
}
*/



/** Base trait for collections which elements follow a specific ordering.
  * @author Marcin Mościcki
  */
trait OrderedVals[@specialized(ItemTypes) E]
	extends FitIterable[E] with OrderedAs[E, OrderedVals[E]]
{
//	type This <: OrderedVals[E]
//	type This = Self

	override def firstKey: E = head
	override def lastKey: E = last


	def reverseIterator :FitIterator[E]
//	override def reverseKeyIterator :FitIterator[E] = reverseIterator
//	def iteratorFrom(start :E) :FitIterator[E]
//	def +(elem :E) :Self
//	def +(elem1 :E, elem2 :E, elems :E*) :Self// = this + elem1 + elem2 ++ elems
//
//	def ++(elems :GenTraversableOnce[E]) :Self
	override protected[this] def newBuilder :FitBuilder[E, OrderedVals[E]] = OrderedSeq.newBuilder

	override def stringPrefix :String = typeStringPrefix + "[" + specialization.classTag + "]"
}

