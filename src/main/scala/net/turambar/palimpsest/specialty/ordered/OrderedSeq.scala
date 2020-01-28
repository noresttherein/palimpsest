package net.turambar.palimpsest.specialty.ordered

import net.turambar.palimpsest.specialty.iterables.{CloneableIterable, IterableAdapter, IterableSpecialization}
import net.turambar.palimpsest.specialty.{?, Blank, ItemTypes, FitBuilder, Sure}
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.seqs.{FitSeq, SliceLike}
import net.turambar.palimpsest.specialty.sets.OrderedSet

import scala.collection.GenTraversableOnce



//todo: make this a skeletal multi set implementation
/** Ordered values stored in a sequence. Provides efficient `O(log n)` search, fast iteration, but adding or removing elements
  * is pessimistically (and realistically) `O(n)`.
  */
class OrderedSeq[@specialized(ItemTypes) E] private[ordered](override protected[this] val source :FitSeq[E])(implicit override val ordering :ValOrdering[E])
	extends IterableAdapter[FitSeq[E], E, OrderedSeq[E]] with OrderedVals[E] with OrderedAs[E, OrderedSeq[E]]
	   with IterableSpecialization[E, OrderedSeq[E]]
{
	//	type This = OrderedSeq[E]

//	override def companion :FitCompanion[OrderedSeq] = OrderedSeq

	override protected[this] def fromSource(other: FitSeq[E]): OrderedSeq[E] =
		new OrderedSeq[E](other)

	override def keyAt(idx: Int): E = source(idx)
	override def head :E = source.head
	override def last :E = source.last


	override def keySet: OrderedSet[E] = OrderedSet(source :_*)

	override def contains(elem :E) :Boolean = indexOf(elem) >= 0


	/** Finds the index of the given element in this sequence.
	  * The element is compared with standard  `==/equals` before returning. This assumes that
	  * standard equality implies equivalency in terms of ordering of this instance.
	  * @param elem element to find in this sequence
	  * @return An [[net.turambar.palimpsest.specialty.Unsure]] value containing the index of the first occurence of `elem`.
	  */
	def index_?(elem :E) : ?[Int] = {
		val i = indexBefore(elem)
		if (i < size && source(i)==elem) Sure(i)
		else Blank
	}


	/** Finds the index of the element in this sequence, if present.
	  * The element is compared with standard `==/equals` before returning. This assumes that
	  * standard equality implies equivalency in terms of ordering of this instance.
	  * @param elem element to find in this sequence
	  * @return index of the first element equal to `elem` or `-1` if no such element can be found.
	  */
	def indexOf(elem :E) :Int = {
		val i = indexBefore(elem)
		if (i < size && source(i)==elem) i
		else -1
	}

	/** Returns the index of the first element `x &gt;= elem`, or `size` if all elements are smaller than the argument. */
	def indexBefore(elem :E) :Int = {
		var start = 0; var end = size
		while (start != end) {
			val middle = (start+end) / 2
			if (lt(source(middle), elem))
				start = middle+1
			else
				end = middle
		}
		start
	}

	/** Returns the index of the first element `x &gt; elem` or `size` if all elements are smaller than the argument. */
	def indexAfter(elem :E) :Int = {
		var start = 0; var end = size
		while (start != end) {
			val middle = (start + end) / 2
			if (lte(source(middle), elem))
				start = middle+1
			else
				end = middle
		}
		start
	}


	override def keysIteratorFrom(start: E): FitIterator[E] =
		if (source.isInstanceOf[SliceLike[E, _]])
			source.drop(indexBefore(start)).iterator
		else
	        iterator.drop(indexBefore(start))
//	override def iteratorFrom(start: E): FitIterator[E] = keysIteratorFrom(start)

	override def rangeImpl(from: ?[E], until: ?[E]): OrderedSeq[E] = (from, until) match {
		case (start :Sure[E], end :Sure[E]) =>
			val from = indexBefore(start.value); val until = indexBefore(end.value)
			new OrderedSeq(source.slice(from, until))
		case (start :Sure[E], _) =>
			val from = indexBefore(start.value)
			new OrderedSeq(source.drop(from))
		case (_, end :Sure[E]) =>
			val until = indexBefore(end.value)
			new OrderedSeq(source.take(until))
		case _ => this
	}


	override def partition(p: E => Boolean) :(OrderedSeq[E], OrderedSeq[E]) = {
		val div = source.partition(p)
		(new OrderedSeq(div._1.sorted), new OrderedSeq(div._2.sorted))
	}

	override def reverseIterator: FitIterator[E] = source.reverseIterator

	override def empty: OrderedSeq[E] = new OrderedSeq[E](FitSeq.Empty)

	override protected[this] def newBuilder :FitBuilder[E, OrderedSeq[E]] =
		FitSeq.newBuilder.mapResult(new OrderedSeq(_))


	override def +(elem: E): OrderedSeq[E] = new OrderedSeq((elem +: source).sorted)
	override def +(elem1: E, elem2: E, rest: E*) = new OrderedSeq((rest ++: elem1 +: elem2 +: source).sorted)
	override def ++(elems: GenTraversableOnce[E]) = new OrderedSeq((elems.seq ++: source).sorted)

	override def -(elem: E): OrderedSeq[E] = {
		val b = source.genericBuilder[E] //genericBuilder[E]
		var found = false
		source traverse { e :E => if (!found && equiv(e, elem)) found = true else b += e }
		new OrderedSeq(b.result())(ordering)
	}

	override def -(elem1: E, elem2: E, elems: E*) :OrderedSeq[E] = {
		val b = source.toFitBuffer
		b -= (elem1, elem2, elems:_*)
		new OrderedSeq(b)
	}

	override def --(elems: GenTraversableOnce[E]) :OrderedSeq[E] = {
		val b = source.toFitBuffer
		b --= elems.seq
		new OrderedSeq(b)
	}


	def reverse :OrderedSeq[E] = {
		val rev = ordering.reverse
		new OrderedSeq(source.sorted(rev))(rev)
	}

	@inline final override def toSeq :FitSeq[E] = source
	@inline final override def toFitSeq :FitSeq[E] = source
}


object OrderedSeq {

	def empty[@specialized(ItemTypes) E :Ordering] :OrderedSeq[E] = new OrderedSeq(FitSeq.empty[E])

	def apply[@specialized(ItemTypes) E :Ordering](elems :E*) :OrderedSeq[E] =
		new OrderedSeq(FitSeq(elems:_*).sorted)

	def newBuilder[@specialized(ItemTypes) E :Ordering] :FitBuilder[E, OrderedSeq[E]] =
		FitSeq.newBuilder[E].mapResult(elems => new OrderedSeq(elems.sorted))
}
