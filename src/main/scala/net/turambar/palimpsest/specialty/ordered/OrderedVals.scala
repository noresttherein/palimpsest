package net.turambar.palimpsest.specialty.ordered

import net.turambar.palimpsest.specialty.FitIterable.IterableAdapter
import net.turambar.palimpsest.specialty.iterables.EmptyIterableTemplate
import net.turambar.palimpsest.specialty.seqs.FitSeq
import net.turambar.palimpsest.specialty.sets.OrderedSet
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterable, FitIterator, IterableSpecialization}

import scala.collection.generic.Sorted
import Ordering.Implicits.infixOrderingOps
import scala.collection.{GenTraversableOnce, SortedSet}


//todo: exclude Boolean specialization
trait OrderedAs[@specialized(Elements) K, +This<:OrderedAs[K, This]] extends Sorted[K, This]  {
	implicit def ordering :Ordering[K]
	override def compare(e1 :K, e2 :K) :Int = ordering.compare(e1, e2)

	/** N-th key in this collection. */
	def keyAt(n :Int) :K
//	override def firstKey = head
//	override def lastKey = last

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

	def empty :This

	def reverseKeyIterator :FitIterator[K]
	def keysIteratorFrom(start :K) :FitIterator[K]

	override def from(from: K) :This = rangeImpl(Some(from), None)

	override def until(until: K) :This = rangeImpl(None, Some(until))

	override def range(from: K, until: K) :This = rangeImpl(Some(from), Some(until))

	override def to(to: K) :This = { //takeWhile { e :E => compare(e, to) <= 0 }
		val from = keysIteratorFrom(to)
		var next = to
		while (from.hasNext && { next = from.next(); compare(next, to)==0 })
			()
		if (!from.hasNext) repr
		else until(next)
	}
}

object OrderedAs {
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
	}
}

/**
  * @author Marcin Mościcki
  */
trait OrderedVals[@specialized(Elements) E] extends OrderedAs[E, OrderedVals[E]] with FitIterable[E] with IterableSpecialization[E, OrderedVals[E]] {
//	type This <: OrderedVals[E]
//	type This = Self

	override def firstKey: E = head
	override def lastKey: E = last

	def reverseIterator :FitIterator[E]
	override def reverseKeyIterator = reverseIterator
	def iteratorFrom(start :E) :FitIterator[E]

	def +(elem :E) :Self
	def +(elem1 :E, elem2 :E, elems :E*) :Self// = this + elem1 + elem2 ++ elems

	def ++(elems :GenTraversableOnce[E]) :Self

	override protected[this] def newBuilder :FitBuilder[E, OrderedVals[E]] = OrderedSeq.newBuilder
}

//todo: make this multiset
class OrderedSeq[@specialized(Elements) E] private[ordered](override protected[this] val source :FitSeq[E])(implicit override val ordering :Ordering[E])
	extends IterableAdapter[FitSeq[E], E, OrderedSeq[E]] with OrderedVals[E] with OrderedAs[E, OrderedSeq[E]] with IterableSpecialization[E, OrderedSeq[E]]
{
//	type This = OrderedSeq[E]

	override protected[this] def fromSource(other: FitSeq[E]): OrderedSeq[E] =
		new OrderedSeq[E](other)

	override def size = source.size
	override def hasFastSize = source.hasFastSize
	override def hasDefiniteSize = source.hasDefiniteSize
	override def isEmpty = source.isEmpty
	override def nonEmpty = source.nonEmpty
	override def ofAtLeast(items :Int) = source.ofAtLeast(items)

	override def keyAt(idx: Int): E = source(idx)
	override def head = source.head
	override def last = source.last


	override def keySet: OrderedSet[E] = OrderedSet(source :_*)
	override def keysIteratorFrom(start: E): FitIterator[E] = iterator.dropWhile{ e :E => !(e equiv start) }
	override def iteratorFrom(start: E): FitIterator[E] = keysIteratorFrom(start)

	override def rangeImpl(from: Option[E], until: Option[E]): OrderedSeq[E] = (from, until) match {
		case (Some(start), Some(end)) => new OrderedSeq(source.dropWhile(_ < start).takeWhile(_ < end))
		case (Some(start), _) => new OrderedSeq(source.dropWhile(_ < start))
		case (_, Some(end)) => new OrderedSeq(source.takeWhile(_ < end))
		case _ => this
	}


	override def partition(p: (E) => Boolean) = {
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
		val b = source.specializedBuilder[E]
		var found = false
		source traverse { e :E => if (!found && (e equiv elem)) found = true else b += e }
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

	@inline final override def toSeq = source
	@inline final override def toFitSeq = source
}


object OrderedSeq {

	def empty[@specialized(Elements) E :Ordering] :OrderedSeq[E] = new OrderedSeq(FitSeq.empty[E])

	def apply[@specialized(Elements) E :Ordering](elems :E*) :OrderedSeq[E] =
		new OrderedSeq(FitSeq(elems:_*).sorted)

	def newBuilder[@specialized(Elements) E :Ordering] :FitBuilder[E, OrderedSeq[E]] =
		FitSeq.newBuilder[E].mapResult(elems => new OrderedSeq(elems.sorted))
}
