package net.turambar.palimpsest.specialty.ordered

import net.turambar.palimpsest.specialty.FitIterable.{IterableAdapter, IterableFoundation}
import net.turambar.palimpsest.specialty.seqs.FitSeq
import net.turambar.palimpsest.specialty.sets.SortedFitSet
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterable, FitIterator, IterableSpecialization}

import scala.collection.generic.Sorted
import Ordering.Implicits.infixOrderingOps
import scala.collection.SortedSet


//todo: exclude Boolean specialization
trait OrderedAs[@specialized(Elements) E, +This<:OrderedAs[E, This]] extends Sorted[E, This] with IterableSpecialization[E, This] {
	implicit def ordering :Ordering[E]
	override def compare(e1 :E, e2 :E) :Int = ordering.compare(e1, e2)

	def nth(idx :Int) :E
	override def firstKey = head
	override def lastKey = last

	def +(elem :E) :This
	def +(elem1 :E, elem2 :E, elems :E*) :This = this + elem1 + elem2 ++ elems

	def ++(elems :TraversableOnce[E]) :This = {
		var res = repr
		elems foreach { e :E => res = res + e }
		res
	}

	def -(elem :E) :This
	def -(elem1 :E, elem2 :E, elems :E*) :This = this - elem1 - elem2 -- elems

	def --(elems :TraversableOnce[E]) :This = {
		var res = repr
		elems foreach { e :E => res = res - e }
		res
	}

	def empty :This

	def reverseIterator :FitIterator[E]
	def iteratorFrom(start :E) :FitIterator[E] = keysIteratorFrom(start)
	def keysIteratorFrom(start :E) :FitIterator[E]
//	def newBuilder :FitBuilder[E, This]
//	def reverse :This
}

/**
  * @author Marcin Mościcki
  */
trait OrderedVals[@specialized(Elements) E] extends FitIterable[E] with OrderedAs[E, OrderedVals[E]] {
	override protected[this] def newBuilder :FitBuilder[E, OrderedVals[E]] = OrderedSeq.newBuilder
}


class OrderedSeq[@specialized(Elements) E] private[ordered](override protected[this] val source :FitSeq[E])(implicit override val ordering :Ordering[E])
	extends IterableAdapter[FitSeq[E], E, OrderedSeq[E]] with OrderedVals[E] with OrderedAs[E, OrderedSeq[E]]
{
	override protected[this] def fromSource(other: FitSeq[E]): OrderedSeq[E] =
		new OrderedSeq[E](other)

	override def size = source.size
	override def hasFastSize = source.hasFastSize
	override def hasDefiniteSize = source.hasDefiniteSize
	override def isEmpty = source.isEmpty
	override def nonEmpty = source.nonEmpty

	override def nth(idx: Int): E = source(idx)
	override def head = source.head
	override def last = source.last


	override def keySet: SortedFitSet[E] = SortedFitSet(source :_*)
	override def keysIteratorFrom(start: E): FitIterator[E] = iterator.dropWhile{ e :E => !(e equiv start) }

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
	override def ++(elems: TraversableOnce[E]) = new OrderedSeq((elems ++: source).sorted)

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

	override def --(elems: TraversableOnce[E]) :OrderedSeq[E] = {
		val b = source.toFitBuffer
		b --= elems
		new OrderedSeq(b)
	}


	def reverse :OrderedSeq[E] = {
		val rev = ordering.reverse
		new OrderedSeq(source.sorted(rev))(rev)
	}
}


object OrderedSeq {
	def empty[@specialized(Elements) E :Ordering] :OrderedSeq[E] = new OrderedSeq(FitSeq.empty[E])

	def apply[@specialized(Elements) E :Ordering](elems :E*) :OrderedSeq[E] =
		new OrderedSeq(FitSeq(elems:_*).sorted)

	def newBuilder[@specialized(Elements) E :Ordering] :FitBuilder[E, OrderedSeq[E]] =
		FitSeq.newBuilder[E].mapResult(elems => new OrderedSeq(elems.sorted))
}
