package net.turambar.palimpsest.specialty.seqs

import java.lang.Math

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.{ofKnownSize, Elements, FitBuilder, FitCompanion, RuntimeType}
import net.turambar.palimpsest.specialty.iterables.{FitIterableFactory, InterfaceIterableFactory, IterableSpecialization, SpecializableIterable}
import net.turambar.palimpsest.specialty.sets.{MutableSet, ValSet}

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, Subtractable}
import scala.collection.{mutable, GenTraversableOnce}



/** Invariant interface of specialized sequences.
  *
  * @author Marcin Mościcki
  */
trait ValSeqLike[@specialized(Elements) E, +Repr <: ValSeqLike[E, Repr]]
	extends IterableSpecialization[E, Repr] with SeqTemplate[E, Repr] with Subtractable[E, Repr] with mutable.Cloneable[Repr]
{

	/** Runtime type used to store elements of this collection. Overriden to provide non-abstract type parameter
	  * possible due to invariance. This method remains not specialized in order to avoid an additional intermediate
	  * call in the most common case of invoking it from non-specialized code. Use the specialized target of this method
	  * [[net.turambar.palimpsest.specialty.seqs.ValSeqLike#specialization]] when `E` is known or specialized.
	  */
	@inline @unspecialized
	override final def runtimeType :RuntimeType[E] = specialization

	/** Runtime type used to store elements of this collection. The difference from
	  * [[net.turambar.palimpsest.specialty.seqs.ValSeqLike#specialization]] is that this method is specialized, while
	  * the former simply delegates to this instance.
	  */
	override def specialization :RuntimeType[E] = RuntimeType.specialized[E]



	override def -(elem: E): Repr = {
		val i = indexOf(elem)
		if (i<0)
			(newBuilder ++= this).result()
		else {
			val b = newBuilder; b.sizeHint(this, -1)
			val (prefix, suffix) = splitAt(i)
			b ++= prefix
			if (suffix.nonEmpty)
				b ++= suffix.tail
			b.result()
		}
	}

	@inline
	final override def -(elem1: E, elem2: E, elems: E*): Repr = diff(FitSeq.pair(elem1, elem2), elems)

	@inline
	final override def --(xs: GenTraversableOnce[E]): Repr = diff(FitSeq.Empty, xs)

	/** Equals to `this -- elems1 -- elems2`, and is the common delegate implementation for
	  * public subtraction methods. Default implementation slices this collection at
	  * the appropriate indexes and appends them sequentially to a builder for the final result.
	  */
	protected[seqs] def diff(elems1 :FitSeq[E], elems2 :GenTraversableOnce[E]) :Repr = {
		val removedIndices = indicesOf(elems1, elems2)//.size //toList.sorted
		if (removedIndices.isEmpty) repr
		else {
			val b = newBuilder
			if (ofKnownSize(removedIndices))
				b.sizeHint(this, -removedIndices.size)
			var next = removedIndices.toList.sorted //todo: toValList
			var dropped = next.head
			var (prefix, suffix) = splitAt(dropped)
			b ++= prefix
			next = next.tail
			while(next.nonEmpty) {
				dropped += 1
				val prefix_suffix = suffix.tail.splitAt(next.head - dropped)
				b ++= prefix_suffix._1
				suffix = prefix_suffix._2
				next = next.tail
			}
			b ++= suffix.tail
			b.result()
		}
	}


	protected[seqs] def indicesOf(elems1 :FitSeq[E], elems2 :GenTraversableOnce[E]) :ValSet[Int] = {
		var result = MutableSet[Int]() //todo: ValSet
		var searchOffsets = mutable.Map[E, Int]().withDefaultValue(0) //todo: AptMap
		def collect(e :E) :Unit = {
			val i = indexOf(e, searchOffsets(e))
			if (i>=0) {
				result += i
				searchOffsets += e -> (i+1)
			}
		}
		elems1.foreach(collect); elems2.foreach(collect)
		result
	}


	def positionOf(elem :E) :Int = positionOf(elem, 0)

	def lastPositionOf(elem :E) :Int = lastPositionOf(elem, length-1)



	/** Specialized variant of [[SeqTemplate#indexOf]] searching for a value of our actual element type.
	  * Hotspot for subclasses to provide specialized implementation of searching for an element
	  * which actually is of our element type. Used by [[SeqTemplate#indexOf]] if the argument
	  * can be cast to `E`.
	  */
	override def positionOf(elem: E, from: Int) :Int = superIndexOf(elem, from)

	/** Specialized variant of [[SeqTemplate#lastIndexOf]] searching for a value of our actual element type.
	  * Hotspot for subclasses to provide specialized implementation of searching for an element
	  * which actually is of our element type. Used by [[SeqTemplate#indexOf]] if the argument
	  * can be cast to `E`.
	  */
	override def lastPositionOf(elem: E, from: Int) :Int = superLastIndexOf(elem, from)

	/** Analogue of [[net.turambar.palimpsest.specialty.seqs.SeqTemplate#+:]], but builds this collection type
	  * and thanks to invariance enforces the prepended element to be a subtype of this collection's element type.
	  * Default implementation simply builds a new collection from scratch, but subclasses may provide a much more
	  * efficient implementation. In particular, returned collection may share contents with this collection,
	  * even if it is mutable.
	  */
	def %:(elem :E) :Repr = {
		val b = newBuilder
		if (hasFastSize)
			b.sizeHint(length + 1)
		b += elem ++= thisCollection
		b.result()
	}

	/** Analogue of [[net.turambar.palimpsest.specialty.seqs.SeqTemplate#:+]], but builds this collection type
	  * and thanks to invariance enforces the appended element to be a subtype of this collection's element type.
	  * Default implementation simply builds a new collection from scratch, but subclasses may provide a much more
	  * efficient implementation. In particular, returned collection may share contents with this collection,
	  * even if it is mutable.
	  */
	def :%(elem :E) :Repr = {
		val b = newBuilder
		if (hasFastSize)
			b.sizeHint(length + 1)
		b ++= thisCollection += elem
		b.result()
	}


	/** The intent of this method is twofold: if this collection is mutable, than the
	  * returned collection should be an exact but independent copy, and thus any future
	  * changes to any of the instances will not affect the other. If the underlying collection
	  * is immutable, then it can just return itself (and thus it is possible that `x.clone() eq x`).
	  * However, if this instance is a slice of a larger collection, sharing representation with other
	  * instances and possibly preventing garbage collection of a larger structure,
	  * then it generally should create a new instance with 'minimised' representation.
	  * Default implementation uses the builder associated with this collection to create a new instance.
	  */
	override def clone() :Repr = (newBuilder ++= this).result()


	@unspecialized
	def copyToFitArray(xs :Array[E], start :Int=0, count :Int = Int.MaxValue) :Int =
		if (start<0)
			throw new IllegalArgumentException(s"$stringPrefix<$length>.copyToFitArray([], $start, $count")
		else {
			val max = Math.min(count, xs.length-start)
			if (max > 0)
				trustedCopyTo(xs, start, max)
			else 0
		}
}



trait ValSeq[@specialized(Elements) E] extends FitSeq[E] with ValSeqLike[E, ValSeq[E]] with SpecializableIterable[E, ValSeq] {
	override def companion :FitCompanion[ValSeq] = ValSeq
}


object ValSeq extends InterfaceIterableFactory[ValSeq] {
	override protected[this] type RealType[@specialized(Elements) E] = SharedArray[E]
	override protected[this] def default: FitIterableFactory[SharedArray] = SharedArray

	override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ValSeq[_], E, ValSeq[E]]): CanBuildFrom[ValSeq[_], E, ValSeq[E]] =
		fit.cbf
}