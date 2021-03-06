package net.noresttherein.palimpsest.seqs

import java.lang.Math

import net.noresttherein.palimpsest.iterables.AptCompanion.CanFitFrom
import net.noresttherein.palimpsest.{ofKnownSize, ItemTypes, AptBuilder, RuntimeType}
import net.noresttherein.palimpsest.iterables.{CloneableIterable, AptCompanion, AptIterableFactory, InterfaceIterableFactory, IterableSpecialization, SpecializableIterable}
import net.noresttherein.palimpsest.sets.{MutableSet, ValSet}

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, Subtractable}
import scala.collection.{mutable, GenTraversableOnce}



/** Invariant interface of specialized sequences.
  *
  * @author Marcin Mościcki
  */
trait ValSeqLike[@specialized(ItemTypes) E, +Repr <: ValSeqLike[E, Repr]]
	extends IterableSpecialization[E, Repr] with SeqTemplate[E, Repr] with Subtractable[E, Repr] //with CloneableIterable[E, Repr]
{

	/** Runtime type used to store elements of this collection. Overriden to provide non-abstract type parameter
	  * possible due to invariance. This method remains not specialized in order to avoid an additional intermediate
	  * call in the most common case of invoking it from non-specialized code. Use the specialized target of this method
	  * [[net.noresttherein.palimpsest.seqs.ValSeqLike#specialization]] when `E` is known or specialized.
	  */
	@inline @unspecialized
	override final def runtimeType :RuntimeType[E] = specialization

	/** Runtime type used to store elements of this collection. The difference from
	  * [[net.noresttherein.palimpsest.seqs.ValSeqLike#specialization]] is that this method is specialized, while
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
	final override def -(elem1: E, elem2: E, elems: E*): Repr = diff(AptSeq.two(elem1, elem2), elems)

	@inline
	final override def --(xs: GenTraversableOnce[E]): Repr = diff(AptSeq.Empty, xs)

	/** Equals to `this -- elems1 -- elems2`, and is the common delegate implementation for
	  * public subtraction methods. Default implementation slices this collection at
	  * the appropriate indexes and appends them sequentially to a builder for the final result.
	  */
	protected[seqs] def diff(elems1 :AptSeq[E], elems2 :GenTraversableOnce[E]) :Repr = {
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


	protected[seqs] def indicesOf(elems1 :AptSeq[E], elems2 :GenTraversableOnce[E]) :ValSet[Int] = {
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


	def offsetOf(elem :E) :Int = offsetOf(elem, 0)

	def lastOffsetOf(elem :E) :Int = lastOffsetOf(elem, length-1)



	/** Specialized variant of [[SeqTemplate#indexOf]] searching for a value of our actual element type.
	  * Hotspot for subclasses to provide specialized implementation of searching for an element
	  * which actually is of our element type. Used by [[SeqTemplate#indexOf]] if the argument
	  * can be cast to `E`.
	  */
	override def offsetOf(elem: E, from: Int) :Int = genericIndexOf(elem, from)

	/** Specialized variant of [[SeqTemplate#lastIndexOf]] searching for a value of our actual element type.
	  * Hotspot for subclasses to provide specialized implementation of searching for an element
	  * which actually is of our element type. Used by [[SeqTemplate#indexOf]] if the argument
	  * can be cast to `E`.
	  */
	override def lastOffsetOf(elem: E, from: Int) :Int = genericLastIndexOf(elem, from)

	/** Analogue of [[net.noresttherein.palimpsest.seqs.SeqTemplate#+:]], but builds this collection type
	  * and thanks to invariance enforces the prepended element to be a subtype of this collection's element type,
	  * allowing specialization of this method. Default implementation simply builds a new collection from scratch,
	  * but subclasses may provide a much more efficient implementation. In particular, returned collection may share
	  * contents with this collection, even if it is mutable.
	  */
	def #:(elem :E) :Repr = {
		val b = newBuilder
		if (hasFastSize)
			b.sizeHint(length + 1)
		b += elem ++= thisCollection
		b.result()
	}

	/** Analogue of [[net.noresttherein.palimpsest.seqs.SeqTemplate#:+]], but builds this collection type
	  * and thanks to invariance enforces the appended element to be a subtype of this collection's element type.
	  * Default implementation simply builds a new collection from scratch, but subclasses may provide a much more
	  * efficient implementation. In particular, returned collection may share contents with this collection,
	  * even if it is mutable.
	  */
	def :#(elem :E) :Repr = {
		val b = newBuilder
		if (hasFastSize)
			b.sizeHint(length + 1)
		b ++= thisCollection += elem
		b.result()
	}


//	/** The intent of this method is twofold: if this collection is mutable, than the
//	  * returned collection should be an exact but independent copy, and thus any future
//	  * changes to any of the instances will not affect the other. If the underlying collection
//	  * is immutable, then it can just return itself (and thus it is possible that `x.clone() eq x`).
//	  * However, if this instance is a slice of a larger collection, sharing representation with other
//	  * instances and possibly preventing garbage collection of a larger structure,
//	  * then it generally should create a new instance with 'minimised' representation.
//	  * Default implementation uses the builder associated with this collection to create a new instance;
//	  * The builder should take advantage of known size of this sequence and reserve no more space than is needed.
//	  */
//	override def clone() :Repr = (newBuilder ++= this).result()


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



trait ValSeq[@specialized(ItemTypes) E]
	extends AptSeq[E] with ValSeqLike[E, ValSeq[E]] with SpecializableIterable[E, ValSeq] with CloneableIterable[E, ValSeq[E]]
{
	override def companion :AptCompanion[ValSeq] = ValSeq
}



object ValSeq extends InterfaceIterableFactory[ValSeq] {
	override protected[this] type RealType[@specialized(ItemTypes) E] = SharedArray[E]
	override protected[this] def default: AptIterableFactory[SharedArray] = SharedArray

	override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ValSeq[_], E, ValSeq[E]]): CanBuildFrom[ValSeq[_], E, ValSeq[E]] =
		fit.cbf
}