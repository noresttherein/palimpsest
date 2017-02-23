package net.turambar.palimpsest.specialty.seqs

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitCompanion, FitIterableFactory, InterfaceIterableFactory, IterableSpecialization, SpecializableIterable, Specialized, ofKnownSize}

import scala.collection.generic.{CanBuildFrom, Subtractable}
import scala.collection.{GenTraversableOnce, mutable}



/** Invariant interface of specialized sequences.
  *
  * @author Marcin Mościcki
  */
trait ValSeqLike[@specialized(Elements) E, +Repr <: ValSeqLike[E, Repr]]
	extends IterableSpecialization[E, Repr] with SeqTemplate[E, Repr] with Subtractable[E, Repr] with mutable.Cloneable[Repr]
{

	@inline override final def specialization :Specialized[E] = Specialized[E]

	//this could use specialization, but it isn't necessary
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

	/** Equivalent to `this -- elems1 -- elems2`, and is the common delegate implementation for
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
			var next = removedIndices.toList.sorted
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


	protected[seqs] def indicesOf(elems1 :FitSeq[E], elems2 :GenTraversableOnce[E]) :Iterable[Int] = {
		var result = mutable.Set[Int]()
		var searchOffsets = mutable.Map[E, Int]().withDefaultValue(0)
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
	override def positionOf(elem: E, from: Int) = superIndexOf(elem, from)

	/** Specialized variant of [[SeqTemplate#lastIndexOf]] searching for a value of our actual element type.
	  * Hotspot for subclasses to provide specialized implementation of searching for an element
	  * which actually is of our element type. Used by [[SeqTemplate#indexOf]] if the argument
	  * can be cast to `E`.
	  */
	override def lastPositionOf(elem: E, from: Int) = superLastIndexOf(elem, from)

	/** Analogue of [[net.turambar.palimpsest.specialty.seqs.SeqTemplate#+:]], but builds this collection type
	  * and thanks to invariance enforces the prepended element to be a subtype of this collection's element type.
	  * Default implementation simply builds a new collection from scratch, but subclasses may provide a much more
	  * efficient implementation. In particular, returned collection may share contents with this collection,
	  * even if it is mutable.
	  */
	def %:[That](elem :E) :Repr = {
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
	def :%[That](elem :E) :Repr = {
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