package net.turambar.palimpsest.specialty.seqs

import java.lang.Math

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.RuntimeType.Fun1Vals
import net.turambar.palimpsest.specialty.{ofKnownSize, FitBuilder, FitIterator}
import net.turambar.palimpsest.specialty.iterables.IterableTemplate

import scala.collection.{GenSeq, IndexedSeqLike, SeqLike}
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq

/**
  * @author Marcin Mościcki
  */
trait SeqTemplate[+E, +Repr] extends SeqLike[E, Repr] with IterableTemplate[E, Repr] {

	
//	/** Fixed to equal [[SeqTemplate#length]]. */
//	@inline
//	final override def size: Int = length

	def length :Int

	/** Create a slice of this instance assuming the indices are already validated. Delegated to from [[slice]] and other subsequence methods. */
	protected def section(from: Int, until: Int): Repr

	/** Access to protected `section` method of sibling collections. */
	@inline
	final protected[this] def sectionOf(seq: SeqTemplate[_, Repr], from: Int, until: Int): Repr = seq.section(from, until)


	
	//todo: make this public again;
	/** Delegates to [[SeqTemplate#reverseIterator]]'s `foreach` method. */
	override protected def reverseForeach(f: E => Unit): Unit = reverseIterator.foreach(f)

	/** Fixed to use [[SeqTemplate#indexOf(U)]]. */
	override def contains[U >: E](elem: U): Boolean = indexOf(elem) >= 0

	/** Delegate to [[SliceLike#prefixLength]]. */
	override def forall(p: E => Boolean): Boolean = indexWhere(p, false, 0) < 0

	/** Delegate to [[SeqTemplate#indexWhere]]. */
	override def exists(p: E => Boolean): Boolean = indexWhere(p, 0) >= 0

	/** Delegates to [[SeqTemplate#segmentLength]]. */
	override def prefixLength(p: E => Boolean): Int = segmentLength(p, 0)



	/** Basis for implementation of [[segmentLength]] and [[indexWhere]], which means that also all other
	  * method searching for an index based on a predicate. Default implementation delegates
	  * to the corresponding method of the iterator: [[net.turambar.palimpsest.specialty.FitIterator#indexWhere]].
	  * It's a good idea to overridde either this, or both `segmentLength` and `indexWhere`.
	  */
	protected[this] def indexWhere(p: E => Boolean, ourTruth: Boolean, from: Int): Int = {
		val start = Math.max(from, 0)
		val i = iterator.drop(start).indexWhere(p, ourTruth)
		if (i < 0) -1
		else start + i
	}



	/** By default delegates simply to `indexWhere(p, where=true, from)`. Most seq implementations
	  * should either override that method, or this one together with `lastIndexWhere`.
	  */
	override def indexWhere(p: E => Boolean, from: Int): Int = indexWhere(p, true, from)


	/** Fixed to delegate to [[SeqTemplate#indexWhere(E=>Boolean, Int)]]. */
	@inline
	final override def indexWhere(p: E => Boolean): Int = indexWhere(p, 0)

	
	/** Implemented using [[reverseIterator]], asks for being overridden in subclasses. */
	override def lastIndexWhere(p: E => Boolean, from: Int): Int =
		if (from < 0) -1
		else {
			val len = length
			val start = Math.min(from, len - 1)
			val it = reverseIterator.drop(len - 1 - start)
			val i = it.indexWhere(p)
			if (i < 0) -1
			else Math.min(from, len - 1) - i
		}

	/** Fixed to delegate to [[SeqTemplate#lastIndexWhere(E=>Boolean, Int)]]. */
	@inline
	final override def lastIndexWhere(p: E => Boolean): Int = lastIndexWhere(p, length)
	
	
	

	/** Fixed to delegate to [[SeqTemplate#indexOf(U, Int)]]. */
	@inline
	final override def indexOf[U >: E](elem: U): Int = indexOf(elem, 0)


	/** Checks if the given argument is compatible with the specialization of this collection and,
	  * if so, forwards the call to specialized [[SeqTemplate#fitIndexOf]] after casting,
	  * otherwise using [[SeqTemplate#superIndexOf]], which forwards the call to the iterator.
	  */
	override def indexOf[U >: E](elem: U, from: Int): Int =
		if (specialization.boxType isAssignableFrom elem.getClass)
			positionOf(elem.asInstanceOf[E], from)
		else
			superIndexOf(elem, from)

	/** Search for the element by delegating to this instance's iterator.
	  * Used particularly when `elem` is not an instance of `E` (or transparently convertible to such),
	  * so we can afford to not tune this to any extent as the result is most likely `-1`
	  * (and possibly a bug) anyway.
	  */
	protected[this] def superIndexOf[U >: E](elem: U, from: Int): Int = {
		val it = iterator.drop(from)
		val i = iterator.indexOf(elem)
		if (i < 0) -1 else Math.max(from, 0) + i
	}

	/** Specialized variant of [[SeqTemplate#indexOf]] searching for a value of our actual element type.
	  * Hotspot for subclasses to provide specialized implementation of searching for an element
	  * which actually is of our element type. Used by [[SeqTemplate#indexOf]] if the argument
	  * can be cast to `E`.
	  */
	protected[this] def positionOf(elem :E, from :Int) :Int = superIndexOf(elem, from)

	/** Fixed to delegate to [[SeqTemplate#lastIndexOf(U, Int)]]. */
	@inline
	final override def lastIndexOf[U >: E](elem: U): Int = lastIndexOf(elem, length)


	/** Checks if the given argument is compatible with the specialization of this collection and,
	  * if so, forwards the call to specialized [[SeqTemplate#fitLastIndexOf]] after casting,
	  * otherwise using [[SeqTemplate#superLastIndexOf]], which forwards the call to the iterator.
	  */
	override def lastIndexOf[U >: E](elem: U, end: Int): Int =
		if (specialization.boxType isAssignableFrom elem.getClass)
			lastPositionOf(elem.asInstanceOf[E], end)
		else
			superLastIndexOf(elem, end)

	/** Specialized variant of [[SeqTemplate#lastIndexOf]] searching for a value of our actual element type.
	  * Hotspot for subclasses to provide specialized implementation of searching for an element
	  * which actually is of our element type. Used by [[SeqTemplate#indexOf]] if the argument
	  * can be cast to `E`.
	  */
	protected[this] def lastPositionOf(elem :E, from :Int) :Int = superLastIndexOf(elem, from)



	/** Search for the element by delegating to this instance's iterator.
	  * Used particularly when `elem` is not an instance of `E` (or transparently convertible to such),
	  * so we can afford to not tune this to any extent as the result is most likely `-1`
	  * (and possibly a bug) anyway.
	  */
	protected[this] def superLastIndexOf[U >: E](elem: U, end: Int): Int =
		if (end < 0) -1
		else {
			val len = length
			val start = Math.min(end, len - 1)
			val it = reverseIterator.drop(len - 1 - start)
			val i = it.indexOf(elem)
			if (i < 0) -1 else start - i
		}




	override def reverseMap[@specialized(Fun1Vals) U, That](f: E => U)(implicit bf: CanBuildFrom[Repr, U, That]): That = {
		val b = FitBuilder(bf(repr)).mapInput(f)
		if (hasFastSize)
			b.sizeHint(length)
		b ++= reverseIterator
		b.result()
	}


	override def +:[U >: E, That](elem: U)(implicit bf: CanBuildFrom[Repr, U, That]): That = {
		val b = FitBuilder(bf(repr))
		if (hasFastSize)
			b.sizeHint(length + 1)
		b += elem ++= thisCollection
		b.result()
	}


	override def :+[U >: E, That](elem: U)(implicit bf: CanBuildFrom[Repr, U, That]): That = {
		val b = FitBuilder(bf(repr))
		if (hasFastSize)
			b.sizeHint(length + 1)
		b ++= thisCollection += elem
		b.result()
	}


	override def reverseIterator :FitIterator[E] = inverse.toIterator



	override def reverse: Repr = {
		val b = newBuilder
		reverseForeach(b.addOne)
		b.result()
	}




	def immutable[U >: E](implicit cbf: CanFitFrom[_, E, StableSeq[U]]): StableSeq[U] =
		(cbf() ++= this).result()

	override def toSeq: FitSeq[E] = toFitSeq


	override def toFitSeq: FitSeq[E] = this.asInstanceOf[FitSeq[E]]

	override def toIndexedSeq: IndexedSeq[E] =
		(StableArray.fitBuilder[E](specialization) ++= this).result()

	override def inverse: FitSeq[E] = (FitList.reverseBuilder(specialization) ++= this).result()






/*
	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
		if (specialization.runType isAssignableFrom xs.getClass.getComponentType )
			if (start<0)
				throw new IllegalArgumentException(s"$stringPrefix.copyToArray([], $start, $len)")
			else {
				val count = Math.min(xs.length-start, len)
				if (count>0)
					trustedCopyTo(xs.asInstanceOf[Array[E]], start, count)
			}
		else iterator.copyToArray(xs, start, len)

	protected[this] def trustedCopyTo(xs: Array[E], start: Int, total: Int): Int =
		if (isEmpty || total<=0) 0
		else {
			iterator.copyToArray(xs, start, total)
			Math.min(total, length)
		}
*/



}
