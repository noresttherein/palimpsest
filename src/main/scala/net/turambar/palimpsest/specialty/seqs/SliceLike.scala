package net.turambar.palimpsest.specialty.seqs

import scala.annotation.unspecialized
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.{GenIterable, GenSeq, GenTraversableOnce, IndexedSeqLike, IndexedSeqOptimized, SeqLike, immutable, mutable}

import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterators.{IndexedIterator, ReverseIndexedIterator}
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.Fun1Vals
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, RuntimeType, ofKnownSize}


/** An interface base trait for sequence-like collections which provide reasonably efficient slicing
  * operations. Serves as the highest upper bound for specialized `Seq`s (all `FitSeq` implementations),
  * but isn't specialized itself. Instead, it provides reasonable defaults for all methods that
  * can be implemented without specialization and attempts to minimise the number of specialized
  * methods required of subclasses.
  *
  * In particular, these would be:
  *   - [[SliceLike#section]] (for all `tail`/`drop`/`slice` methods)
  *   - [[SliceLike#length]] - generally assumed to be O(1) in most cases;
  *   - [[SliceLike#indexWhere(E=>Boolean, Int)]] - especially if `length` isn't efficient - and
  *   - [[SliceLike#segmentLength]] (which currently delegate to common [[SliceLike@indexWhere(E=>Boolean, Boolean, Int)]]).
  *	  - [[SliceLike#splitAt]] and [[SliceLike#span]] (optionally, if arbitrary `section` operations aren't effective)
  *
  * @tparam E    specialized element type
  * @tparam Repr a specialized collection, assumed to be a subclass of [[FitSeq]]
  * @see [[FitSeq]]
  * @author Marcin Mościcki
  */
trait SliceLike[+E, +Repr] extends SeqLike[E, Repr] with SeqTemplate[E, Repr] { //SeqLike for super calls


	/** Create a slice of this instance assuming the indices are already validated. Delegated to from [[slice]] and other subsequence methods. */
	protected def section(from: Int, until: Int): Repr

	/** Access to protected `section` method of sibling collections. */
	@inline
	final protected[this] def sectionOf(seq: SliceLike[_, Repr], from: Int, until: Int): Repr = seq.section(from, until)


	/** Empty collection returned when requested for slices of zero length; implemented as `section(0, 0)`. */
	protected[this] def empty: Repr = section(0, 0)


	/** ************ General methods ***************/

	/** The length of this sequence, assumed to be a fast operation by
	  * default implementations of most methods provided here. While a prevailing feature
	  * among collections in this library, this isn't strictly required as long as the sequence
	  * provides alternate implementations for methods which currently make use of it.
	  */
	override def length :Int
	override def lengthCompare(len :Int) :Int = length - len

	override def hasDefiniteSize: Boolean = true
	override def hasFastSize :Boolean = true

	override def isEmpty :Boolean = length == 0
	override def nonEmpty :Boolean = length > 0

	/** Implemented by [[SliceLike#section]] and [[SliceLike#length]]. */
	override def tail: Repr =
		if (nonEmpty) section(1, length)
		else throw new UnsupportedOperationException("empty.tail")

	/** Implemented using [[SliceLike#length]] [[SliceLike#section]]. */
	override def init: Repr =
		if (nonEmpty) section(0, length - 1)
		else throw new UnsupportedOperationException("empty.init")

	/** Validates against [[SliceLike#length]] and delegates to [[SliceLike#section]]. */
	override def take(n: Int): Repr =
		if (n >= length) repr
		else if (n <= 0) empty //section(0, 0)
		else section(0, n)

	/** Validates against [[SliceLike#length]] and delegates to [[SliceLike#section]]. */
	override def drop(n: Int): Repr =
		if (n >= length) empty //section(length, length)
		else if (n <= 0) repr
		else section(n, length)

	/** Validates against [[SliceLike#length]] and delegates to [[SliceLike#section]]. */
	override def takeRight(n: Int): Repr =
		if (n >= length) repr
		else if (n <= 0) empty //section(length, length)
		else section(length - n, length)

	/** Validates against [[SliceLike#length]] and delegates to [[SliceLike#section]]. */
	override def dropRight(n: Int): Repr =
		if (n >= length) empty //section(0, 0)
		else if (n <= 0) repr
		else section(0, length - n)

	/** Validates against [[SliceLike#length]] and delegates to [[SliceLike#section]]. */
	override def splitAt(n: Int): (Repr, Repr) =
		if (n >= length) (repr, empty) //(repr, section(length, length))
		else if (n <= 0) (empty, repr) //(section(0, 0), repr)
		else (section(0, n), section(n, length))

	/** Validates against [[SliceLike#length]] and delegates to [[SliceLike#section]]. */
	override def slice(from: Int, until: Int): Repr = {
		val start = from max 0 min length
		section(start, until max start min length)
	}


	/** Delegates to [[SliceLike#section]] and [[SliceLike#prefixLength]]. */
	@inline
	override def takeWhile(p: E => Boolean): Repr = section(0, prefixLength(p))

	/** Delegates to [[SliceLike#section]] and [[SliceLike#prefixLength]]. */
	override def dropWhile(p: E => Boolean): Repr = section(prefixLength(p), length)

	/** Implemented using [[SliceLike#dropWhile]] for the suffix and [[SliceLike#length]] with [[SliceLike#section]] for the prefix. */
	override def span(p: E => Boolean): (Repr, Repr) = {
		val suffix = dropWhile(p)
		(section(0, length - toCollection(suffix).length), suffix)
	}



	/** Implemented here by using [[SeqTemplate#indexWhere(E=>Boolean, Boolean, Int)]] and [[SeqTemplate#length]].
	  * If the derived sequence doesn't provide fast `length` operation it makes particular sense to override
	  * this method (best together with [[SeqTemplate#indexWhere(E=Boolean, Int)]], instead of the longer variant).
	  */
	override def segmentLength(p: E => Boolean, from: Int): Int =
		indexWhere(p, false, from) match {
			case -1 => math.max(length - math.max(from, 0), 0)
			case n => math.max(from, 0) + n
		}





}
