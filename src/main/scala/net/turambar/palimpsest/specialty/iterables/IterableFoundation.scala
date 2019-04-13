package net.turambar.palimpsest.specialty.iterables

import net.turambar.palimpsest.specialty.Specialized.{Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.{FitBuilder, FitIterable, FitIterator, FitTraversableOnce, IterableSpecialization, IterableTemplate, Specialized, ofKnownSize}

import scala.collection.{GenIterable, GenTraversableOnce}
import scala.collection.generic.CanBuildFrom


/** Base class for [[FitIterable]] hierarchy containing default implementations of all methods
  * which don't require specialization (or can be implemented by delegating to a smaller subset of specialized methods).
  * Exists to decrease the class sizes of concrete types as well as static forwarding to trait-implemented methods.
  * All implementations provided here, unless stated to the contrary, delegate to the corresponding iterator methods and,
  * in case of collection results, append the iterator as a whole to `newBuilder`.
  * @author Marcin Mościcki
  */
abstract class IterableFoundation[+E, +Repr] extends FitTraversableOnce[E] with IterableTemplate[E, Repr] {
//todo: this shouldn't extend FitTraversableOnce
//
//	override def specialization :Specialized[_<:E] = mySpecialization
//
//	//		@inline override final def items :ForEach[E] = new ForEach(this)
//	override def isEmpty = !ofAtLeast(1)
//	override def nonEmpty = ofAtLeast(1)
//	override def hasFastSize = isEmpty
//
//
//	protected[this] def dropTake(from :Int, until :Int) :Repr = {
//		val builder = newBuilder
//		val taken = until-from
//		if (taken<=0)
//			builder.result()
//		else {
//			builder.sizeHintBounded(taken, this)
//			builder ++= iterator.drop(from).take(taken)
//			builder.result()
//		}
//	}
//
//	override def slice(from: Int, until: Int): Repr = dropTake(math.max(from, 0), until)
//
//	override def take(n: Int): Repr = dropTake(0, n)
//
//	override def drop(n: Int): Repr = dropTake(math.max(n, 0), Int.MaxValue)
//
//
//
//	override def takeWhile(p: (E) => Boolean): Repr = {
//		val b = newBuilder
//		b ++= iterator.takeWhile(p)
//		b.result()
//	}
//
//	override def dropWhile(p: (E) => Boolean): Repr = {
//		val b = newBuilder
//		b ++= iterator.dropWhile(p)
//		b.result()
//	}
//
//
//
//
//	/*  ********** Traversing & predicate testing methods delegating to the iterator *************  */
//
//	//overrides to limit static forwarders in subclasses
//
//	/** Delegates to iterator's specialized [[FitIterator#foreach]]. */
//	override def foreach[@specialized(Unit) O](f: (E) => O): Unit = iterator.foreach(f)
//
//	/** Traverses the whole collection in an order opposite to [[foreach]]. */
//	override protected def reverseForeach(f :E=>Unit) :Unit = inverse.foreach(f)
//
//	override def traverse(f :E=>Unit) :Unit = foreach(f) //iterator.foreach(f)
//
//	override def forall(p: E => Boolean): Boolean = iterator.forall(p)
//
//
//	override def exists(p: E => Boolean): Boolean = iterator.exists(p)
//
//
//	override def find(p: E => Boolean): Option[E] = iterator.find(p)
//
//
//	override def count(p: (E) => Boolean): Int = iterator.count(p)
//
//
//	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O): O = iterator.foldLeft(z)(op)
//
//
//	/** Delegates to [[foldLeft]]. */
//	@inline final override def /:[@specialized(Fun2) O](z: O)(op: (O, E) => O): O = foldLeft(z)(op)
//
//
//	/** Delegates to specialized [[foldRight]]. */
//	@inline final override def :\[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = foldRight(z)(op)
//
//
//	override def fold[U >: E](z: U)(op: (U, U) => U): U = foldLeft(z)(op)
//
//
//
//	/** Delegates to the corresponding method of our iterator and adds the whole iterator to a new builder from `newBuilder`. */
//	protected[this] def filter(p :(E) => Boolean, ourTruth :Boolean) :Repr =
//		(newBuilder ++= iterator.filter(p, ourTruth)).result()
//
//
//	override def filter(p: (E) => Boolean): Repr = filter(p, ourTruth = true)
//
//	override def filterNot(p: (E) => Boolean): Repr = filter(p, ourTruth = false)
//
//
//
//
//	override def map[@specialized(Fun1Vals) O, That](f: (E) => O)(implicit bf: CanBuildFrom[Repr, O, That]): That = {
//		val b = FitBuilder(bf(repr)).mapInput(f)
//		b.sizeHint(this)
//		b ++= this
//		b.result()
//	}
//
//
//
//	override def flatMap[U, That](f: E => GenTraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]): That = {
//		val b = FitBuilder(bf(repr)).flatMapInput(f)
//		b ++= this
//		b.result()
//	}
//
//
//
//	override def ++[B >: E, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
//		val b = FitBuilder(bf(repr))
//		if (hasFastSize && ofKnownSize(that))
//			b.sizeHint(size + that.size)
//		b ++= thisCollection ++= that.seq
//		b.result()
//	}
//
//	override def ++:[B >: E, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
//		val b = FitBuilder(bf(repr))
//		if (hasFastSize && ofKnownSize(that))
//			b.sizeHint(size + that.size)
//		b ++= thisCollection ++= that.seq
//		b.result()
//	}
//
//
//
//	override def iterator :FitIterator[E] = toIterator
//
//
//	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
//		if (mySpecialization.runType isAssignableFrom xs.getClass.getComponentType )
//			if (start<0)
//				throw new IllegalArgumentException(s"$stringPrefix.copyToArray([], $start, $len)")
//			else {
//				val count = math.min(xs.length-start, len)
//				if (count>0)
//					uncheckedCopyTo(xs.asInstanceOf[Array[E]], start, count)
//			}
//		else iterator.copyToArray(xs, start, len)
//
//	protected[this] override def uncheckedCopyTo(xs: Array[E], start: Int, total: Int): Int =
//		if (isEmpty) 0
//		else { iterator.copyToArray(xs, start, total); math.min(total, size) }
//
//
//	override def sameElements[U >: E](that: GenIterable[U]): Boolean = that match {
//		case it :FitIterable[U] if hasFastSize && it.hasFastSize && size!=it.size => false
//		case _ =>
//			val these = this.iterator
//			val those = that.iterator
//			these sameElements those
//	}

}
