package net.turambar.palimpsest.specialty.iterables

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.{?, Blank, FitBuilder, ItemTypes, Sure}
import net.turambar.palimpsest.specialty
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.{Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.iterables.FitIterable.{FilterIterable, SpecializedFilter}

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom



/** An implementation trait for `FitIterable` which redefines methods defined in
 * [[net.turambar.palimpsest.specialty.iterables.IterableTemplate IterableTemplate]] to their default implementations,
 * generally delegating to the corresponding method of this collection's iterator.
 * The reason for this duplication is that for some specific implementations of various collection interfaces,
 * most notably view-like classes, the overriden implementations inherited from their interface are no longer suitable,
 * but the classes are not prominent enough to warrant completely new overrides of all the methods
 * (especially as super calls are broken in specialized classes). By including this trait at the end of the extends
 * list, such a class reverts all implementations to their most general behaviour, requiring only a new implementation
 * of the `iterator` method, reducing class size.
 */
trait IterableOverridesTemplate[+E, +Repr] extends IterableTemplate[E, Repr] {

	override def hasFastSize :Boolean = isEmpty

	override def size :Int = iterator.size

	override def ofAtLeast(size :Int) :Boolean = iterator.ofAtLeast(size)



	override def filter(p :E => Boolean) :Repr = filter(p, true)

	override def filterNot(p :E => Boolean) :Repr = filter(p, false)

	def filter(p :E=>Boolean, neoTruth :Boolean) :Repr



	override def foreach[@specialized(Unit) U](f: E => U): Unit = iterator.foreach(f)

	protected def reverseForeach(f :E=>Unit) :Unit

	override def traverse(f :E=>Unit) :Unit = foreach(f)


	override def map[@specialized(Fun1Vals) O, That](f: E => O)(implicit bf: CanBuildFrom[Repr, O, That]): That = {
		val b = FitBuilder(bf(repr)).mapInput(f)
		b.sizeHint(this)
		b ++= this
		b.result()
	}

	override def flatMap[U, That](f: E => GenTraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]): That = {
		val b = FitBuilder(bf(repr)).flatMapInput(f)
		b ++= this
		b.result()
	}



	override def find(p :E=>Boolean) :Option[E] = find_?(p, true).toOption

	override def find_?(p :E=>Boolean, where :Boolean = true): ?[E] = iterator.find_?(p, where)

	override def exists(p :E=>Boolean) :Boolean = find_?(p).isDefined
	override def forall(p :E=>Boolean) :Boolean = find_?(p, false).isEmpty

	override def count(p :E => Boolean) :Int = iterator.count(p)


	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O): O = iterator.foldLeft(z)(op)

	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O): O = iterator.foldRight(z)(op)


	protected[this] override def trustedCopyTo(xs: Array[E], start: Int, total: Int): Int =
		if (isEmpty) 0
		else {
			iterator.copyToArray(xs, start, total)
			Math.min(total, size)
		}

}






/** An implementation trait for `FitIterable` which redefines methods defined in
 * [[net.turambar.palimpsest.specialty.iterables.IterableSpecialization IterableSpecialization]]
 * to their default implementations, generally delegating to the corresponding method of this collection's iterator.
 * The reason for this duplication is that for some specific implementations of various collection interfaces,
 * most notably view-like classes, the overriden implementations inherited from their interface are no longer suitable,
 * but the classes are not prominent enough to warrant completely new overrides of all the methods
 * (especially as super calls are broken in specialized classes). By including this trait at the end of the extends
 * list, such a class reverts all implementations to their most general behaviour, requiring only a new implementation
 * of the `iterator` method, reducing class size.
 */
trait IterableOverrides[@specialized(ItemTypes) +E, +Repr]
	extends IterableOverridesTemplate[E, Repr] with IterableSpecialization[E, Repr]
{
	override def head: E = iterator.head

	override def head_? : ?[E] = if (isEmpty) Blank else Sure(head)

	override def last_? : ?[E] = if (isEmpty) Blank else Sure(last)


	override def span(p: E => Boolean): (Repr, Repr) = specialty.span(iterator, newBuilder, newBuilder)(p)

	override def filter(p :E => Boolean, neoTruth :Boolean) :Repr = specialty.filter(iterator, newBuilder)(p, neoTruth)

	override def partition(p: E => Boolean): (Repr, Repr) = specialty.partition(iterator, newBuilder, newBuilder)(p)

}