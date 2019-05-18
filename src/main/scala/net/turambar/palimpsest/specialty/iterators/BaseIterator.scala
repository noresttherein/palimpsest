package net.turambar.palimpsest.specialty.iterators

import net.turambar.palimpsest.specialty.{?, Elements}
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize

import scala.collection.AbstractIterator


/** Base class for [[FitIterator]] instances introduced to minimize static forwarding of trait methods.
  * This class is ''unspecialized'' (so can be extended by specialized classes)
  * and contains only methods which don't need specializations (to be provided by concrete implementations).
  * Note that this class doesn't implement [[FitIterator]] in order to enforce subclasses to explicitly
  * extend it, as otherwise they would inherit just the unspecialized version
  */
abstract class BaseIterator[+E] extends AbstractIterator[E] with IteratorTemplate[E] { this :FitIterator[E] =>

	//	override def traverse(f: E => Unit): Unit = foreach[Unit](f :E=>Unit) //dear compiler, please use specialized f.apply


	//		/** Delegates to the specialized `next()`. This will result in boxing the returned value,
	//		  * but a single boxing may be preferable to doing the whole computation boxed.
	//		  */
	//		override def skip() :Unit = next()
}





/** An iterator of known size, computable in `O(1)` or close enough to make little difference.
  * Implements size-related methods using its `size`.
  */
abstract class FastSizeIterator[+E] extends BaseIterator[E] with OfKnownSize { this :FitIterator[E] =>
	override def hasNext :Boolean = size > 0
}



/** A ''single abstract method'' iterator base trait which implements all required methods using abstract `next_?`
  * returning an uncertain value. Having a single abstract method allows concrete subclasses to be defined
  * in-line using function syntax.
  */
trait SAMIterator[@specialized(Elements) +E] extends FitIterator[E] {
	private[this] var future: ?[E] = _

	/** If non-empty, returns the next element as a `Sure` value, immediately advancing over it.
	  * If empty, returns simply `Blank`. It must be equivalent to:
	  * {{
	  *      if (hasNext) Sure(next())
	  *      else Blank
	  * }}
	  */
	def next_? : ?[E]

	override def hasNext :Boolean = {
		if (future == null)
			future = next_?
		future.isDefined
	}


	override def head :E = {
		if (future == null)
			future = next_?
		future.get
	}

	override def next() :E = {
		if (future == null)
			future = next_?
		val res = future.get
		future = next_?
		res
	}

	override def skip() :Unit = next()

}





/** Non specialized base class for iterators of known size, specified by the `limit` parameter. */
abstract class CountdownIterator[+E](protected[this] final var limit :Int) extends FastSizeIterator[E] { this :FitIterator[E] =>
	override def size :Int = Math.max(0, limit)
	override def hasNext :Boolean = limit > 0

	override def take(n :Int) :FitIterator[E] =
		if (n>size) this
		else if (n<=0) empty
		else {
			limit -= n; this
		}

	override def drop(n :Int) :FitIterator[E] =
		if (n<=0) this
		else if (n>size) empty
		else {
			val until = size-n
			do { skip() } while (size > until)
			this
		}
}
