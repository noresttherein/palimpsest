package net.turambar.palimpsest.specialty

import scala.collection.generic.CanBuildFrom

import Specialized.Fun2Vals
import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation

/** A backwards view of another [[FitSeq]].
  * Index `i` of this seq refers to element `original.length -1 -i` in the `original` sequence.
  *
  * @author Marcin Mościcki
  */
class ReverseSeq[@specialized(Elements) +E](override val reverse :FitSeq[E]) extends IterableFoundation[E, FitSeq[E]] with FitSeq[E] {
	@inline
	final private[this] def orgIdx(idx :Int) = reverse.length -1 -idx
	
	@inline
	final override protected[this] def at(idx: Int): E = reverse(orgIdx(idx))
	
	@inline
	final override def length: Int = reverse.length

	
	@inline
	final override protected def section(from: Int, until: Int): FitSeq[E] =
		new ReverseSeq(sectionOf(reverse, orgIdx(until), orgIdx(from)))
	
	
	override def reverseMap[@specialized(Fun2Vals) U, That](f: (E) => U)(implicit bf: CanBuildFrom[FitSeq[E], U, That]): That =
		reverse.map(f)
	
	override def inverse = reverse

	override def iterator: FitIterator[E] = reverse.reverseIterator
	
	override def reverseIterator: FitIterator[E] = reverse.iterator
}



