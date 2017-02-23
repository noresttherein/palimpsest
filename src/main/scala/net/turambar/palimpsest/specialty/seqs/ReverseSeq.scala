package net.turambar.palimpsest.specialty.seqs

import scala.collection.generic.CanBuildFrom

import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation
import net.turambar.palimpsest.specialty.Specialized.Fun2Vals
import net.turambar.palimpsest.specialty.{Elements, FitIterator}

/** A backwards view of another [[FitSeq]].
  * Index `i` of this seq refers to element `original.length -1 -i` in the `original` sequence.
  *
  * @author Marcin Mościcki
  */
class ReverseSeq[@specialized(Elements) +E](override val reverse :FitIndexedSeq[E]) //todo: IndexedSeq?
	extends IterableFoundation[E, FitIndexedSeq[E]] with FitIndexedSeq[E] //with SliceLike[E, FitSeq[E]]//with FitIndexedSeqLike[E, FitSeq[E]]
{
	@inline
	final private[this] def orgIdx(idx :Int) = reverse.length -1 -idx
	

	/** Target of `apply` for internal use, assuming the index is valid (faster). */
	override protected[this] def at(idx: Int): E = reverse.get(reverse.length -1 -idx)

	@inline
	final override def length: Int = reverse.length

//	override def hasFastSize = reverse.hasFastSize
//	override def hasDefiniteSize = true
	
	@inline
	final override protected def section(from: Int, until: Int): FitIndexedSeq[E] =
		new ReverseSeq(sectionOf(reverse, orgIdx(until), orgIdx(from)))
	
	
	override def reverseMap[@specialized(Fun2Vals) U, That](f: (E) => U)(implicit bf: CanBuildFrom[FitIndexedSeq[E], U, That]): That =
		reverse.map(f)
	
	override def inverse = reverse

	override def iterator: FitIterator[E] = reverse.reverseIterator
	
	override def reverseIterator: FitIterator[E] = reverse.iterator
}



