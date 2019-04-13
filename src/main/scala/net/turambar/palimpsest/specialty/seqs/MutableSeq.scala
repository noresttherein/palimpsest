package net.turambar.palimpsest.specialty.seqs

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.{Elements, FitCompanion, FitIterableFactory, FitTraversableOnce, InterfaceIterableFactory, IterableSpecialization, SpecializableIterable, Specialized, arrayFill}

import scala.annotation.unspecialized



/**
  * @author Marcin Mościcki
  */
trait MutableSeq[@specialized(Elements) E]
	extends mutable.Seq[E] with mutable.SeqLike[E, MutableSeq[E]] with ValSeq[E] with ValSeqLike[E, MutableSeq[E]]
			with SpecializableIterable[E, MutableSeq]
{

	override def seq: MutableSeq[E] = this

	override def companion: FitCompanion[MutableSeq] = MutableSeq
	

	
//	override def seq: MutableSeq[E] = this.asInstanceOf[MutableSeq[E]]
	
	@inline
	final protected[seqs] def uncheckedUpdate(idx :Int, elem :E) :Unit = set(idx, elem)
	
	/** Set the value at the given index without checking the range. */
	protected[this] def set(idx :Int, elem :E) :Unit

	/** Fixed to validate the range of the index and delegate to [[MutableSeq#set]]. */
	override def update(idx: Int, elem: E): Unit =
		if (idx<0 || idx>=length)
			throw new IndexOutOfBoundsException(idx.toString)
		else set(idx, elem)


	/** Write the contents of the collection `elems` to this sequence, starting with index `idx`.
	  * Copying stops when either the end of this sequence or the passed collection is reached.
	  */
	def update(idx :Int, elems :TraversableOnce[E]) :Unit

	@unspecialized
	def update(idx :Int, elems :FitTraversableOnce[E]) :Unit = update(idx, elems :TraversableOnce[E])

	/** Write the given element `count` number of times in the range of indices `&lt;fromIndex..fromIndex+count)`
	  * in this sequence.
	  */
	def update(fromIndex :Int, value :E, count :Int) :Unit
	
	
	override def transform(f: (E) => E): this.type = {
		var i=0; val l=length
		while (i<l) {
			set(i, f(at(i)))
			i+=1
		}
		this
	}
	
	
	
	
	
	/** Treat this whole sequence as space for a buffer.
	  * Returned buffer will be initially empty, limited in capacity by this sequence's length,
	  * and start writing from the first element of this sequence.
	  * All append operations on the returned buffer trigger update of the corresponding elements in this sequence.
	  * @return an empty specialized buffer backed by this sequence.
	  */
	def overwrite :FitBuffer[E] = overwrite(0, length)
	
	/** Treat a section of this sequence as space for a buffer.
	  * Returned buffer will be initially empty and bound by range `start..(start+length) min this.length`
	  * of indices. Writing will start from the `start` index, and if any expansion method of the
	  * returned buffer was to write outside of the specified range, an exception will be thrown.
	  * In particular, all contents added to the created buffer will result in direct updates of this instance.
	  * @param start index in this sequence at which created buffer will start writing
	  * @param length maximum capacity of the buffer.
	  * @return an empty specialized buffer using the given range to store all its data.
	  */
	def overwrite(start :Int, length :Int) :FitBuffer[E]
	
	
	
}


object MutableSeq extends InterfaceIterableFactory[MutableSeq] {
	protected[this] type RealType[@specialized(Elements) X] = SharedArray[X]

	override protected[this] final def default: FitIterableFactory[SharedArray] = SharedArray

	def of[E <: AnyVal :Specialized](size :Int) :MutableSeq[E] =
		SharedArray(Specialized.arrayFor[E](size))

	def of[E :Specialized](size :Int, value :E) :MutableSeq[E] =
		SharedArray(arrayFill(Specialized.arrayFor[E](size), value))
	

	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[MutableSeq[_], E, MutableSeq[E]]): CanBuildFrom[MutableSeq[_], E, MutableSeq[E]] =
		fit.cbf
}
