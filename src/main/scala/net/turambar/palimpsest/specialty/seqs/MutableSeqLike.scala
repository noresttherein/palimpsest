package net.turambar.palimpsest.specialty.seqs

import scala.collection.generic.Subtractable
import scala.collection.{GenTraversableOnce, mutable}

import net.turambar.palimpsest.specialty.{ItemTypes, RuntimeType}

/**
  * @author Marcin Mościcki
  */
/*
trait MutableSeqLike[@specialized(Elements) E, +Repr<:MutableSeqLike[E, Repr] with MutableSeq[E]]
	extends mutable.IndexedSeqOptimized[E, Repr] with mutable.SeqLike[E, Repr] with Subtractable[E, Repr]
	        with AptIndexedSeq[E, Repr]
{

	@inline override final def specialization :Specialized[E] = Specialized[E]

	override def seq: MutableSeq[E] = this.asInstanceOf[MutableSeq[E]]

	@inline
	final protected[seqs] def uncheckedUpdate(idx :Int, elem :E) :Unit = set(idx, elem)
	
	/** Set the value at the given index without checking the range. */
	protected[this] def set(idx :Int, elem :E) :Unit

	@inline
	final override def update(idx: Int, elem: E): Unit =
		if (idx<0 || idx>=length)
			throw new IndexOutOfBoundsException(idx.toString)
		else set(idx, elem)

	def update(idx :Int, elems :TraversableOnce[E]) :Unit


	def update(fromIndex :Int, value :E, count :Int) :Unit


	override def transform(f: (E) => E): this.type = {
		var i=0; val l=length
		while (i<l) {
			set(i, f(at(i)))
			i+=1
		}
		this
	}

	
	//todo: move subtractable implementation somewhere
	override def -(elem: E): Repr = {
		val i = indexOf(elem)
		if (i<0)
			(newBuilder ++= this).result()
		else {
			val b = newBuilder; b.sizeHint(length-1)
			b ++= section(0, i) ++= section(i+1, length)
			b.result()
		}
	}

	@inline
	final override def -(elem1: E, elem2: E, elems: E*): Repr = diff(Seq(elem1, elem2), elems)

	@inline
	final override def --(xs: GenTraversableOnce[E]): Repr = diff(Seq(), xs)

	protected[seqs] def diff(elems1 :Traversable[E], elems2 :GenTraversableOnce[E]) :Repr = {
		val removedIndices = indicesOf(elems1, elems2)
		val b = newBuilder; b.sizeHint(removedIndices.size)
		var i=0; val l=length
		while(i<l) {
			if (!removedIndices(i)) b += at(i)
			i+=1
		}
		b.result()
	}

	protected[seqs] def indicesOf(elems1 :Traversable[E], elems2 :GenTraversableOnce[E]) :mutable.Set[Int] = {
		var result = mutable.Set[Int]()
		var searchOffsets = mutable.Map[E, Int]().withDefaultValue(0)
		def collect(e :E) :Unit = { //todo this is not specialized!
			val i = indexOf(e, searchOffsets(e))
			if (i>=0) {
				result += i
				searchOffsets += e -> (i+1)
			}
		}
		elems1.foreach(collect); elems2.foreach(collect)
		result
	}
	
	
	/** Treat this whole sequence as space for a buffer.
	  * Returned buffer will be initially empty, limited in capacity by this sequence's length,
	  * and start writing from the first element of this sequence.
	  * All append operations on the returned buffer trigger update of the corresponding elements in this sequence.
	  * @return an empty specialized buffer backed by this sequence.
	  */
	def overwrite :AptBuffer[E] = overwrite(0, length)
	
	/** Treat a section of this sequence as space for a buffer.
	  * Returned buffer will be initially empty and bound by range `start..(start+length) min this.length`
	  * of indices. Writing will start from the `start` index, and if any expansion method of the
	  * returned buffer was to write outside of the specified range, an exception will be thrown.
	  * In particular, all contents added to the created buffer will result in direct updates of this instance.
	  * @param start index in this sequence at which created buffer will start writing
	  * @param length maximum capacity of the buffer.
	  * @return an empty specialized buffer using the given range to store all its data.
	  */
	def overwrite(start :Int, length :Int) :AptBuffer[E]



}
*/