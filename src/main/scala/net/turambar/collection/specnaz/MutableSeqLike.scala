package net.turambar.collection.specnaz

import scala.collection.generic.Subtractable
import scala.collection.{GenTraversableOnce, mutable}


/**
  * @author Marcin Mościcki
  */
trait MutableSeqLike[@specialized(Reified) E, +Repr<:MutableSeqLike[E, Repr] with MutableSeq[E]]
	extends mutable.IndexedSeqOptimized[E, Repr] with mutable.SeqLike[E, Repr] with Subtractable[E, Repr]
	with SpecSeqLike[E, Repr]
{

	@inline override final def specialization :Specialized[E] = Specialized[E]

	override def seq: MutableSeq[E] = this.asInstanceOf[MutableSeq[E]]

	@inline
	final protected[specnaz] def uncheckedUpdate(idx :Int, elem :E) :Unit = set(idx, elem)

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

	override def -(elem: E): Repr = {
		val i = indexOf(elem)
		if (i<0)
			(newBuilder ++= this).result()
		else {
			val b = newBuilder; b.sizeHint(length-1)
			b ++= subseq(0, i) ++= subseq(i+1, length)
			b.result()
		}
	}

	@inline
	final override def -(elem1: E, elem2: E, elems: E*): Repr = diff(Seq(elem1, elem2), elems)

	@inline
	final override def --(xs: GenTraversableOnce[E]): Repr = diff(Seq(), xs)

	protected[specnaz] def diff(elems1 :Traversable[E], elems2 :GenTraversableOnce[E]) :Repr = {
		val removedIndices = indicesOf(elems1, elems2)
		val b = newBuilder; b.sizeHint(removedIndices.size)
		var i=0; val l=length
		while(i<l) {
			if (!removedIndices(i)) b += at(i)
			i+=1
		}
		b.result()
	}

	protected[specnaz] def indicesOf(elems1 :Traversable[E], elems2 :GenTraversableOnce[E]) :mutable.Set[Int] = {
		var result = mutable.Set[Int]()
		var searchOffsets = mutable.Map[E, Int]().withDefaultValue(0)
		def collect(e :E) = {
			val i = indexOf(e, searchOffsets(e))
			if (i>=0) {
				result += i
				searchOffsets += e -> (i+1)
			}
		}
		elems1.foreach(collect); elems2.foreach(collect)
		result
	}



	def overwrite :SpecBuffer[E] = overwrite(0, length)

	def overwrite(start :Int, length :Int) :SpecBuffer[E]



}
