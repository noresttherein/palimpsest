package net.noresttherein.palimpsest.seqs

import net.noresttherein.palimpsest.iterables.EmptyIterableTemplate
import net.noresttherein.palimpsest.iterators.AptIterator

import scala.collection.{IndexedSeqLike, LinearSeq, LinearSeqLike}


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait EmptySeqTemplate[+E, +Repr <: AptSeq[E]] extends IndexedSeqLike[E, Repr] with SliceLike[E, Repr] with EmptyIterableTemplate[E, Repr]
{ this :Repr => //could extend LinearSeq

	override def inverse :AptSeq[E] = this

//	override def seq :Repr = this
	override def length = 0

	override def head :E = throw new NoSuchElementException(stringPrefix + "().head")
	override def tail :Repr = throw new UnsupportedOperationException(stringPrefix + "().tail")

	override def apply(idx :Int) :E = throw new NoSuchElementException("EmptySeq.at("+idx+")")
	protected override def section(from :Int, until :Int) :Repr = repr //indices already validated

	protected override def indexWhere(p :E => Boolean, ourTruth :Boolean, from :Int) :Int = -1
	override def segmentLength(p :E => Boolean, from :Int) :Int = 0
	override def lastIndexWhere(p: E => Boolean, from: Int): Int = -1

	override def indexOf[U >: E](elem :U, from :Int) :Int = -1
	override def lastIndexOf[U >: E](elem :U, end :Int) :Int = -1

//	override def iterator :FitIterator[E] = FitIterator.Empty
	override def reverseIterator :AptIterator[E] = AptIterator.Empty

	override def toFitSeq :Repr = repr
	override def toSeq :Repr = repr

	override def debugPrefix = "EmptySeq"
	override def debugString :String = debugPrefix + "[" + specialization.typeName + "]"
}
