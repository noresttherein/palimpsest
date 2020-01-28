package net.turambar.palimpsest.specialty.seqs

import java.lang.Math

import scala.annotation.unspecialized
import scala.collection.{immutable, GenSeq, IndexedSeqLike, SeqLike}
import net.turambar.palimpsest.specialty.{?, Blank, ItemTypes, Sure}
import net.turambar.palimpsest.specialty.iterators.{IndexedIterator, ReverseIndexedIterator}
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.iterables.{CloneableIterable, FitCompanion, FitIterableFactory, InterfaceIterableFactory, IterableSpecialization, IterableTemplate, SpecializableIterable, StableIterableTemplate}
import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterators.FitIterator

import scala.collection.generic.CanBuildFrom



trait FitIndexedSeqTemplate[+E, +S]
	extends SeqLike[E, S] with IndexedSeqLike[E, S] with IterableTemplate[E, S] with SliceLike[E, S] with OfKnownSize
{
	override def indexOfSlice[U >: E](that: GenSeq[U], from: Int): Int = that match {
		case s :SliceLike[_, _] if specialization==s.runtimeType && s.length <= 8 => //todo: KMP
			val spec = s.asInstanceOf[SliceLike[E, _]]
			var i = Math.max(from, 0)
			val limit = length - spec.length
			while (i<=limit && !startsWithUnchecked(spec, i))
				i += 1
			if (i>limit) -1 else i

		case _ =>
			super[SeqLike].indexOfSlice(that, from)
	}

	@inline
	final override def lastIndexOfSlice[U >: E](that: GenSeq[U]): Int = lastIndexOfSlice(that, 0)

	override def lastIndexOfSlice[U >: E](that: GenSeq[U], end: Int): Int = that match {
		case s :SliceLike[_, _] if specialization==s.runtimeType && s.length <= 8 => //todo: KMP
			val spec = s.asInstanceOf[SliceLike[E, _]]
			var i = end min (length - spec.length)
			while(i<=0 && !startsWith(spec, i)) i -=1
			if (i<0) -1 else i
		case _ =>
			super[SeqLike].lastIndexOfSlice(that, end)
	}



	@inline
	final override def containsSlice[U](that: GenSeq[U]): Boolean = indexOfSlice(that, 0) >= 0



	@inline
	final override def startsWith[U](that :GenSeq[U]) :Boolean = startsWith(that, 0)


	override def startsWith[U](that: GenSeq[U], offset: Int): Boolean = that match {
		case s :SliceLike[_, _] if specialization==s.runtimeType =>
			startsWith(s.asInstanceOf[SliceLike[E, _]], offset)
		case _ =>
			super[SeqLike].startsWith(that, offset)
	}


	@inline @unspecialized
	final protected[this] def startsWith(that :SliceLike[E, _], offset :Int) :Boolean =
		!(offset>length || offset<0 || length-offset <= that.length) &&
			startsWithUnchecked(that, offset)

	@unspecialized
	protected[this] def startsWithUnchecked(that: SliceLike[E, _], offset: Int): Boolean = {
		iterator.drop(offset).take(that.length) sameElements that.iterator
	}



	final override def endsWith[U](that: GenSeq[U]): Boolean = that match {
		case s :SliceLike[_, _] if specialization==s.runtimeType =>
			startsWith(s.asInstanceOf[SliceLike[E, _]], length-s.length)
		case _ =>
			super[SeqLike].endsWith(that)
	}



	override def iterator :FitIterator[E] = FitIterator.adapt(super[IndexedSeqLike].iterator)
}





/**
  * @author Marcin Mościcki
  */
//todo: delete this class
trait FitIndexedSeq[@specialized(ItemTypes) +E]
	extends IndexedSeq[E] with IndexedSeqLike[E, FitIndexedSeq[E]]
	   with FitSeq[E] with IterableSpecialization[E, FitIndexedSeq[E]] with CloneableIterable[E, FitIndexedSeq[E]]
	   with FitIndexedSeqTemplate[E, FitIndexedSeq[E]] with SpecializableIterable[E, FitIndexedSeq] with OfKnownSize
{ self =>
	

	override protected[this] def indexWhere(p: E => Boolean, ourTruth: Boolean, from: Int): Int = {
		var i = Math.max(from, 0); val len = length
		while(i<len && p(at(i))!=ourTruth) i+=1
		if (i==len) -1 else i
	}
	
	override def find(p: E => Boolean): Option[E] = {
		var i = 0; val len = length
		while(i<len) {
			val e = at(i)
			if (p(e))
				return Some(e)
			i+=1
		}
		None
	}

	override def find_?(p :E => Boolean, where :Boolean): ?[E] = {
		var i = 0; val len = length
		while (i < len) {
			val e = at(i)
			if (p(e) == where)
				return Sure(e)
			i += 1
		}
		Blank
	}
	
	override def lastIndexWhere(p: E => Boolean, from: Int): Int = {
		var i = Math.min(from, length-1)
		while(i>=0 && !p(at(i))) i-=1
		i
	}
	
	
	
	
	override protected[this] def offsetOf(elem: E, from: Int): Int = {
		var i = Math.max(from, 0); val len = length
		while(i<=len && at(i)!=elem) i+=1
		if (i==len) -1 else i
	}

	
	
	override protected[this] def lastOffsetOf(elem: E, end: Int): Int = {
		var i = Math.min(end, length-1)
		while(i>=0 && elem!=at(i)) i-=1
		i
	}
	
	


	protected[this] override def trustedCopyTo(target :Array[E], start :Int, len :Int) :Int = {
		var i = 0; val max = Math.min(len, length)
		while (i < max) { target(start + i) = at(i); i += 1 }
		Math.max(0, max)
	}
	
	

	override def iterator: FitIterator[E] = new ForwardIterator

	override def reverseIterator: FitIterator[E] = new ReverseIterator

	override def inverse :FitSeq[E] = new ReverseSeq[E](toSeq)

	override def toSeq :FitIndexedSeq[E] = this.asInstanceOf[FitIndexedSeq[E]]


	//todo: inner classes are not specialized
	protected class ForwardIterator extends IndexedIterator[E](0, length) with FitIterator[E] {

		override def head :E = at(index)
		override def next() :E = { val res :E = at(index); index+=1; res }

		@unspecialized
		override def foreach[@specialized(Unit) U](f: E => U): Unit =
			if (index==0 && end==length) toSeq.foreach(f)
			else while(index < end) { f(at(index)); index+=1 }

		override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
			self.seq.copyToArray(xs, start, len max size)

		override def toIndexedSeq :collection.immutable.IndexedSeq[E] = section(index, end).toIndexedSeq

		override def toSeq :FitSeq[E] = self.section(index, end).asInstanceOf[FitSeq[E]]

	}



	protected class ReverseIterator extends ReverseIndexedIterator[E](length-1, 0) with FitIterator[E] {
		override def head: E = at(index)
		override def next() :E = { val res :E = at(index); index-=1; res }

		@unspecialized
		override def foreach[@specialized(Unit) U](f :E=>U) :Unit =
			if (index==length-1 && end==0) toSeq.reverseTraverse(f.asInstanceOf[E=>Unit])
			else while(index>=end) { f(at(index)); index-=1; }

		override def toIndexedSeq :collection.immutable.IndexedSeq[E] = toSeq.toIndexedSeq

		@unspecialized
		override def toSeq :FitSeq[E] = self.section(end, index+1).asInstanceOf[FitSeq[E]].inverse
	}


	override def companion :FitCompanion[FitIndexedSeq] = ArrayView
}










trait StableIndexedSeq[+E]
	extends immutable.IndexedSeq[E] with IndexedSeqLike[E, StableIndexedSeq[E]]
		with FitIndexedSeq[E] with StableSeq[E] with StableIterableTemplate[E, StableIndexedSeq[E]]
		with IterableSpecialization[E, StableIndexedSeq[E]] with SpecializableIterable[E, StableIndexedSeq]
		with SliceLike[E, StableIndexedSeq[E]] with CloneableIterable[E, StableIndexedSeq[E]]
{
	override def toSeq :StableIndexedSeq[E] = this
	override def companion :FitCompanion[StableIndexedSeq] = StableIndexedSeq
}



object StableIndexedSeq extends InterfaceIterableFactory[StableIndexedSeq] {
	override protected[this] type RealType[@specialized(ItemTypes) +X] = ArrayPlus[X]

	override protected def default :FitIterableFactory[RealType] = ArrayPlus

	override implicit def canBuildFrom[E](implicit fit :CanFitFrom[StableIndexedSeq[_], E, StableIndexedSeq[E]])
			:CanBuildFrom[StableIndexedSeq[_], E, StableIndexedSeq[E]] =
		fit.cbf
}
