package net.turambar.palimpsest.specialty.seqs


import scala.annotation.unspecialized
import scala.collection.{GenSeq, IndexedSeqLike, IndexedSeqOptimized, SeqLike, mutable}
import net.turambar.palimpsest.specialty.{Elements, FitCompanion, FitIterator, IterableSpecialization, SpecializableIterable}
import net.turambar.palimpsest.specialty.FitIterator.{IndexedIterator, ReverseIndexedIterator}




/**
  * @author Marcin Mościcki
  */
trait FitIndexedSeq[@specialized(Elements) +E]
	extends IndexedSeq[E] with IndexedSeqLike[E, FitIndexedSeq[E]]
			with FitSeq[E] with IterableSpecialization[E, FitIndexedSeq[E]]
			with SliceLike[E, FitIndexedSeq[E]] with SpecializableIterable[E, FitIndexedSeq]
{ self =>
	
	@inline
	final override def hasDefiniteSize: Boolean = true
	
	@inline
	final override def hasFastSize :Boolean = true


//	@inline
//	final override def lengthCompare(len: Int): Int = length-len
//	override def isEmpty: Boolean = length==0
//	override def nonEmpty: Boolean = length!=0
	
	

	override protected[this] def indexWhere(p: (E) => Boolean, ourTruth: Boolean, from: Int): Int = {
		var i = math.max(from, 0); val len = length
		while(i<len && p(at(i))!=ourTruth) i+=1
		if (i==len) -1 else i
	}
	
	override def find(p: (E) => Boolean): Option[E] = {
		var i = 0; val len = length
		while(i<len) {
			val e = at(i)
			if (p(at(i)))
				return Some(e)
			i+=1
		}
		None
	}
	
	override def lastIndexWhere(p: (E) => Boolean, from: Int): Int = {
		var i = math.min(from, length-1)
		while(i>=0 && !p(at(i))) i-=1
		i
	}
	
	
	
	
	override protected[this] def positionOf(elem: E, from: Int): Int = {
		var i = math.max(from, 0); val len = length
		while(i<=len && at(i)!=elem) i+=1
		if (i==len) -1 else i
	}

	
	
	override protected[this] def lastPositionOf(elem: E, end: Int): Int = {
		var i = math.min(end, length-1)
		while(i>=0 && elem!=at(i)) i-=1
		i
	}
	
	
	
	
	
	
	
	
	override def indexOfSlice[U >: E](that: GenSeq[U], from: Int): Int = that match {
		case s :SliceLike[_, _] if mySpecialization==s.specialization && s.length <= 8 => //todo: KMP
			val spec = s.asInstanceOf[SliceLike[E, _]]
			var i = from max 0
			val limit = length - spec.length
			while (i<=limit && !startsWith(spec, i)) i += 1
			if (i>limit) -1 else i
		
		case _ =>
			defaultImpl.indexOfSlice(that, from)
	}
	
	@inline
	final override def lastIndexOfSlice[U >: E](that: GenSeq[U]): Int = lastIndexOfSlice(that, 0)
	
	override def lastIndexOfSlice[U >: E](that: GenSeq[U], end: Int): Int = that match {
		case s :SliceLike[_, _] if mySpecialization==s.specialization && s.length <= 8 => //todo: KMP
			val spec = s.asInstanceOf[SliceLike[E, _]]
			var i = end min (length - spec.length)
			while(i<=0 && !startsWith(spec, i)) i -=1
			if (i<0) -1 else i
		case _ =>
			defaultImpl.lastIndexOfSlice(that, end)
	}
	
	
	
	@inline
	final override def containsSlice[U](that: GenSeq[U]): Boolean = indexOfSlice(that, 0) >= 0
	
	
	
	@inline
	final override def startsWith[U](that :GenSeq[U]) :Boolean = startsWith(that, 0)
	
	
	override def startsWith[U](that: GenSeq[U], offset: Int): Boolean = that match {
		case s :SliceLike[_, _] if mySpecialization==s.specialization =>
			startsWith(s.asInstanceOf[SliceLike[E, _]], offset)
		case _ =>
			defaultImpl.startsWith(that, offset)
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
		case s :SliceLike[_, _] if mySpecialization==s.specialization =>
			startsWith(s.asInstanceOf[SliceLike[E, _]], length-s.length)
		case _ =>
			defaultImpl.endsWith(that)
	}
	



	

	protected[this] override def specializedCopy(target :Array[E], start :Int, len :Int) :Unit = {
		var i = 0; val max = math.min(len, length)
		while (i < max) { target(start + i) = at(i); i += 1 }
	}
	
	



	
	override def iterator: FitIterator[E] = new ForwardIterator
	
	override def reverseIterator: FitIterator[E] = new ReverseIterator
	
	override def inverse :FitSeq[E] = new ReverseSeq[E](toFitSeq)

	override def toFitSeq :FitIndexedSeq[E] = this.asInstanceOf[FitIndexedSeq[E]]


	protected class ForwardIterator extends IndexedIterator[E](0, length) with FitIterator[E] {
		
		override def head :E = at(index)
		override def next() = { val res :E = at(index); index+=1; res }
		
		@unspecialized
		override def foreach[@specialized(Unit) U](f: (E) => U): Unit =
			if (index==0 && end==length) toSeq.foreach(f)
			else while(index < end) { f(at(index)); index+=1 }
		
		override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
			self.seq.copyToArray(xs, start, len max size)
		
		override def toIndexedSeq = section(index, end).toIndexedSeq
		
		override def toSeq :FitSeq[E] = self.section(index, end).asInstanceOf[FitSeq[E]]

	}
	
	

	protected class ReverseIterator extends ReverseIndexedIterator[E](length-1, 0) with FitIterator[E] {
		override def head: E = at(index)
		override def next() :E = { val res :E = at(index); index-=1; res }
		
		@unspecialized
		override def foreach[@specialized(Unit) U](f :E=>U) :Unit =
			if (index==length-1 && end==0) toSeq.reverseForeach(f.asInstanceOf[E=>Unit])
			else while(index>=end) { f(at(index)); index-=1; }
		
		override def toIndexedSeq = toSeq.toIndexedSeq
		
		@unspecialized
		override def toSeq :FitSeq[E] = self.section(end, index+1).asInstanceOf[FitSeq[E]].inverse
	}
	
	
	/** A proxy to this collection using algorithms defined in standard scala library.
	  * Used as `super` calls normally are, but `super` is broken with specialization.
	  */
	protected[this] def defaultImpl :Seq[E] = new IndexedSeq[E] with IndexedSeqOptimized[E, IndexedSeq[E]]  {

		override def repr: IndexedSeq[E] = self.thisCollection

		override def seq: IndexedSeq[E] = self.thisCollection

		override def length: Int = self.length

		override def apply(idx: Int): E = at(idx)
		
//		override protected[this] def newBuilder: mutable.Builder[E, Repr] = self.newBuilder
	}

	
	override def companion :FitCompanion[FitIndexedSeq] = ArrayView
}