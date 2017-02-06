package net.turambar.palimpsest.specialty.seqs

import scala.annotation.{tailrec, unspecialized}
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.LinearSeq
import scala.collection.{GenSeq, LinearSeqLike, SeqLike, mutable}

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation
import net.turambar.palimpsest.specialty.FitIterator.CountdownIterator
import net.turambar.palimpsest.specialty.seqs.FitList.{FitListBuilder, FitListIterator, FullLink, Link, Terminus}
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitCompanion, FitIterator, ImplementationIterableFactory, Specialized, SpecializedTraversableTemplate}

/** Specialized linked list with O(1) `length` and O(1) `take` operations.
  * Random indexing and `drop` still take O(n), though.
  * In line with [[FitSeq]] philosophy, slicing tries to share the contents and return view on
  * the parent sequence which prevent 'dropped' tails from garbage collection -
  * make a copy if you want to store one for longer and suspect it's only a fragment of a larger structure.
  *
  * @author Marcin Mościcki
  */
class FitList[@specialized(Elements) +E] private[seqs] (
		override val length :Int,
		contents :Link[E]
	) extends IterableFoundation[E, FitList[E]] with LinearSeq[E] with LinearSeqLike[E, FitList[E]]
              with ConstSeq[E] with FitSeqLike[E, FitList[E]] with SpecializedTraversableTemplate[E, FitList]
{
	import Specialized.Fun2Vals
	
	
	@tailrec private[this] final def dropped(n :Int, l :Link[E]=contents) :Link[E] =
		if (n<=0) l
		else dropped(n-1, l.tail)
	
	override protected def section(from: Int, until: Int): FitList[E] =
		new FitList(until - from, dropped(from))
	
	override protected[this] def emptyCollection: FitList[E] = FitList.Empty
	
	override protected[this] def at(idx: Int): E = {
		var i = 0; var l = contents
		while (i<idx) { l = l.tail; i += 1 }
		l.head
	}
	
	override def head: E =
		if (length>0) contents.head
		else throw new NoSuchElementException(s"FitList().head")
	
	
	override def headOption =
		if (length>0) Some(contents.head)
		else None
	
	/************** Slicing methods ***************/
	
	override def tail :FitList[E] =
		if (length==0) throw new UnsupportedOperationException(s"FitList().tail")
		else new FitList(length-1, contents.tail)
	
	
	override def take(n: Int): FitList[E] =
		if (n<=0) FitList.Empty
		else if (n>=length) this
		else new FitList[E](n, contents)
	
	
	override def dropWhile(p: (E) => Boolean): FitList[E] = {
		var len=length; var elems=contents
		while(len>0 && p(elems.head)) { len-=1; elems=elems.tail }
		new FitList(len, elems)
	}
	
	
	
	
	
	
	/************** Searching for an element methods  ***************/
	
	override def segmentLength(p: (E) => Boolean, from: Int): Int =
		if (from>=length) 0
		else {
			var l = dropped(from); var len=0; val max = length - math.max(from, 0)
			while(len < max && p(l.head)) {
				len += 1; l=l.tail
			}
			len
		}
	
	
	override def indexWhere(p: (E) => Boolean, from: Int): Int =
		if (from>=length) 0
		else {
			var l = dropped(from); var len=0; val end = length - math.max(from, 0)
			while(len < end && !p(l.head)) {
				len += 1; l = l.tail
			}
			len
		}
	
	
	
	override def find(p: (E) => Boolean): Option[E] = {
		var len = length; var elem = contents
		while(len>0 && !p(elem.head)) {
			len -= 1; elem = elem.tail
		}
		if (len>0) Some(elem.head)
		else None
	}
	

	
	override def lastIndexWhere(p: (E) => Boolean, from: Int): Int = {
		var i =0; var l = contents; var last = -1; val e = math.min(from, length)
		while(i<e) {
			if (p(l.head)) last = i
			i += 1
			l = l.tail
		}
		last
	}
	
	
	override protected[this] def fitIndexOf(elem: E, from: Int): Int =
		if (from>=length) -1
		else {
			var i = math.max(from, 0); var l = dropped(i)
			while (i<length && l.head != elem) {
				i+=1; l=l.tail
			}
			if (i==length) -1 else i
		}
	
	override protected[this] def fitLastIndexOf(elem: E, end: Int): Int = {
		var i = 0; var l = contents; var last = -1; val e = math.min(end, length)
		while(i<e) {
			if (l.head==elem) last = i
			i+=1; l=l.tail
		}
		last
	}
	
	
	
	/************** Searching & matching for sequences ***************/
	
//	override def indexOfSlice[U >: E](that: GenSeq[U], from: Int): Int = this[LinearSeq].indexOfSlice(that, from)
//
//	override def lastIndexOfSlice[U >: E](that: GenSeq[U], end: Int): Int = super[LinearSeq].lastIndexOfSlice(that, end)
//
//	override def startsWith[U](that: GenSeq[U], offset: Int): Boolean = super[LinearSeq].startsWith(that, offset)
	
	@unspecialized
	override protected[this] def startsWithUnchecked(that: FitSeqLike[E, _], offset: Int): Boolean = {
		iterator.drop(offset).take(that.length) sameElements that.iterator
	}
	
	

	
	
	override def indexOfSlice[U >: E](that: GenSeq[U], from: Int): Int = defaultImpl.indexOfSlice(that, from)
	
	override def lastIndexOfSlice[U >: E](that: GenSeq[U], end: Int): Int = defaultImpl.lastIndexOfSlice(that, end)
	
	
	/************** Predicate testing and traversing methods ***************/
	
	override def foreach[@specialized(Unit) O](f: (E) => O): Unit = {
		var left = length; var link = contents
		while(left>0) { f(link.head); link=link.tail }
	}
	
	@unspecialized
	override def reverseForeach(f: (E) => Unit): Unit = inverse.foreach(f)
	
	override def foldLeft[@specialized(Fun2Vals) O](z: O)(op: (O, E) => O): O = {
		var left = length; var link = contents
		var acc = z
		while(left>0) { acc = op(acc, link.head); link = link.tail }
		acc
	}
	
	
	
	
	

	
	override protected[this] def filter(p :(E) => Boolean, value :Boolean) :FitList[E] = {
		val builder = newBuilder
		var i = 0; val l = length; var link = contents
		while (i < l) {
			val e = link.head
			if (p(e)==value) builder += e
			i += 1
		}
		builder.result()
	}
	

	
	@unspecialized
	override def reverse: FitList[E] = inverse
	
	override def inverse: FitList[E] = {
		var l = length; var tail :Link[E] = Terminus; var elem = contents
		while(l>0) {
			tail = new FullLink[E](elem.head, tail)
			elem = elem.tail
			l -= 1
		}
		new FitList[E](length, tail)
	}
	
	override def iterator: FitIterator[E] = new FitListIterator(contents, length)
	
	@unspecialized
	override def reverseIterator: FitIterator[E] = inverse.iterator
	
	override protected[this] def specializedCopy(xs: Array[E], start: Int, total: Int): Unit = {
		var i = start; val e = start+total; var l = contents
		while (i<e) {
			xs(i) = l.head
			l = l.tail
			i+=1
		}
	}
	
	
	override protected[this] def defaultImpl: SeqLike[E, FitList[E]] = new LinearSeq[E] with SeqLike[E, FitList[E]] {
		override def seq: LinearSeq[E] = FitList.this
		override def length: Int = FitList.this.length
		override def apply(idx: Int): E = at(idx)
		override protected[this] def newBuilder: mutable.Builder[E, FitList[E]] = FitList.this.newBuilder
	}
	
	override def companion: FitCompanion[FitList] = FitList
	
	
	override protected[this] def newBuilder: FitBuilder[E, FitList[E]] =
		new FitListBuilder
	
	override def seq :FitList[E] = this
	override protected[this] def thisCollection :FitList[E] = this
	protected[this] override def toCollection(repr: FitList[E]): FitList[E] = repr
	
	override protected[this] def typeStringPrefix: String = "FitList"
}






object FitList extends ImplementationIterableFactory[FitList] {

	override def empty[@specialized(Elements) E]: FitList[E] = new FitList(0, Terminus)
	
	override def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, FitList[E]] =
		new FitListBuilder[E]
	
	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, FitList[E]] = new FitListBuilder[E]

	def newReverseBuilder[@specialized(Elements) E] :FitBuilder[E, FitList[E]] = new ReverseFitListBuilder[E]
	
	
	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[FitList[_], E, FitList[E]]): CanBuildFrom[FitList[_], E, FitList[E]] =
		fit.cbf
	
	private[seqs] sealed trait Link[@specialized(Elements) +E] {
		def head :E
		def tail :Link[E]
	}
	
	private[seqs] object Terminus extends Link[Nothing] {
		override def head: Nothing = throw new NoSuchElementException("FitList().head")
		override def tail: Link[Nothing] = throw new UnsupportedOperationException("FitList().tail")
	}
	
	private[seqs] final class FullLink[@specialized(Elements) E](override val head :E, private[FitList] var t :Link[E]=Terminus) extends Link[E] {
		override def tail = t
	}
	
	
	private[seqs] class FitListIterator[@specialized(Elements) E] private[seqs]
			(private[this] final var contents :Link[E], max :Int)
		extends CountdownIterator[E](max) with FitIterator[E]
	{
		override def head: E = contents.head
		
		override def next(): E = {
			val res = contents.head
			contents = contents.tail
			limit -= 1
			res
		}
		
		override def skip(): Unit = { contents = contents.tail; limit -=1 }
		
		
		override protected[this] def specializedCopy(xs: Array[E], start: Int, len: Int): Unit = {
			val total = math.min(len, limit)
			var i = start; val e = start + total
			while(i<e) {
				xs(i) = contents.head
				contents = contents.tail
				i+=1
			}
			limit -= total
		}
	}
	
	
	private[seqs] class FitListBuilder[@specialized(Elements) E] extends FitBuilder[E, FitList[E]] {
		private[this] var length :Int=0
		private[this] var tail :FullLink[E] = new FullLink(Specialized[E].Null)
		private[this] var head :FullLink[E] = tail
		
		def count = length
		
		override def addOne :E=>Unit = { e :E =>
			val last = new FullLink(e)
			tail.t = last; tail = last
			length += 1
		}
		
		
		override def +=(elem: E): this.type = {
			val last = new FullLink(elem)
			tail.t = last
			tail = last
			length += 1
			this
		}
		
		override def result(): FitList[E] = {
			val res = head
			head = new FullLink(Specialized[E].Null)
			tail = head
			length = 0
			new FitList(length, res)
		}
			
		
		override def clear(): Unit = {
			head = new FullLink(Specialized[E].Null); tail = head; length=0
		}
	}
	
	private[seqs] class ReverseFitListBuilder[@specialized(Elements) E] extends FitBuilder[E, FitList[E]] {
		private[this] var length :Int=0
		private[this] var head :Link[E] = Terminus
		
		
		override private[specialty] def addOne :E=>Unit =
			{ e :E => head = new FullLink(e, head); length += 1 }
		
		override def +=(elem: E): this.type = {
			head = new FullLink(elem, head)
			length += 1
			this
		}
		
		override def result(): FitList[E] = new FitList[E](length, head)
		
		override def count: Int = length
		
		override def clear(): Unit = {
			length = 0; head = Terminus
		}
	}
	
}
