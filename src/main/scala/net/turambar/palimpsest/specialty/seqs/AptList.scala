package net.turambar.palimpsest.specialty.seqs

import scala.annotation.{tailrec, unspecialized}
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.LinearSeq
import scala.collection.{mutable, GenSeq, LinearSeqLike, SeqLike}
import net.turambar.palimpsest.specialty.iterables.AptCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.{CloneableIterable, AptCompanion, IterableSpecialization, SpecializableIterable, SpecializableIterableFactory, StableIterableTemplate}
import net.turambar.palimpsest.specialty.iterators.{CountdownIterator, AptIterator}
import net.turambar.palimpsest.specialty.seqs.AptList.{AptListBuilder, AptListIterator, FullLink, Link, Terminus}
import net.turambar.palimpsest.specialty.seqs.AptSeq.SeqFoundation
import net.turambar.palimpsest.specialty._
import net.turambar.palimpsest.specialty.Vals.OfKnownSize
import net.turambar.palimpsest.specialty.RuntimeType.Specialized




/** Specialized linked list with O(1) `length` and O(1) `take` operations.
  * Random indexing and `drop` still take O(n), though.
  * In line with [[AptSeq]] philosophy, slicing tries to share the contents and return view on
  * the parent sequence which will prevent 'dropped' tails from garbage collection -
  * make a copy if you want to store one for longer and suspect it's only a fragment of a larger structure.
  *
  * @author Marcin Mościcki
  */
class AptList[@specialized(ItemTypes) +E] private[seqs](
		override val length :Int,
		start :Link[E]
	) extends SeqFoundation[E, AptList[E]] with LinearSeq[E] with LinearSeqLike[E, AptList[E]]
	     with IterableSpecialization[E, AptList[E]] with StableSeq[E] with SliceLike[E, AptList[E]]
	     with SpecializableIterable[E, AptList]
	     with StableIterableTemplate[E, AptList[E]] with CloneableIterable[E, AptList[E]] with OfKnownSize
{
	import Specialized.Fun2Vals

//	override def isEmpty :Boolean = length == 0
//	override def nonEmpty :Boolean = length > 0
//	override def hasFastSize = true



	@tailrec private[this] final def dropped(n :Int, l :Link[E]=start) :Link[E] =
		if (n<=0) l
		else dropped(n-1, l.tail)

	override protected def section(from: Int, until: Int): AptList[E] =
		new AptList(until - from, dropped(from))

	override protected[this] def empty: AptList[E] = AptList.Empty

	override protected[this] def at(idx: Int): E = dropped(idx).head
//	override def apply(idx :Int) :E =
//		if (idx<0 || idx>=length) throw new IndexOutOfBoundsException(idx.toString)
//		else dropped(idx).head


	override def head: E =
		if (length>0) start.head
		else throw new NoSuchElementException(s"AptList().head")

	override def head_? : ?[E] =
		if (length > 0) Sure(start.head)
		else Blank

	override def headOption :Option[E] =
		if (length>0) Some(start.head)
		else None

	override def last :E =
		if (length>0) dropped(length-1).head
		else throw new NoSuchElementException(s"AptList().last")

	override def lastOption :Option[E] =
		if (length>0) Some(dropped(length-1).head)
		else None

	/************** Slicing methods ***************/

	override def tail :AptList[E] =
		if (length==0) throw new UnsupportedOperationException(s"AptList().tail")
		else new AptList(length-1, start.tail)


	override def dropWhile(p: E => Boolean): AptList[E] = {
		var len=length; var elems=start
		while(len>0 && p(elems.head)) { len-=1; elems=elems.tail }
		new AptList(len, elems)
	}






	/************** Searching for an element methods  ***************/

	override def segmentLength(p: E => Boolean, from: Int): Int =
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



	override def find(p: E => Boolean): Option[E] = {
		var len = length; var elem = start
		while(len>0 && !p(elem.head)) {
			len -= 1; elem = elem.tail
		}
		if (len>0) Some(elem.head)
		else None
	}


	override def find_?(p :E => Boolean, where :Boolean) : ?[E] = {
		var len = length; var elem = start
		while (len > 0 && p(elem.head) != where) {
			len -= 1; elem = elem.tail
		}
		if (len > 0) Sure(elem.head)
		else Blank
	}

	override def lastIndexWhere(p: E => Boolean, from: Int): Int = {
		var i =0; var l = start; var last = -1; val e = math.min(from, length)
		while(i<e) {
			if (p(l.head))
				last = i
			i += 1
			l = l.tail
		}
		last
	}


	override protected[this] def offsetOf(elem: E, from: Int): Int =
		if (from>=length) -1
		else {
			var i = math.max(from, 0); var l = dropped(i)
			while (i<length && l.head != elem) {
				i+=1; l=l.tail
			}
			if (i==length) -1 else i
		}

	override protected[this] def lastOffsetOf(elem: E, end: Int): Int = {
		var i = 0; var l = start; var last = -1; val e = math.min(end, length)
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

//	@unspecialized
//	override protected[this] def startsWithUnchecked(that: FitSeqLike[E, _], offset: Int): Boolean = {
//		iterator.drop(offset).take(that.length) sameElements that.iterator
//	}





//	override def indexOfSlice[U >: E](that: GenSeq[U], from: Int): Int = defaultImpl.indexOfSlice(that, from)
//
//	override def lastIndexOfSlice[U >: E](that: GenSeq[U], end: Int): Int = defaultImpl.lastIndexOfSlice(that, end)


	/************** Predicate testing and traversing methods ***************/

	override def foreach[@specialized(Unit) O](f: E => O): Unit = {
		var left = length; var link = start
		while(left>0) { f(link.head); link=link.tail; left -= 1 }
	}

	@unspecialized
	override def reverseForeach(f: E => Unit): Unit = inverse.foreach(f)

	override def foldLeft[@specialized(Fun2Vals) O](z: O)(op: (O, E) => O): O = {
		var left = length; var link = start
		var acc = z
		while(left>0) { acc = op(acc, link.head); link = link.tail }
		acc
	}







	override def filter(p :E => Boolean, value :Boolean) :AptList[E] = {
		val builder = newBuilder
		var i = 0; val l = length; var link = start
		while (i < l) {
			val e = link.head
			if (p(e)==value) builder += e
			i += 1
			link = link.tail
		}
		builder.result()
	}



	@unspecialized
	override def reverse: AptList[E] = inverse

	override def inverse: AptList[E] = {
		var l = length; var tail :Link[E] = Terminus; var elem = start
		while(l>0) {
			tail = new FullLink[E](elem.head, tail)
			elem = elem.tail
			l -= 1
		}
		new AptList[E](length, tail)
	}

	override def iterator: AptIterator[E] = new AptListIterator(start, length)

	@unspecialized
	override def reverseIterator: AptIterator[E] = inverse.iterator



	protected[this] override def trustedCopyTo(xs: Array[E], start: Int, total: Int): Int = {
		var i = start; var l = this.start
		val count = math.min(total, length); val e = start + count
		while (i<e) {
			xs(i) = l.head
			l = l.tail
			i+=1
		}
		count
	}
	
	
//	override protected[this] def defaultImpl: SeqLike[E, AptList[E]] = new LinearSeq[E] with SeqLike[E, AptList[E]] {
//		override def seq: LinearSeq[E] = AptList.this
//		override def length: Int = AptList.this.length
//		override def apply(idx: Int): E = AptList.this.apply(idx)
//		override protected[this] def newBuilder: mutable.Builder[E, AptList[E]] = AptList.this.newBuilder
//	}
	
	override def companion: AptCompanion[AptList] = AptList
	
	
	override protected[this] def newBuilder: AptBuilder[E, AptList[E]] =
		new AptListBuilder
	
	override def seq :AptList[E] = this
	override protected[this] def thisCollection :AptList[E] = this
	protected[this] override def toCollection(repr: AptList[E]): AptList[E] = repr
	
	override protected[this] def typeStringPrefix: String = "AptList"
}






object AptList extends SpecializableIterableFactory[AptList] {

	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[AptList[_], E, AptList[E]]): CanBuildFrom[AptList[_], E, AptList[E]] =
		fit.cbf


	final val Empty = empty[Nothing]

	override def empty[@specialized(ItemTypes) E]: AptList[E] = new AptList(0, Terminus)
	
//	override def specializedBuilder[@specialized(Elements) E: Specialized]: AptBuilder[E, AptList[E]] =
//		new AptListBuilder[E]
	
	override def newBuilder[@specialized(ItemTypes) E]: AptBuilder[E, AptList[E]] = new AptListBuilder[E]

	def newReverseBuilder[@specialized(ItemTypes) E] :AptBuilder[E, AptList[E]] = new ReverseAptListBuilder[E]

	def reverseBuilder[E :RuntimeType] :ReverseAptListBuilder[E] = ReverseBuilder()

	private[this] final val ReverseBuilder = new Specialize[ReverseAptListBuilder] {
		override def specialized[@specialized E : RuntimeType] = new ReverseAptListBuilder[E]
	}


	private[seqs] sealed trait Link[@specialized(ItemTypes) +E] {
		def head :E
		def tail :Link[E]
	}

	private[seqs] object Terminus extends Link[Nothing] {
		override def head: Nothing = throw new NoSuchElementException("AptList().head")
		override def tail: Link[Nothing] = throw new UnsupportedOperationException("AptList().tail")
	}

	private[seqs] final class FullLink[@specialized(ItemTypes) E](override val head :E, private[AptList] var t :Link[E]=Terminus) extends Link[E] {
		override def tail = t
	}


	private[seqs] class AptListIterator[@specialized(ItemTypes) E] private[seqs]
			(private[this] final var contents :Link[E], max :Int)
		extends CountdownIterator[E](max) with AptIterator[E]
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


	private[seqs] class AptListBuilder[@specialized(ItemTypes) E] extends AptBuilder[E, AptList[E]] {
		private[this] var length :Int=0
		private[this] var tail :FullLink[E] = new FullLink(RuntimeType[E].default)
		private[this] var head :FullLink[E] = tail


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

		override def result(): AptList[E] = {
			val res = head
			head = new FullLink(RuntimeType[E].default)
			tail = head
			length = 0
			new AptList(length, res)
		}


		override def clear(): Unit = {
			head = new FullLink(RuntimeType[E].default); tail = head; length=0
		}
	}

	private[seqs] class ReverseAptListBuilder[@specialized(ItemTypes) E] extends AptBuilder[E, AptList[E]] {
		private[this] var length :Int=0
		private[this] var head :Link[E] = Terminus
		
		
		override private[specialty] def addOne :E=>Unit =
			{ e :E => head = new FullLink(e, head); length += 1 }
		
		override def +=(elem: E): this.type = {
			head = new FullLink(elem, head)
			length += 1
			this
		}
		
		override def result(): AptList[E] = new AptList[E](length, head)
		
		override def clear(): Unit = {
			length = 0; head = Terminus
		}
	}
	
}
