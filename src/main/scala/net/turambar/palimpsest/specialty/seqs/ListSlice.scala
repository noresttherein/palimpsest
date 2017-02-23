package net.turambar.palimpsest.specialty.seqs

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.seqs.FitSeq.SeqFoundation
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterable, FitIterator, FitTraversableOnce, ImplementationIterableFactory, SpecializableIterable, Specialized}
import net.turambar.palimpsest.specialty.seqs.LinkedList.{Empty, NonEmpty}

import scala.annotation.unspecialized
import scala.collection.LinearSeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.LinearSeq
import Specialized.Fun2
import net.turambar.palimpsest.specialty.FitIterable.{ElementDeserializer, ElementSerializer}
import net.turambar.palimpsest.specialty.FitIterator.CountdownIterator
import net.turambar.palimpsest.specialty.seqs.ListSlice.{ListSliceBuilder, ListSliceIterator, SerializedListSlice}

/**
  * @author Marcin Mościcki
  */
@SerialVersionUID(100)
class ListSlice[@specialized(Elements) +E] private[seqs] (start :LinkedList[E], end :LinkedList[E], override val length :Int)
	extends SeqFoundation[E, ListSlice[E]] with LinearSeq[E] with LinearSeqLike[E, ListSlice[E]]
			with StableSeq[E] with SliceLike[E, ListSlice[E]] with SpecializableIterable[E, ListSlice]
{
	private[seqs] def firstLink = start
	private[seqs] def lastLink = end

	override def nonEmpty = length>0
	override def isEmpty = length==0
	override def hasFastSize = true


	override protected[this] def at(idx: Int): E = start.get(idx)

	override protected def section(from: Int, until: Int): ListSlice[E] =
		if (until==length)
			new ListSlice(start.blindDrop(from), end, until-from)
		else
			new ListSlice(start.blindDrop(from), Empty, until-from)

	override def head =
		if (length>0) start.head
		else throw new NoSuchElementException(s"$stringPrefix().head")

	override def headOption =
		if (length>0) Some(start.head)
		else None

	override def last = //todo: remember the result?
		if (length==0) throw new NoSuchElementException(s"$stringPrefix().last")
		else if (end.nonEmpty) end.head
		else start.get(length-1)

	override def lastOption =
		if (length==0) None
		else end.headOption orElse Some(start.get(length-1))

	override def tail =
		if (length==0) throw new UnsupportedOperationException(s"$stringPrefix().tail")
		else new ListSlice(start, end, length-1)



	override def takeWhile(p: (E) => Boolean) = {
		var last :LinkedList[E] = Empty
		var l :LinkedList[E] = start
		var left = length
		while(left>0 && p(l.head)) {
			left -= 1; last = l; l = l.tail
		}
		new ListSlice(start, last, length-left)
	}

	override def dropWhile(p: (E) => Boolean) = {
		var l = start; var left = length
		while (left>0 && p(l.head)) {
			left -= 1; l = l.tail
		}
		new ListSlice(l, end, left)
	}



	override def span(p: (E) => Boolean) = {
		val prefix = takeWhile(p)
		if (prefix.lastLink.isEmpty) (prefix, ListSlice.Empty) //we could use this instead of prefix
		else (prefix, new ListSlice(prefix.lastLink.tail, end, length-prefix.length))
	}


	override def splitAt(n: Int) =
		if (n<=0) (ListSlice.Empty, this)
		else if (n>=length) (this, ListSlice.Empty)
		else {
			val cut = start.drop(n-1)
			(new ListSlice(start, cut, n), new ListSlice(cut.tail, end, length-n))
		}



	override def segmentLength(p: (E) => Boolean, from: Int) =
		if (from>=length) 0
		else {
			var l = start.blindDrop(from); var count = 0; val lim = length - math.max(from, 0)
			while(count<lim && p(l.head)) {
				count += 1; l = l.tail
			}
			count
		}

	override def indexWhere(p: (E) => Boolean, from: Int) :Int =
		if (from>=length)
			-1
		else if (from==length-1 && end.nonEmpty)
			if (p(end.head)) from else -1
		else {
			var i = math.max(from, 0); var l = start.blindDrop(i); val lim = length
			while(i<lim) {
				if (p(l.head))
					return i
				i += 1; l = l.tail
			}
			-1
		}

	override def lastIndexWhere(p: (E) => Boolean, from: Int) :Int = {
//		if (from>=length-1 && end.nonEmpty && p(end.head))
//			return length-1 //we should check if this.isEmpty first!
		var last = -1; var l = start; var i = 0; val lim = math.min(from, length-1)
		while (i<=lim) {
			if (p(l.head)) last = i
			i += 1; l = l.tail
		}
		last //todo: we could update coccyx here
	}

	override protected[this] def positionOf(elem: E, from: Int) :Int =
		if (from>=length) -1
		else if (from==length-1 && end.nonEmpty)
			if (end.head==elem) from else -1
		else {
			var i = math.max(from, 0); var l = start.blindDrop(i); val lim = length
			while(i<lim) {
				if (l.head==elem)
					return i
				i += 1; l = l.tail
			}
			-1
		}

	override protected[this] def lastPositionOf(elem: E, from: Int) :Int = {
//		if (from>=length-1 && end.nonEmpty && end.head==elem)
//			return length-1 //we should check if this.isEmpty first!
		var last = -1; var l = start; var i = 0; val lim = math.min(from, length-1)
		while(i<=lim) {
			if (l.head==elem)
				last = i
			i += 1; l = l.tail
		}
		last //todo: we could update coccyx here
	}

	override def find(p: (E) => Boolean) :Option[E] = {
		var l = start; var left = length
		while(left>0) {
			val e = l.head
			if (p(e))
				return Some(e)
			left -= 1; l = l.tail
		}
		None
	}

	override def foreach[@specialized(Unit) O](f: (E) => O) :Unit = {
		var left = length; var l = start
		while(left>0) {
			f(l.head); l = l.tail; left-=1
		}
	}

	@unspecialized
	override def reverseForeach(f: (E) => Unit) = inverse.foreach(f)

	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) = {
		var res = z; var left = length; var l = start
		while(left>0) {
			res = op(res, l.head)
			l = l.tail; left -= 1
		}
		res
	}

	override protected[this] def filter(p: (E) => Boolean, ourTruth: Boolean) =
		if (isEmpty) this
		else {
			val b = new ListSliceBuilder(new NonEmpty(start.head))
			var l = start; var left = length
			do {
				val e = l.head
				if (p(e)==ourTruth) b += e
				left -= 1; l = l.tail
			} while (left > 0)
			b.result()
		}

	override def reverse =
		if (length==0) this
		else {
			var res :LinkedList[E] = new NonEmpty(start.head); val last = res
			var l = start.tail; var left = length-1
			while (left>0) {
				res = new NonEmpty(l.head, res)
				l = l.tail; left-=1
			}
			new ListSlice(res, last, length)
		}

	@unspecialized override def inverse = reverse


	override def clone() =
		if (isEmpty) this
		else {
			val res = new NonEmpty(start.head); var last = res
			var l = start.tail; var left = length-1
			while(left > 0) {
				val n = new NonEmpty(l.head)
				last.t = n; last = n
				left -= 1; l = l.tail
			}
			new ListSlice(res, end, length)
		}


	override protected[this] def specializedCopy(xs: Array[E], start: Int, total: Int) :Unit = {
		var i = start; val end = start + math.min(total, length)
		var l = this.start
		while (i < end) { xs(i) = l.head; i+=1; l = l.tail }
	}

	override def iterator = new ListSliceIterator(start, length)
	
	@unspecialized override def reverseIterator = reverse.iterator

	override def typeStringPrefix = "ListSlice"
	
	override def companion = ListSlice

	override protected[this] def newBuilder = new ListSliceBuilder()

	protected[this] def writeReplace :AnyRef = new SerializedListSlice(this)
}





object ListSlice extends ImplementationIterableFactory[ListSlice] {

	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ListSlice[_], E, ListSlice[E]]): CanBuildFrom[ListSlice[_], E, ListSlice[E]] =
		fit.cbf

	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, ListSlice[E]] = 
		new ListSliceBuilder

	override def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, ListSlice[E]] =
		new ListSliceBuilder(new NonEmpty(Specialized[E].Null))
	
	override def empty[@specialized(Elements) E] :ListSlice[E] =
		new ListSlice[E](LinkedList.Empty, LinkedList.Empty, 0)


	private[seqs] class ListSliceIterator[@specialized(Elements) +E](private[this] var hd :LinkedList[E], max :Int)
		extends CountdownIterator[E](max) with FitIterator[E] 
	{
		override def head: E = hd.head

		override def next(): E = { val res = hd.head; hd = hd.tail; limit-=1; res;  }

		override def skip(): Unit = { hd = hd.tail; limit -= 1 }

		@unspecialized override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int) =
			if (start<0)
				throw new IllegalArgumentException(s"$this.copyToArray([], $start, $len)")
			else
				hd.copyToArray(xs, start, math.min(len, xs.length-start))
	}
	
	private[seqs] class ListSliceBuilder[@specialized(Elements) E](
			private var hat :NonEmpty[E], private var coccyx :NonEmpty[E])
		extends FitBuilder[E, ListSlice[E]] 
	{
		def this(handle :NonEmpty[E] = new NonEmpty(Specialized[E].Null)) =
			this(handle, handle)

		private[this] var length = 0


		override private[specialty] def addOne = { e :E =>
			val next = new NonEmpty(e)
			coccyx.t = next; coccyx = next; length += 1
		}

		def ++=(xs :ListSlice[E]) :this.type = {
			val tail = xs.lastLink match {
				case known :NonEmpty[E] => known synchronized {
					if (known.t.isEmpty) {
						known.t = hat //block anyone else from appending to that slice
						coccyx.t = xs.firstLink
						coccyx = known
						length += xs.length
						xs
					}
				}
				case _ => xs.firstLink.take(xs.length)
			}
			this

		}

		override def ++=(xs: FitTraversableOnce[E]) = xs match {
			case _ if xs.isEmpty => this
				
			case l :ListSlice[E] => ++=(l)
//				end.t = tail.firstLink
//				end = tail.lastLink
//				length += tail.length
				
			case _ => 
				val it = xs.fitIterator
				while(it.hasNext) { 
					val next = new NonEmpty(it.next())
					coccyx.t = next; coccyx = next; length += 1
				}
				this
		}

		override def +=(elem: E): this.type = {
			val next = new NonEmpty(elem)
			coccyx.t = next; coccyx = next; length += 1
			this
		}

		override def clear(): Unit = {
			hat.t = LinkedList.Empty; coccyx = hat; length = 0
		}

		override def result(): ListSlice[E] = {
			val res = new ListSlice(hat.t, if (length==0) LinkedList.Empty else coccyx, length)
			hat.t = LinkedList.Empty; coccyx = hat; length = 0
			res
		}

		override def count: Int = length
		
	}
	
	private[seqs] class ReverseListSliceBuilder[@specialized(Elements) E](
			private[this] var res :LinkedList[E]=LinkedList.Empty, 
			private[this] var length :Int=0) 
		extends FitBuilder[E, ListSlice[E]] 
	{
		
		override def +=(elem: E): this.type = {
			res = new NonEmpty(elem, res)
			length += 1
			this
		}

		override def clear(): Unit = {
			res = LinkedList.Empty; length = 0
		}

		override def result(): ListSlice[E] = {
			val l = new ListSlice(res, LinkedList.Empty, length)
			res = LinkedList.Empty
			length = 0
			l
		} 

		override def count: Int = length
	}




	@SerialVersionUID(100)
	private[ListSlice] class SerializedListSlice[@specialized(Elements) E](@transient private var list :ListSlice[E]) extends Serializable {
		import java.io.{ObjectOutputStream=>OS, ObjectInputStream=>IS}

		private def writeObject(os :OS) :Unit = writeList(os, list)

		//extracted for specialization
		private def writeList(os :OS, list :ListSlice[E]) :Unit = {
			os.defaultWriteObject()
			val writer = ElementSerializer[E]()
			var length = list.length; var hd = list.firstLink
			os.writeInt(length)
			while(length>0) {
				writer(os, hd.head)
				length-=1
			}
		}

		private def readObject(is :IS) :Unit = list = readList(is)

		//extracted for specialization
		private def readList(is :IS) :ListSlice[E] = {
			is.defaultReadObject()
			val reader = ElementDeserializer[E]()
			val length = is.readInt
			if (length==0) ListSlice.empty[E]
			else {
				val first = new NonEmpty(reader(is)); var last = first; var left = length
				while(left > 0) {
					val next = new NonEmpty(reader(is))
					last.t = next; last=next; left-=1
				}
				new ListSlice(first, last, length)
			}
		}

		private def readResolve :AnyRef = list
	}
}