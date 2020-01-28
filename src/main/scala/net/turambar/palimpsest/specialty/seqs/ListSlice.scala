package net.turambar.palimpsest.specialty.seqs

import java.lang.Math

import net.turambar.palimpsest.SerializationVersion
import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.seqs.FitSeq.SeqFoundation
import net.turambar.palimpsest.specialty._
import net.turambar.palimpsest.specialty.seqs.LinkedList.{Empty, NonEmpty}

import scala.annotation.unspecialized
import scala.collection.LinearSeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.LinearSeq
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.Fun2
import net.turambar.palimpsest.specialty.iterables.FitIterable.{ElementDeserializer, ElementSerializer}
import net.turambar.palimpsest.specialty.iterators.CountdownIterator
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.iterables.{CloneableIterable, IterableSpecialization, SpecializableIterable, SpecializableIterableFactory, StableIterableTemplate}
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.seqs.ListSlice.{ListSliceBuilder, ListSliceIterator, SerializedListSlice}

/**
  * @author Marcin Mościcki
  */
@SerialVersionUID(SerializationVersion)
class ListSlice[@specialized(ItemTypes) +E] private[seqs] (start :LinkedList[E], end :LinkedList[E], override val length :Int)
	extends SeqFoundation[E, ListSlice[E]] with LinearSeq[E] with LinearSeqLike[E, ListSlice[E]]
	   with StableSeq[E] with IterableSpecialization[E, ListSlice[E]] with SliceLike[E, ListSlice[E]]
	   with SpecializableIterable[E, ListSlice]
	   with StableIterableTemplate[E, ListSlice[E]] with CloneableIterable[E, ListSlice[E]] with OfKnownSize
{
	private[seqs] def firstLink = start
	private[seqs] def lastLink = end

//	override def nonEmpty :Boolean = length>0
//	override def isEmpty :Boolean = length==0
//	override def hasFastSize = true


	override protected[this] def at(idx: Int): E = start.get(idx)

	override protected def section(from: Int, until: Int): ListSlice[E] =
		if (until==length)
			new ListSlice(start.blindDrop(from), end, until-from)
		else
			new ListSlice(start.blindDrop(from), Empty, until-from)

	override def head :E =
		if (length>0) start.head
		else throw new NoSuchElementException(s"$stringPrefix().head")

	override def head_? : ?[E] =
		if (length>0) Sure(start.head)
		else Blank

	override def headOption :Option[E] =
		if (length>0) Some(start.head)
		else None

	override def last :E = //todo: remember the result?
		if (length==0) throw new NoSuchElementException(s"$stringPrefix().last")
		else if (end.nonEmpty) end.head
		else start.get(length-1)


	override def last_? : ?[E] =
		if (length==0) Blank
		else if (end.nonEmpty) end.head_?
		else Sure(start.get(length-1))

	override def lastOption :Option[E] =
		if (length==0) None
		else end.headOption orElse Some(start.get(length-1))

	override def tail :ListSlice[E] =
		if (length==0) throw new UnsupportedOperationException(s"$stringPrefix().tail")
		else new ListSlice(start, end, length-1)



	override def takeWhile(p: E => Boolean) :ListSlice[E] = {
		var last :LinkedList[E] = Empty
		var l :LinkedList[E] = start
		var left = length
		while(left>0 && p(l.head)) {
			left -= 1; last = l; l = l.tail
		}
		new ListSlice(start, last, length-left)
	}

	override def dropWhile(p: E => Boolean) :ListSlice[E] = {
		var l = start; var left = length
		while (left>0 && p(l.head)) {
			left -= 1; l = l.tail
		}
		new ListSlice(l, end, left)
	}



	override def span(p: E => Boolean) :(ListSlice[E], ListSlice[E]) = {
		val prefix = takeWhile(p)
		if (prefix.lastLink.isEmpty) (prefix, ListSlice.Empty) //we could use this instead of prefix
		else (prefix, new ListSlice(prefix.lastLink.tail, end, length-prefix.length))
	}


	override def splitAt(n: Int) :(ListSlice[E], ListSlice[E]) =
		if (n<=0) (ListSlice.Empty, this)
		else if (n>=length) (this, ListSlice.Empty)
		else {
			val cut = start.drop(n-1)
			(new ListSlice(start, cut, n), new ListSlice(cut.tail, end, length-n))
		}



	override def segmentLength(p: E => Boolean, from: Int) :Int =
		if (from>=length) 0
		else {
			var l = start.blindDrop(from); var count = 0; val lim = length - Math.max(from, 0)
			while(count<lim && p(l.head)) {
				count += 1; l = l.tail
			}
			count
		}

	override def indexWhere(p: E => Boolean, from: Int) :Int =
		if (from>=length)
			-1
		else if (from==length-1 && end.nonEmpty)
			if (p(end.head)) from else -1
		else {
			var i = Math.max(from, 0); var l = start.blindDrop(i); val lim = length
			while(i<lim) {
				if (p(l.head))
					return i
				i += 1; l = l.tail
			}
			-1
		}

	override def lastIndexWhere(p: E => Boolean, from: Int) :Int = {
//		if (from>=length-1 && end.nonEmpty && p(end.head))
//			return length-1 //we should check if this.isEmpty first!
		var last = -1; var l = start; var i = 0; val lim = Math.min(from, length-1)
		while (i<=lim) {
			if (p(l.head)) last = i
			i += 1; l = l.tail
		}
		last //todo: we could update coccyx here
	}

	override protected[this] def offsetOf(elem: E, from: Int) :Int =
		if (from>=length) -1
		else if (from==length-1 && end.nonEmpty)
			if (end.head==elem) from else -1
		else {
			var i = Math.max(from, 0); var l = start.blindDrop(i); val lim = length
			while(i<lim) {
				if (l.head==elem)
					return i
				i += 1; l = l.tail
			}
			-1
		}

	override protected[this] def lastOffsetOf(elem: E, from: Int) :Int = {
//		if (from>=length-1 && end.nonEmpty && end.head==elem)
//			return length-1 //we should check if this.isEmpty first!
		var last = -1; var l = start; var i = 0; val lim = Math.min(from, length-1)
		while(i<=lim) {
			if (l.head==elem)
				last = i
			i += 1; l = l.tail
		}
		last //todo: we could update coccyx here
	}

	override def find(p: E => Boolean) :Option[E] = {
		var l = start; var left = length
		while(left>0) {
			val e = l.head
			if (p(e))
				return Some(e)
			left -= 1; l = l.tail
		}
		None
	}

	override def find_?(p :E => Boolean, where :Boolean) : ?[E] = {
		var l = start; var left = length
		while (left > 0) {
			val e = l.head
			if (p(e) == where)
				return Sure(e)
			left -= 1; l = l.tail
		}
		Blank
	}


	override def foreach[@specialized(Unit) O](f: E => O) :Unit = {
		var left = length; var l = start
		while(left>0) {
			f(l.head); l = l.tail; left-=1
		}
	}

	@unspecialized
	override def reverseForeach(f: E => Unit) :Unit = inverse.foreach(f)

	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) :O = {
		var res = z; var left = length; var l = start
		while(left>0) {
			res = op(res, l.head)
			l = l.tail; left -= 1
		}
		res
	}

	override def filter(p: E => Boolean, ourTruth: Boolean) :ListSlice[E] =
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

	override def reverse :ListSlice[E] =
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

	@unspecialized override def inverse :ListSlice[E] = reverse


	override def clone() :ListSlice[E] =
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


	override protected[this] def trustedCopyTo(xs: Array[E], start: Int, total: Int) :Int = {
		var i = start; val count = Math.min(total, length); val end = start + count
		var l = this.start
		while (i < end) { xs(i) = l.head; i+=1; l = l.tail }
		count
	}

	override def iterator :FitIterator[E] = new ListSliceIterator(start, length)
	
	@unspecialized override def reverseIterator :FitIterator[E] = reverse.iterator

	override def typeStringPrefix = "ListSlice"
	
	override def companion = ListSlice

	override protected[this] def newBuilder = new ListSliceBuilder()

	protected[this] def writeReplace :AnyRef = new SerializedListSlice(this)
}





object ListSlice extends SpecializableIterableFactory[ListSlice] {
	final val Empty = empty[Nothing]


	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ListSlice[_], E, ListSlice[E]]): CanBuildFrom[ListSlice[_], E, ListSlice[E]] =
		fit.cbf


	override def newBuilder[@specialized(ItemTypes) E]: FitBuilder[E, ListSlice[E]] =
		new ListSliceBuilder

//	override def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, ListSlice[E]] =
//		new ListSliceBuilder(new NonEmpty(Specialized[E].Null))
	
	override def empty[@specialized(ItemTypes) E] :ListSlice[E] =
		new ListSlice[E](LinkedList.Empty, LinkedList.Empty, 0)


	private[seqs] class ListSliceIterator[@specialized(ItemTypes) +E](private[this] var hd :LinkedList[E], max :Int)
		extends CountdownIterator[E](max) with FitIterator[E] 
	{
		override def head: E = hd.head

		override def next(): E = { val res = hd.head; hd = hd.tail; limit-=1; res;  }

		override def skip(): Unit = { hd = hd.tail; limit -= 1 }

		@unspecialized override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int) :Unit =
			if (start<0)
				throw new IllegalArgumentException(s"$this.copyToArray([], $start, $len)")
			else
				hd.copyToArray(xs, start, Math.min(len, xs.length-start))
	}
	
	private[seqs] class ListSliceBuilder[@specialized(ItemTypes) E](
			private var hat :NonEmpty[E], private var coccyx :NonEmpty[E])
		extends FitBuilder[E, ListSlice[E]] 
	{
		def this(handle :NonEmpty[E] = new NonEmpty(RuntimeType[E].default)) =
			this(handle, handle)

		private[this] var length = 0


		override private[specialty] def addOne = { e :E =>
			val next = new NonEmpty(e)
			coccyx.t = next; coccyx = next; length += 1
		}

		def ++=(xs :ListSlice[E]) :this.type = {
			//todo
			xs.lastLink match {
				case known :NonEmpty[E] => known synchronized {
					if (known.t.isEmpty) {
						known.t = hat //block anyone else from appending to that slice
						coccyx.t = xs.firstLink
						coccyx = known
						length += xs.length
						xs
					}
				}
				case _ => this ++= xs.firstLink.take(xs.length)
			}
			this

		}

		override def ++=(xs: FitTraversableOnce[E]) :ListSliceBuilder.this.type = xs match {
			case _ if xs.isEmpty => this
				
			case l :ListSlice[E] => ++=(l)
//				end.t = tail.firstLink
//				end = tail.lastLink
//				length += tail.length
				
			case _ => 
				val it = xs.toIterator
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

	}
	
	private[seqs] class ReverseListSliceBuilder[@specialized(ItemTypes) E](
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

	}




	@SerialVersionUID(100)
	private[ListSlice] class SerializedListSlice[@specialized(ItemTypes) E](@transient private var list :ListSlice[E]) extends Serializable {
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
