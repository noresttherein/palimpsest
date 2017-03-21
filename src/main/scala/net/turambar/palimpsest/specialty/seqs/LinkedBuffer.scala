package net.turambar.palimpsest.specialty.seqs

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.seqs.LinkedList.{Empty, LinkedListIterator, NonEmpty}
import net.turambar.palimpsest.specialty.seqs.ListSlice.ListSliceIterator
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterator, FitTraversableOnce, ImplementationIterableFactory, IterableSpecialization, SpecializableIterable, Specialized}

import scala.annotation.{tailrec, unspecialized}
import scala.collection.generic.CanBuildFrom
import scala.collection.{LinearSeqLike, mutable}
import scala.compat.Platform.ConcurrentModificationException


/**
  * @author Marcin Mościcki
  */
class LinkedBuffer[@specialized(Elements) E] private[seqs] (
		hat :NonEmpty[E],
		private var coccyx :NonEmpty[E],
		private[this] var len :Int)
	extends mutable.LinearSeq[E] with LinearSeqLike[E, LinkedBuffer[E]]
			with FitBuffer[E] with ValSeqLike[E, LinkedBuffer[E]] //MutableSliceLike[E, LinkedBuffer[E]]
			with SpecializableIterable[E, LinkedBuffer] with OfKnownSize
{ //todo: extend slicelike?

	@inline final override def length: Int = len
	@inline final override def isEmpty = length==0
	@inline final override def nonEmpty = length>0
	@inline final override def hasFastSize = true
//	override def ofAtLeast(items :Int) = hat.drop(items).nonEmpty

	@unspecialized @inline
	final protected[this] def ff(n :Int) :NonEmpty[E] = hat.blindDrop(n+1) match {
		case present :NonEmpty[E] => present
		case _ => throw new NoSuchElementException(s"$stringPrefix<$length>($n)")
	}

	@unspecialized @inline
	final protected[this] def node(n :Int) :NonEmpty[E] =
		if (n<0 || n>=length)
			throw new IndexOutOfBoundsException(s"$stringPrefix<$length>($n)")
		else hat.blindDrop(n+1) match {
			case present :NonEmpty[E] => present
			case _ => throw new NoSuchElementException(s"$stringPrefix<$length>($n)")
		}

	@unspecialized @inline
	final protected[this] def preceding(idx :Int) :NonEmpty[E] =
		if (idx<0 || idx>=length)
			throw new IndexOutOfBoundsException(s"$stringPrefix<$length>($idx)")
		else hat.blindDrop(idx) match {
			case present :NonEmpty[E] => present
			case _ => throw new NoSuchElementException(s"$stringPrefix<$length>(${idx-1})")
		}


	override protected def section(from: Int, until: Int): LinkedBuffer[E] = ???

	override def overwrite(start: Int, length: Int): FitBuffer[E] = ???

//	@unspecialized
//	private[seqs] def firstLink = hat.t
//	@unspecialized
//	private[seqs] def lastLink = coccyx

	override def head = hat.t.head
	override def headOption = hat.t.headOption

	override def last =
		if (length>0) coccyx.x
		else throw new NoSuchElementException(s"$stringPrefix<$length>.last")

	override def lastOption =
		if (length>0) Some(coccyx.x)
		else None

	override def tail =
		if (length==0) throw new UnsupportedOperationException(s"$stringPrefix().tail")
		else new LinkedBuffer(hat.tail.asInstanceOf[NonEmpty[E]], coccyx, len-1)


	override protected[this] def at(idx: Int): E = hat.get(idx+1)

	override protected[this] def set(idx: Int, elem: E): Unit = ff(idx).x = elem

	override def update(idx: Int, elem: E): Unit =
		if (idx<0 || idx>=len)
			throw new IndexOutOfBoundsException(idx.toString)
		else if (idx==len-1)
			coccyx.x = elem
		else ff(idx).x = elem


	override def update(idx: Int, elems: TraversableOnce[E]): Unit = elems match {
		case _ if idx<0 =>
			throw new IndexOutOfBoundsException(s"$stringPrefix<$length>($idx)")
		case _ if idx>=length || elems.isEmpty => ()

		case FitIterator(it) => update(idx, it)

		case _ if idx == len-1 =>
			coccyx.x = elems.toIterator.next()

		case _ =>
			var i = idx; var n = hat.blindDrop(i+1)
			val it = elems.toIterator
			while (it.hasNext && i<length) {
				n.asInstanceOf[NonEmpty[E]].x = it.next()
				n = n.tail
				i += 1
			}

	}

	override def update(idx: Int, elems: FitTraversableOnce[E]) :Unit =
		if (idx < len && elems.nonEmpty)
			if (idx<0)
				throw new IndexOutOfBoundsException(s"$stringPrefix<$length>.update($idx, ...)")
			else if (idx==len-1)
				coccyx.x = elems.head
			else {
				var i = idx; var n = hat.blindDrop(i+1); val limit = len
				val it = elems.fitIterator
				while (it.hasNext && i < limit) {
					n.asInstanceOf[NonEmpty[E]].x = it.next()
					n = n.tail
					i += 1
				}
			}

	override def update(fromIndex: Int, value: E, count: Int): Unit =
		if (fromIndex < length && count>0)
			if (fromIndex<0)
				throw new IndexOutOfBoundsException(s"$stringPrefix<$length>.update($fromIndex, ???, $count)")
			else if (fromIndex==len-1)
				coccyx.x = value
			else {
				var left = math.min(count, length-fromIndex)
				var n = hat.blindDrop(fromIndex)
				do {
					n.asInstanceOf[NonEmpty[E]].x = value; left -= 1
					n = n.tail
				} while(left>0)
			}




	override def +=(elem: E): this.type = {
		val last = new NonEmpty(elem)
		coccyx.t = last; coccyx = last; len+=1
		this
	}

	override def ++=(elems: FitTraversableOnce[E]): this.type = { //todo: optimize for LinkedList, ListSlice
		val it = elems.toIterator
		var last = coccyx
		while(it.hasNext) {
			val next = new NonEmpty(it.next())
			last.t = next; last = next
			len += 1
		}
		coccyx = last
		this
	}

	override def +=:(elem: E): this.type = {
		val l = new NonEmpty(elem, hat.t)
		hat.t = l
		if (length==0)
			coccyx = l
		len += 1
		this
	}


	override def -=(x: E) = {
		var left = length; var prev = hat; var cur = prev.tail
		while(left>0 & cur.head!=x) {
			left -= 1; prev = cur.asInstanceOf[NonEmpty[E]]; cur = cur.tail
		}
		if (left==1) {
			len -= 1; coccyx = prev; prev.t = Empty
		} else if (left>0) {
			cur = cur.tail; prev.t = cur
		}
		this
	}

//	override def -=(elem1: E, elem2: E, elems: E*) = super.-=(elem1, elem2, elems)

	override def insertAll(idx: Int, elems: Traversable[E]): Unit =  elems match {
		case fit :FitTraversableOnce[E] => insertAll(idx, fit :FitTraversableOnce[E])

		case _ if idx<0 || idx > length =>
			throw new IndexOutOfBoundsException(s"LinkedBuffer<$length>($idx)")
		case _ if idx==length =>
			this ++= elems
		case _ if elems.isEmpty => ()

		case FitIterator(it) => insertAll(idx, it) //for wrapped arrays

		case _ => insertAll(idx, FitIterator.adapt(elems.toIterator)) //to create specialized links
	}

	def insertAll(idx :Int, elems :FitTraversableOnce[E]) :Unit =
		if (idx<0 || idx>length)
			throw new IndexOutOfBoundsException(s"$stringPrefix<$length>.insertAll($idx, ...)")
		else if (idx==length)
			this ++= elems
		else if (elems.nonEmpty) {
			val it = elems.fitIterator
			var last = ff(idx-1); val after = last.tail
			while (it.hasNext) {
				val next = new NonEmpty(it.next())
				last.t = next; last = next
				len += 1
			}
			last.t = after
		}


	override def remove(n: Int): E = {
		val l = preceding(n)
		val res = l.head
		if (n<length-1)
			l.t = l.t.tail
		else {
			l.t = LinkedList.Empty
			coccyx = l
		}
		len -= 1
		res
	}

	override def remove(n: Int, count: Int) :Unit =
		if (count<0)
			throw new IllegalArgumentException(s"$stringPrefix<$length>.remove($n, $count)")
		else if (n<0 || n > len-count)
			throw new IndexOutOfBoundsException(s"$stringPrefix<$length>.remove($n, $count)")
		else if (count>0)
			if (n + count == len) {
				coccyx = NonEmpty.shouldExist(hat.blindDrop(n))
				coccyx.t = Empty
				len -= count
			} else {
				val prev = NonEmpty.shouldExist(hat.blindDrop(n))
				val next = prev.blindDrop(count+1)
				prev.t = next
				len -= count
			}

	override def clear(): Unit = {
		hat.t = Empty; coccyx = hat; len = 0
	}



	override protected[this] def filter(p: (E) => Boolean, ourTruth: Boolean): LinkedBuffer[E] = {
		val res = new NonEmpty(hat.x); var last = res; var count = 0
		var cur = hat.tail; var left = len
		while(left > 0) {
			if (p(cur.head)==ourTruth) {
				val next = new NonEmpty(cur.head)
				last.t = next; last = next; count += 1
			}
			left -= 1; cur = cur.tail
		}
		new LinkedBuffer(res, last, count)
	}

	override def iterator :FitIterator[E] = new ListSliceIterator(hat.tail, length)

	override def reverseIterator = inverse.iterator

	override def seq :LinkedBuffer[E] = this


	override def typeStringPrefix = "LinkedBuffer"

	override def companion = LinkedBuffer

//	override def newBuilder = new
}



object LinkedBuffer extends ImplementationIterableFactory[LinkedBuffer] {

	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[LinkedBuffer[_], E, LinkedBuffer[E]]): CanBuildFrom[LinkedBuffer[_], E, LinkedBuffer[E]] =
		fit.cbf

	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, LinkedBuffer[E]] = ???

	override def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, LinkedBuffer[E]] = ???



/*
	private class LinkedFiller[@specialized(Elements) E](
			hat :NonEmpty[E],
			private var coccyx :NonEmpty[E],
			private[this] var len :Int,
			limit :Int)
		extends mutable.LinearSeq[E] with LinearSeqLike[E, LinkedFiller[E]]
				with FitBuffer[E] with MutableSliceLike[E, LinkedFiller[E]]
				with SpecializableTraversableTemplate[E, LinkedFiller]
	{
		override def length = len
		override def hasFastSize = true
		override def isEmpty = length==0
		override def nonEmpty = length>0

		override protected[this] def at(idx: Int): E = hat.get(idx+1)

		override protected[this] def set(idx: Int, elem: E): Unit = shouldNotBeEmpty(hat.drop(idx+1)).x = elem

		@unspecialized @inline
		final protected[this] def ff(n :Int) :NonEmpty[E] = hat.blindDrop(n+1).asInstanceOf[NonEmpty[E]]

		@tailrec private def shiftLeft(to :LinkedList[E], from :LinkedList[E], left :Int) : Unit =
			if (left>0) {
				shouldNotBeEmpty(to).x = shouldNotBeEmpty(from).x
				shiftLeft(to.tail, from.tail, left-1)
			}

		@tailrec private def shiftRight(from :NonEmpty[E], left :Int) :LinkedList[E] =
			if (left<=0) from
			else {
				val next = shouldNotBeEmpty(from.tail); next.x = from.x
				shiftRight(next, left-1)
			}


		override protected def section(from: Int, until: Int): LinkedFiller[E] = ???
		override def overwrite(start: Int, length: Int): FitBuffer[E] = ???

		override def update(idx: Int, elem: E) :Unit =
			if (idx < 0 || idx>=length)
				throw new IndexOutOfBoundsException(s"$stringPrefix<$len>.update($idx, ???)")
			else if (idx==length-1)
				coccyx.x = elem
			else
				shouldNotBeEmpty(hat.blindDrop(idx+1)).x = elem


		override def update(fromIndex: Int, value: E, count: Int): Unit =
			if (count>0)
				if (fromIndex<0 || fromIndex>=length)
					throw new IndexOutOfBoundsException(s"$stringPrefix<$len>.update($fromIndex, ???, $count)")
				else if (fromIndex==len-1) {
					coccyx.x = value
				} else {
					var left = math.min(count, len-fromIndex); var l = hat.blindDrop(fromIndex+1)
					while(left > 0) {
						shouldNotBeEmpty(l).x = value; l = l.tail; left -= 1
					}
				}

		override def update(idx: Int, elems: TraversableOnce[E]): Unit = elems match {
			case _ if idx >= length || elems.isEmpty => ()
			case _ if idx<0 =>
				throw new IndexOutOfBoundsException(s"$stringPrefix<$len>.update($idx, ...)")

			case fit :FitTraversableOnce[E] => update(idx, fit :FitTraversableOnce[E])

			case FitIterator(it) => update(idx, it)

			case _ if idx==len-1 => coccyx.x = elems.toIterator.next()

		}

		override def update(idx: Int, elems: FitTraversableOnce[E]) =
			if (elems.nonEmpty && idx<len)
				if (idx<0)
					throw new IndexOutOfBoundsException(s"$stringPrefix<$len>.update($idx, ...)")
				else if (idx==len-1)
					coccyx.x = elems.head
				else {
					var l = hat.blindDrop(idx+1); val it = elems.toIterator; var left=len-idx
					while(it.hasNext && left>0) {
						shouldNotBeEmpty(l).x = it.next()
						l = l.tail
					}
				}

		override def +=(elem: E): this.type =
			if (len>=limit)
				overLimit
			else {
				val n = shouldNotBeEmpty(coccyx.tail)
				n.x = elem; len += 1; coccyx = n
				this
			}

		override def +=:(elem: E): this.type =
			if (len==0) this += elem
			else if (len>=limit)
				overLimit
			else  {
				//todo
//				shiftForwardCopy(start.tail, shouldNotBeEmpty(start.tail).tail, len)
				coccyx = shouldNotBeEmpty(coccyx.tail)
				len += 1
				this
			}

		override def ++=:(xs: TraversableOnce[E]) = super.++=:(xs)

		override def remove(n: Int): E =
			if (n<0 || n>=len)
				throw new IndexOutOfBoundsException(s"$stringPrefix<$length>.remove($n)")
			else hat.blindDrop(n+1) match {
				case rem :NonEmpty[E] =>
					val res = rem.x
					shiftLeft(rem, rem.tail, len-n-1)
					len -= 1; res
				case _ => concurrentMod
			}


		override def remove(n: Int, count: Int) :Unit =
			if (count > 0)
				if (n<0 || n>=len)
					throw new IndexOutOfBoundsException(s"$stringPrefix<$length>.remove($n, $count)")
				else if (n+count >= len) {
					len = n
					coccyx = shouldNotBeEmpty(hat.blindDrop(n))
				} else {
					val to = shouldNotBeEmpty(hat.blindDrop(n+1))
					val from = to.blindDrop(count)
					shiftLeft(to, from, length-n-count)
				}

		override def clear(): Unit = {
			coccyx = hat; len = 0
		}

		override def insertAll(n: Int, elems: Traversable[E]): Unit = ???






		override protected[this] def filter(p: (E) => Boolean, ourTruth: Boolean): LinkedFiller[E] = ???

		private final def concurrentMod =
			throw new NoSuchElementException("FitBuffer.overwrite: underlying buffer shrunk in concurrent modification.")
//			throw new ConcurrentModificationException("FitBuffer.overwrite: underlying buffer shrunk.")

		private final def shouldNotBeEmpty(link :LinkedList[E]) :NonEmpty[E] = link match {
			case n :NonEmpty[E] => n
			case _ => concurrentMod
		}

		private final def overLimit =
			throw new UnsupportedOperationException(s"FitBuffer.overwrite reached max capacity $limit.")


		override def seq :LinkedFiller[E] = this

	}
*/

}
