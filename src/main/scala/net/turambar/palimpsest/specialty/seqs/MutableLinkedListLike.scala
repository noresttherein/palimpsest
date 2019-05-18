package net.turambar.palimpsest.specialty.seqs


import net.turambar.palimpsest.specialty.{ofKnownSize, Elements, FitTraversableOnce}
import net.turambar.palimpsest.specialty.seqs.LinkedList.{Empty, NonEmpty}

import scala.annotation.{tailrec, unspecialized}
import scala.collection.{GenTraversableOnce, mutable}

/**
  * @author Marcin Mościcki
  */
trait MutableLinkedListLike[@specialized(Elements) E, +Repr<:MutableLinkedListLike[E, Repr] with MutableSeq[E]]
	extends mutable.SeqLike[E, Repr] with ValSeqLike[E, Repr]
{ //this :MutableSeq[E] =>


	protected def hat :NonEmpty[E]
	protected def coccyx :NonEmpty[E]
	protected[this] var len :Int

	@inline final override def length = len
	@inline final override def isEmpty = len==0
	@inline final override def nonEmpty = len>0
	@inline final override def hasFastSize = true

	@unspecialized @inline
	private final def shouldNotBeEmpty(link :LinkedList[E]) :NonEmpty[E] = link match {
		case n :NonEmpty[E] => n
		case _ => throw new NoSuchElementException(s"$stringPrefix<$length>: list shrunk due to concurrent modification")
	}

	@unspecialized @inline
	final protected[this] def ff(n :Int) :NonEmpty[E] = hat.blindDrop(n+1) match {
		case present :NonEmpty[E] => present
		case _ => throw new NoSuchElementException(s"$stringPrefix<$length>($n)")
	}

/*
	@unspecialized @inline
	final protected[this] def keyNode(n :Int) :NonEmpty[E] =
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
*/



	protected[this] def newCollection(beforeFirst :NonEmpty[E], last :NonEmpty[E], size :Int) :Repr

//	@unspecialized
//	override protected def section(from: Int, until: Int): Repr =
//		if (until==len)
//			newCollection(shouldNotBeEmpty(hat.blindDrop(from)), coccyx, until-from)
//		else {
//			val first = hat.blindDrop(from); val last = first.blindDrop(until-from)
//			newCollection(shouldNotBeEmpty(first), shouldNotBeEmpty(last), until-length)
//		}


//	override def overwrite(start: Int, length: Int): FitBuffer[E] = ???
//
//
//	override protected[this] def at(idx: Int): E = hat.get(idx+1)
//
//	override protected[this] def set(idx: Int, elem: E): Unit = ff(idx).x = elem



	override def head :E =
		if (len>0) hat.t.head
		else throw new NoSuchElementException(s"$stringPrefix().head")

	override def headOption :Option[E] =
		if (len>0) Some(hat.t.head)
		else None

	override def last :E =
		if (len>0) coccyx.x
		else throw new NoSuchElementException(s"$stringPrefix<$length>.last")

	override def lastOption :Option[E] =
		if (len>0) Some(coccyx.x)
		else None

//	override def tail =
//		if (length==0) throw new UnsupportedOperationException(s"$stringPrefix().tail")
//		else new LinkedBuffer(hat.tail.asInstanceOf[NonEmpty[E]], coccyx, len-1)


	override def apply(idx :Int) :E =
		if (idx<0 || idx>=len)
			throw new IndexOutOfBoundsException(s"$stringPrefix<$length>($idx)")
		else if (idx==len-1) coccyx.x
		else hat.blindDrop(idx+1).head

	override def update(idx: Int, elem: E): Unit =
		if (idx<0 || idx>=len)
			throw new IndexOutOfBoundsException(idx.toString)
		else if (idx==len-1)
			coccyx.x = elem
		else ff(idx).x = elem


/*
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
				val it = elems.toIterator
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
				var cur = hat.blindDrop(fromIndex)
				do {
					cur.asInstanceOf[NonEmpty[E]].x = value; left -= 1
					cur = cur.tail
				} while(left>0)
			}

*/



	override def filter(p: E => Boolean, ourTruth: Boolean): Repr = {
		val res = new NonEmpty(hat.x); var last = res; var count = 0
		var cur = hat.tail; var left = len
		while(left > 0) {
			if (p(cur.head)==ourTruth) {
				val next = new NonEmpty(cur.head)
				last.t = next; last = next; count += 1
			}
			left -= 1; cur = cur.tail
		}
		newCollection(res, last, count)
	}


	override def -(elem: E) = {
		val res = new NonEmpty(hat.x); var end = res
		var cur = hat.tail; var left = len; var removed = 0
		while(left>0) {
			val e = cur.head
			left -= 1; cur = cur.tail
			if (e!=elem) {
				val next = new NonEmpty(e)
				end.t = next; end = next
			} else {
				removed = 1
				while(left>0) {
					val next = new NonEmpty(cur.head)
					end.t = next; end = next; cur = cur.tail; left-=1
				}
			}
		}
		newCollection(res, end, length-removed)
	}

	override protected[seqs] def diff(elems1: FitSeq[E], elems2: GenTraversableOnce[E]) :Repr = {
		val counts = mutable.Map[E, Int]()
		if (elems1.hasFastSize && ofKnownSize(elems2))
			counts.sizeHint(elems1.length + elems2.size)
		def countDuplicates(e :E) :Unit = counts(e) = counts.getOrElse(e, 0) + 1
		elems1 traverse countDuplicates
		elems2 foreach countDuplicates

		filter( e  => counts.getOrElse(e, 0) match {
				case 0 => true
				case n => counts(e) = n-1; false
			}, ourTruth = true
		)
	}
}
