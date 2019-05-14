package net.turambar.palimpsest.specialty.seqs

import net.turambar.palimpsest.SerializationVersion
import net.turambar.palimpsest.specialty
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterator.BaseIterator
import net.turambar.palimpsest.specialty.seqs.LinkedList.{Empty, LinkedListBuilder, LinkedListIterator, NonEmpty, SerializedLinkedList}
import specialty._

import scala.annotation.{tailrec, unspecialized}
import scala.collection.generic.CanBuildFrom
import scala.collection.{LinearSeq, LinearSeqLike}
import RuntimeType.Fun2
import net.turambar.palimpsest.specialty.iterables.{IterableSpecialization, SpecializableIterable, SpecializedIterableFactory}
import net.turambar.palimpsest.specialty.iterables.FitIterable.{ElementDeserializer, ElementSerializer}
import net.turambar.palimpsest.specialty.seqs.FitSeq.SeqFoundation




//trait LinkedListTemplate[E] extends SeqFoundation[E]


/**
  * @author Marcin Mościcki
  */
@SerialVersionUID(SerializationVersion)
sealed trait LinkedList[@specialized(Elements) +E]
	extends SeqFoundation[E, LinkedList[E]] with LinearSeq[E] with LinearSeqLike[E, LinkedList[E]] //with SliceLike[E, LinkedList[E]]
			with FitSeq[E] with IterableSpecialization[E, LinkedList[E]] with SpecializableIterable[E, LinkedList] with Serializable
{
//	override def isEmpty = false
//	override def nonEmpty = true


	override def length :Int = {
		var len = 0; var l = this
		while(l.nonEmpty) { len += 1; l = l.tail }
		len
	}


	@unspecialized
	override protected def section(from: Int, until: Int): LinkedList[E] =
		blindDrop(from).blindTake(until)

	override private[seqs] def get(idx: Int) = blindDrop(idx).head

	override protected[this] def at(idx: Int): E = blindDrop(idx).head


	override def apply(idx: Int): E = {
		val res = drop(idx)
		if (idx<0 || res.isEmpty)
			throw new IndexOutOfBoundsException(s"LinkedList($idx)")
		res.head
	}


//	override def headOption :Option[E] =
//		if (isEmpty) None
//		else Some(head)

	override def last :E =
		if (isEmpty) throw new NoSuchElementException(s"LinkedList.Empty.last")
		else {
			var l = this; var next = tail
			while (next.nonEmpty) {
				l = next; next = next.tail
			}
			l.head
		}

	/** Exactly like [[LinkedList#drop]], but assumes `this.length >= n` and doesn't check
	  * for list termination when advancing. Intended for other sequence implementations which
	  * know this list's length.
	  */
	@unspecialized
	private[seqs] final def blindDrop(n :Int) :LinkedList[E] = {
		@tailrec def rec(l :LinkedList[E] = this, x :Int=n) :LinkedList[E] =
			if (n<=0) l
			else rec(l.tail, x-1)
		rec()
	}


	@unspecialized
	override final def drop(n :Int) :LinkedList[E] = {
		@tailrec def rec(l :LinkedList[E], x :Int) :LinkedList[E] =
			if (x<=0 || isEmpty) l
			else rec(l.tail, x-1)
		rec(this, n)
	}




	override def take(n :Int) :LinkedList[E] =
		if (n<=0 || isEmpty) LinkedList.Empty
		else {
			val res = new NonEmpty(head, tail)
			var end = res
			var end_t = tail
			var left = n-1
			while(left > 0 && end_t.nonEmpty) {
				val next = end_t.tail
				val copy = new NonEmpty(end_t.head, next)
				end.t = copy; end = copy; end_t = next
				left-=1
			}
			end.t = LinkedList.Empty
			res
		}


	private[seqs] def blindTake(n :Int) :LinkedList[E] =
		if (n<=0) LinkedList.Empty
		else {
			var end_t = tail
			val res = new NonEmpty(head, end_t)
			var end = res; var left = n-1
			while(left > 0) {
				val next = end_t.tail
				val copy = new NonEmpty(end_t.head, next)
				end.t = copy; end = copy; end_t = next
				left -= 1
			}
			end.t = LinkedList.Empty
			res
		}



	@unspecialized
	override def takeRight(n: Int) :LinkedList[E] = {
		var terminator = drop(n); var next = this
		while(terminator.nonEmpty) {
			terminator = terminator.tail; next = next.tail
		}
		next
	}


	override def dropRight(n :Int) :LinkedList[E] ={
		var terminator = drop(n)
		if (terminator.isEmpty) terminator //handles also this.isEmpty
		else {
			val res = new NonEmpty(head, tail); var end = res
			terminator = terminator.tail
			while (terminator.nonEmpty) {
				val next = end.tail
				val copy = new NonEmpty(next.head, next.tail)
				end.t = copy; end = copy
				terminator = terminator.tail
			}
			end.t = LinkedList.Empty
			res
		}
	}

	@unspecialized override def init :LinkedList[E] =
		if (isEmpty) throw new UnsupportedOperationException(s"LinkedList.Empty.init")
		else dropRight(1)


	override def splitAt(n: Int) :(LinkedList[E], LinkedList[E]) =
		if (n<=0 || isEmpty) (LinkedList.Empty, this)
		else {
			val prefix = new NonEmpty(head, tail); var end = prefix; var suffix = tail
			var left = n-1
			while(left > 0 && suffix.nonEmpty) {
				val next = suffix.tail
				val copy = new NonEmpty(suffix.head, next)
				end.t = copy; end = copy
				suffix = next
				left -= 1
			}
			end.t = LinkedList.Empty
			(prefix, suffix)
		}

	@unspecialized
	override final def slice(from :Int, until :Int) :LinkedList[E] =
		if (until <= 0 || until<=from) LinkedList.Empty
		else if (from<=0) take(until)
		else drop(from).take(until-from)

//	override protected def section(from: Int, until: Int): LinkedList[E] = slice(from, until)

	@unspecialized
	override def dropWhile(p: E => Boolean) :LinkedList[E] = {
		var l = this
		while (l.nonEmpty && p(l.head)) l = l.tail
		l
	}

	@unspecialized
	override def takeWhile(p :E=>Boolean) :LinkedList[E] = span(p)._1


	override def span(p: E => Boolean) :(LinkedList[E], LinkedList[E]) =
		if (isEmpty || !p(head)) (LinkedList.Empty, this)
		else {
			val prefix = new NonEmpty(head, tail); var end = prefix; var suffix = tail
			while(suffix.nonEmpty && p(suffix.head)) {
				val next = suffix.tail
				val copy = new NonEmpty(suffix.head, next)
				end.t = copy; end = copy
				suffix = next
			}
			end.t = LinkedList.Empty
			(prefix, suffix)
		}




	override def foreach[@specialized(Unit) U](f: (E) => U) :Unit = {
		var l = this
		while(l.nonEmpty) { f(l.head); l = l.tail }
	}

	@unspecialized
	override def reverseForeach(f :E=>Unit) :Unit = inverse.foreach(f)

	@unspecialized
	override def forall(p: E => Boolean) :Boolean = dropWhile(p).isEmpty


	override def segmentLength(p: E => Boolean, from: Int) :Int = {
		var l = drop(from); var len = 0
		while (l.nonEmpty && p(l.head)) { l = l.tail; len+=1 }
		len
	}

	override def indexWhere(p: E => Boolean, from: Int) :Int = {
		var i = math.max(from, 0); var l = drop(i)
		while(l.nonEmpty) {
			if (p(l.head)) return i
			l = l.tail; i+=1
		}
		-1
	}

	override def lastIndexWhere(p: E => Boolean, end: Int) :Int = {
		var i = 0; var last = -1; var l = this
		while(i<=end && l.nonEmpty) {
			if (p(l.head)) last = i
			i += 1; l = l.tail
		}
		last
	}

	override protected[this] def positionOf(elem: E, from: Int) :Int = {
		var i = math.max(from, 0); var l = drop(i)
		while(l.nonEmpty) {
			if (l.head == elem) return i
			l = l.tail; i+=1
		}
		-1
	}

	override protected[this] def lastPositionOf(elem: E, end: Int) :Int = {
		var i = 0; var last = -1; var l = this
		while(i<=end && l.nonEmpty) {
			if (l.head == end) last = i
			i += 1; l = l.tail
		}
		last
	}

	override def find(p: E => Boolean) :Option[E] = {
		var l = this
		while(l.nonEmpty) {
			val h = l.head
			if (p(h)) return Some(h)
			l = l.tail
		}
		None
	}

	override def find_?(p :E => Boolean, where :Boolean) : ?[E] = {
		var l = this
		while (l.nonEmpty) {
			val h = l.head
			if (p(h) == where) return Sure(h)
			l = l.tail
		}
		Blank
	}

	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) :O = {
		var res = z; var l = this
		while(l.nonEmpty) {
			res = op(res, l.head)
			l = l.tail
		}
		res
	}

	override def filter(p: E => Boolean, ourTruth: Boolean): LinkedList[E] =
		if (isEmpty) this
		else {
			val hat = new NonEmpty(head)
			var end = hat; var l = this
			do {
				if (p(l.head)) {
					val f = new NonEmpty(l.head)
					end.t = f; end = f
				}
				l = l.tail
			} while(l.nonEmpty)
			hat.t
		}


	@unspecialized
	override def +:[B >: E, That](elem: B)(implicit bf: CanBuildFrom[LinkedList[E], B, That]) :That = bf(this) match {
		case b :LinkedListBuilder[_] => //todo: is it really what we want with mutable lists?
			val h = (b += elem).result().asInstanceOf[NonEmpty[B]] //if bf is specialized it will give us a properly specialized link
			h.t = this
			h.asInstanceOf[That]
		case b =>
			(b += elem ++= this).result()
	}

	protected[this] def ::(elem :E) :NonEmpty[E] =
		new NonEmpty(elem, this)


	override def reverse :LinkedList[E] = {
		var l = this; var res = Empty :LinkedList[E]
		while(l.nonEmpty) {
			res = new NonEmpty(l.head, res)
			l = l.tail
		}
		res
	}

	@unspecialized
	override def inverse :LinkedList[E] = reverse


	protected[this] override def trustedCopyTo(xs: Array[E], start: Int, total: Int): Int = {
		var i = start; val e = start + total; var l = this
		while (i<e && l.nonEmpty) {
			xs(i) = l.head
			l = l.tail
			i+=1
		}
		i-start
	}



	override def iterator :FitIterator[E] = new LinkedListIterator(this)
	@unspecialized
	override def reverseIterator :FitIterator[E] = reverse.iterator

	override def companion :FitCompanion[LinkedList] = LinkedList
	override protected[this] def newBuilder = new LinkedListBuilder[E]

	override def typeStringPrefix = "LinkedList"

	protected[this] def writeReplace :AnyRef = new SerializedLinkedList(this)
}


//trait ValList[@specialized(Elements) E] extends ValSeq[E] with ValSeqLike[E, ValList[E]] with LinkedList[E]



object LinkedList extends SpecializedIterableFactory[LinkedList] {

	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[LinkedList[_], E, LinkedList[E]]): CanBuildFrom[LinkedList[_], E, LinkedList[E]] =
		fit.cbf

	override def empty[@specialized(Elements) E] :LinkedList[E] = new Empty[E]

	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, LinkedList[E]] =
		new LinkedListBuilder[E]

//	override def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, LinkedList[E]] =
//		new LinkedListBuilder[E]



	/** An empty, non-specialized linked list which can serve as `LinkedList[E]` for any element type `E` thanks to covariance.
	  * Note that this is '''not''' the only instance of empty [[LinkedList[E]]], so check `list==Empty` will yield false negatives!
	  * This is because in many circumstances it can be beneficial to preserve specialization of the list so it is not lost
	  * with future append operations, and some classes may empose artificial limiting of list length.
	  * Always check `list.isEmpty` or match against `Empty()` instead!
	  */
	object Empty extends Empty[Nothing] {
		def unapply[E](l :LinkedList[E]) :Boolean = l.isEmpty
		//todo: hashCode; implementation consistent with Seq.equals
		override def equals(that :Any) :Boolean = that match {
			case _ :Empty[_] => true
			case _ => false
		}

		override def canEqual(that: Any) :Boolean = that.isInstanceOf[Empty[_]]

		override def toString = "LinkedList()"
	}

	@SerialVersionUID(SerializationVersion)
	sealed class Empty[@specialized(Elements) +E] private[seqs]() extends LinkedList[E] {
		override def ofAtLeast(size: Int): Boolean = size <= 0
		override def head = throw new NoSuchElementException("LinkedList.Empty.head")
		override def tail = throw new UnsupportedOperationException("LinkedList.Empty.tail")
//		override def ensureNonEmpty[U >: E](msg: String): NonEmpty[U] = throw new NoSuchElementException(msg)
	}


	object NonEmpty {
		def unapply[E](list :LinkedList[E]) :Option[(E, LinkedList[E])] =
			if (list.isEmpty) None
			else Some((list.head, list.tail))

		private[seqs] def shouldExist[E](link :LinkedList[E]) :NonEmpty[E] = link match {
			case exists :NonEmpty[E] => exists
			case _ => throw new NoSuchElementException("LinkedList.Empty where non-empty link was expected.")
		}

		private[seqs] def shouldExist[E](link :LinkedList[E], msg :String) :NonEmpty[E] = link match {
			case exists :NonEmpty[E] => exists
			case _ => throw new NoSuchElementException(msg)
		}
	}

	/** Non empty link of a linked list; used both as the basic building block of [[LinkedList]] itself as well as the underlying buffer
	  * for various other, higher level `Seq` implementations.
	  */
	@SerialVersionUID(SerializationVersion)
	private[seqs] final class NonEmpty[@specialized(Elements) E] (private[seqs] var x :E, private[seqs] var t :LinkedList[E]=LinkedList.Empty)
		extends LinkedList[E]
	{
		override def head = x
		override def tail = t
		override def ::(elem :E) :NonEmpty[E] = new NonEmpty(elem, this)

		override def isEmpty = false
		override def nonEmpty = true
		override def ofAtLeast(size: Int): Boolean = {
			@tailrec def count(l :LinkedList[E], left :Int) :Boolean =
				left <= 0 || l.nonEmpty && count(l.tail, left-1)
			size <= 1 || count(tail, size-1)
		}
	}


	private[seqs] object Mutable {
		@tailrec private[seqs] final def shiftLeft[@specialized(Elements) E](to :LinkedList[E], from :LinkedList[E], left :Int) : Unit =
			if (left>0) {
				NonEmpty.shouldExist(to).x = NonEmpty.shouldExist(from).x
				shiftLeft(to.tail, from.tail, left-1)
			}

		@tailrec private[seqs] final def shiftRight[@specialized(Elements) E](from :NonEmpty[E], left :Int) :LinkedList[E] =
			if (left<=0) from
			else {
				val next = NonEmpty.shouldExist(from.tail); next.x = from.x
				shiftRight(next, left-1)
			}

	}



	private[seqs] class LinkedListIterator[@specialized(Elements) E](private[this] var hd :LinkedList[E])
		extends BaseIterator[E] with FitIterator[E]
	{
		def head :E = hd.head
		override def hasNext :Boolean = hd.nonEmpty
		override def next() :E = { val res = hd.head; hd=hd.tail; res }
		override def skip() :Unit = { hd = hd.tail }

	}

	private[seqs] class LinkedListBuilder[@specialized(Elements) E] extends FitBuilder[E, LinkedList[E]] {
		private[LinkedList] val hat = new NonEmpty[Any](null)
		private[this] var coccyx = hat.asInstanceOf[NonEmpty[E]]

		override def +=(elem: E): this.type = {
			val next = new NonEmpty(elem)
			coccyx.t = next; coccyx = next
			this
		}

		@unspecialized
		override def result(): LinkedList[E] = {
			coccyx = hat.asInstanceOf[NonEmpty[E]]
			val res = coccyx.t; hat.t = Empty
			res
		}

		override def clear(): Unit = {
			coccyx = hat.asInstanceOf[NonEmpty[E]]; hat.t = Empty
		}

	}


	@SerialVersionUID(100)
	class SerializedLinkedList[@specialized(Elements) E](@transient private var hd :LinkedList[E]) extends Serializable {
		import java.io.{ObjectOutputStream=>OS, ObjectInputStream=>IS}

		private def writeObject(os :OS) :Unit = writeList(os, hd)

		private def writeList(os :OS, list :LinkedList[E]) :Unit = {
			os.defaultWriteObject()
			val serializer = ElementSerializer[E]()
			var reversed = Empty :LinkedList[E]; var length = 0
			var l = list
			while (l.nonEmpty) {
				reversed = new NonEmpty(l.head, reversed)
				length+=1; l = l.tail
			}
			os.writeInt(length)
			while(length>0) {
				serializer(os, reversed.head)
				reversed = reversed.tail; length-=1
			}
		}

		private def readObject(is :IS) :Unit = hd = readList(is)

		private def readList(is :IS) :LinkedList[E] = {
			is.defaultReadObject()
			val deserializer = ElementDeserializer[E]()
			var res = Empty :LinkedList[E]
			var length = is.readInt
			while(length>0) {
				res = new NonEmpty(deserializer(is), res)
				length -= 1
			}
			res
		}

		private def readResolve :AnyRef = hd

	}
}
