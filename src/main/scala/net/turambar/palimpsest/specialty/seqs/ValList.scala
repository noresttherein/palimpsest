package net.turambar.palimpsest.specialty.seqs

import net.turambar
import net.turambar.palimpsest
import net.turambar.palimpsest.specialty
import net.turambar.palimpsest.specialty._
import net.turambar.palimpsest.specialty.iterables.{EmptyIterableTemplate, SpecializableIterable, SpecializedIterableFactory}
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.seqs.ValList.{Link, ListBuilder, ListIterator}
import net.turambar.palimpsest.specialty.RuntimeType.Fun2

import scala.annotation.{tailrec, unspecialized}
import scala.collection.immutable.LinearSeq
import scala.collection.LinearSeqLike
import scala.collection.generic.CanBuildFrom

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed trait ValList[@specialized(Elements) E]
	extends LinearSeq[E] with LinearSeqLike[E, ValList[E]] with StableSeq[E]
	   with ValSeq[E] with ValSeqLike[E, ValList[E]] with SpecializableIterable[E, ValList]
{
	override protected def section(from :Int, until :Int) :ValList[E] = drop(from).take(until-from)


	override protected def at(idx :Int) :E = {
		var next :ValList[E] = this; var left = idx
		while(left > 0 && next.nonEmpty) {
			next = next.tail; left -= 1
		}
		if (next.isEmpty)
			throw new IndexOutOfBoundsException(String.valueOf(idx))
		next.head
	}

	override def apply(idx :Int) :E =
		if (idx < 0)
			throw new IndexOutOfBoundsException(s"ValList($idx)")
		else at(idx)


	override def positionOf(elem :E, from :Int) :Int = {
		var i = 0; var next = this
		while (next.nonEmpty && i < from) {
			next = next.tail
			i += 1
		}
		while (next.nonEmpty) {
			if (next.head == elem)
				return i
			next = next.tail
			i += 1
		}
		-1
	}


	override def lastPositionOf(elem :E, end :Int) :Int = {
		var i = 0; var next = this; var last = -1
		while (i <= end && next.nonEmpty) {
			if (elem == next.head)
				last = i
			next = next.tail; i += 1
		}
		last
	}


	override def prefixLength(p :E=>Boolean) :Int = {
		var i = 0; var next = this
		while (next.nonEmpty && p(next.head)) {
			next = next.tail; i+=1
		}
		i
	}

	@unspecialized
	override def segmentLength(p :E=>Boolean, from :Int) :Int = drop(from).prefixLength(p)

	override def indexWhere(p :E => Boolean, from :Int) :Int = {
		var i = 0; var next = this
		while (next.nonEmpty && i < from) {
			next = next.tail; i += 1
		}
		while (next.nonEmpty) {
			if (p(next.head))
				return i
			next = next.tail; i += 1
		}
		-1
	}

	override def lastIndexWhere(p :E => Boolean, end :Int) :Int = {
		var i = 0; var next = this; var last = -1
		while (i <= end && next.nonEmpty) {
			if (p(next.head))
				last = i
			next = next.tail; i += 1
		}
		last
	}



	/** Prepends an element to this list, as with standard scala lists. */
	def ::(elem :E) :ValList[E] = new Link(elem, this)

	override def +:[U >: E, That](elem :U)(implicit cbf :CanBuildFrom[ValList[E], U, That]) :That = cbf match {
		case cff :CanFitFrom[ValList[E], U, That] if cff.honorsBuilderFrom && (runtimeType.boxType isAssignableFrom elem.getClass) =>
			(elem.asInstanceOf[E]::this).asInstanceOf[That]
		case _ => (cbf(this) += elem ++= this).result()
	}


	override def %:(elem :E) :ValList[E] = ::(elem)

	override def :%(elem :E) :ValList[E] = super.:%(elem)



	override def clone() :ValList[E] = this


	override def iterator :FitIterator[E] = new ListIterator(this)

	override def newBuilder :FitBuilder[E, ValList[E]] = ValList.newBuilder

	override def companion :FitCompanion[ValList] = ValList
}



object ValList extends SpecializedIterableFactory[ValList] {

	override implicit def canBuildFrom[E](implicit fit :CanFitFrom[ValList[_], E, ValList[E]]) :CanBuildFrom[ValList[_], E, ValList[E]] = fit.cbf


	override def empty[@specialized(Elements) E] :ValList[E] = new EmptyList[E]


	override def newBuilder[@specialized(Elements) E] :FitBuilder[E, ValList[E]] = ListBuilder()





	private final class Link[@specialized(Elements) E](override val head :E, private[this] var t :ValList[E]) extends ValList[E] {

		@inline private[ValList] def tail_=(list :ValList[E]) :Unit = t = list

		override def isEmpty = false

		override def length :Int = {
			var l = tail; var res = 1
			while (l.nonEmpty) {
				l = l.tail; res += 1
			}
			res
		}

		override def ofAtLeast(n :Int) :Boolean = {
			var l :ValList[E] = this; var count = n
			while(count > 0 && l.nonEmpty) {
				l = l.tail; count -= 1
			}
			count <= 0
		}

		override def head_? : ?[E] = Sure(head)

		override def headOption :Option[E] = Some(head)

		override def tail :ValList[E] = t

		override def last: E = {
			@tailrec def rec(l :ValList[E]) :E = {
				val t = l.tail
				if (t.isEmpty) l.head
				else rec(t)
			}
			rec(this)
		}

		override def last_? : ?[E] = Sure(last)

		override def lastOption :Option[E] = Some(last)

		override def init :ValList[E] =
			if (tail.isEmpty)
				empty[E]
			else {
				val nil = empty[E]
				val first = new Link(head, nil)
				var next = tail; var last = first
				do {
					val l = new Link(next.head, nil)
					last.tail = l; last = l
					next = next.tail
				} while (next.nonEmpty)
				first
			}



		override def drop(n :Int) :ValList[E] = {
			@tailrec def rec(l :ValList[E], left :Int) :ValList[E] =
				if (l.isEmpty || left <= 0) l
				else rec(l.tail, left-1)
			rec(this, n)
		}

		override def take(n :Int) :ValList[E] =
			if (n <= 0) empty[E]
			else {
				val nil = empty[E]
				val first = new Link(head, nil); var last = first
				var left = n-1; var next = tail
				while(left > 0 && next.nonEmpty) {
					val l = new Link(next.head, nil)
					last.tail = l; last = l
					next = next.tail; left -= 1
				}
				first
			}


		override def slice(start :Int, end :Int) :ValList[E] =
			if (start <= 0) take(end)
			else if (end <= start) empty[E]
			else drop(start).take(end-start)


		override def splitAt(idx :Int) :(ValList[E], ValList[E]) =
			if (idx<=0) (empty[E], this)
			else {
				val nil = empty[E]
				val first = new Link(head, nil); var last = first
				var left = idx-1; var next = tail
				while(left > 0 && next.nonEmpty) {
					val l = new Link(next.head, nil)
					last.tail = l; last = l
					next = next.tail; left -= 1
				}
				(first, next)
			}


		override def dropWhile(p :E => Boolean) :ValList[E] = {
			@tailrec def rec(l :ValList[E]) :ValList[E] =
				if (l.nonEmpty && p(l.head)) rec(l.tail)
				else l
			rec(this)
		}


		override def takeWhile(p :E => Boolean) :ValList[E] =
			if (!p(head))
				empty[E]
			else {
				val nil = empty[E]
				val first = new Link(head, nil); var last = first
				var next = tail
				while (next.nonEmpty && p(next.head)) {
					val l = new Link(next.head, nil)
					last.tail = l; last = l
					next = next.tail
				}
				first
			}


		override def find_?(p :E => Boolean, where :Boolean) : ?[E] = {
			@tailrec def rec(l :ValList[E]): ?[E] =
				if (l.isEmpty) Blank
				else if (p(l.head) == where) Sure(l.head)
				else rec(l.tail)
			rec(this)
		}


		override def filter(p :E => Boolean, where :Boolean): ValList[E] = {
			val nil = empty[E]
			val first = new Link(head, nil); var last = first
			var next = this :ValList[E]
			do {
				val e = next.head
				if (p(e) == where) {
					val l = new Link(e, nil)
					last.tail = l; last = l
				}
				next = next.tail
			} while(next.nonEmpty)
			first.tail
		}


		override def partition(p :E => Boolean): (ValList[E], ValList[E]) = {
			val nil = empty[E]
			val yes = new Link(head, nil); var lastyes = yes
			val no = new Link(head, nil); var lastno = no
			var next = this :ValList[E]
			do {
				val e = next.head
				val l = new Link(e, nil)
				if (p(e)) {
					lastyes.tail = l; lastyes = l
				} else {
					lastno.tail = l; lastno = l
				}
				next = next.tail
			} while (next.nonEmpty)
			(yes.tail, no.tail)
		}

		override def foreach[@specialized(Unit) U](f :E=>U) :Unit = {
			var next :ValList[E] = this
			do {
				f(next.head)
				next = next.tail
			} while(next.nonEmpty)
		}

		override def reverseForeach(f :E=>Unit) :Unit =
			reverse.foreach(f)


		override def foldLeft[@specialized(Fun2) O](z :O)(op :(O, E) => O) :O = {
			var next :ValList[E] = this
			var acc = z
			do {
				acc = op(acc, next.head)
				next = next.tail
			} while(next.nonEmpty)
			acc
		}


		override def -(elem :E) :ValList[E] =
			if (head==elem)
				tail
			else {
				val nil = empty[E]
				val first = new Link(head, nil); var last = first
				var next = tail
				while (next.nonEmpty) {
					if (next.head == elem) {
						last.tail = next.tail
						return first
					}
					val l = new Link(next.head, nil)
					last.tail = l; last = l
					next = next.tail
				}
				next
			}


		override protected def trustedCopyTo(xs :Array[E], start :Int, total :Int) :Int = {
			var i = start; val end = i + total
			var next :ValList[E] = this
			do {
				xs(i) = next.head
				next = next.tail; i += 1
			} while(i < end && next.nonEmpty)
			i - start
		}


		override def reverse :ValList[E] = {
			var last = empty[E]
			var next = tail
			do {
				last = new Link(next.head, last)
				next = next.tail
			} while (next.nonEmpty)
			last
		}


	}




	private class EmptyList[@specialized(Elements) E] extends EmptyIterableTemplate[E, ValList[E]] with ValList[E] {
		override def length = 0

//		/** Target of `apply` for internal use, assuming the index is valid (faster). */
//		override protected def at(idx :Int) :E = throw new IndexOutOfBoundsException(s"ValList.empty($idx)")

//		/** Create a slice of this instance assuming the indices are already validated. Delegated to from [[slice]] and other subsequence methods. */
//		override protected def section(from :Int, until :Int) :ValList[E] = this

		override def -(elem :E) :ValList[E] = this
	}






	private class ListIterator[@specialized(Elements) E](private[this] var list :ValList[E]) extends FitIterator[E] {
		override def head :E = list.head

		override def next() :E = { val res = list.head; list = list.tail; res }

		override def hasNext :Boolean = list.isEmpty

		override def skip() :Unit = list = list.tail
	}





	private class ListBuilder[@specialized(Elements) E](dummy :E, nil :ValList[E] = empty[E]) extends FitBuilder[E, ValList[E]] {
		private[this] val hat :Link[E] = new Link(dummy, nil)
		private[this] var last = hat

		override def +=(elem :E) :this.type = {
			val l = new Link(elem, nil)
			last.tail = l; last = l
			this
		}

		override def result() :ValList[E] = { val res = hat.tail; hat.tail = nil; last = hat; res }


		override def clear() :Unit = { hat.tail = nil; last = hat }

		override def typeHint[L <: E](implicit specialization :RuntimeType[L]) :FitBuilder[E, ValList[E]] = ListBuilder()
	}



	object ListBuilder extends Specialize[SpecializedBuilder] {
		override def specialized[@specialized E](implicit s :RuntimeType[E]) :SpecializedBuilder[E] =
			new ListBuilder[E](s.default)
	}
}
