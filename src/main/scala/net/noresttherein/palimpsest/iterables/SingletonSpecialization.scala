package net.noresttherein.palimpsest.iterables

import net.noresttherein.palimpsest.Vals.OfKnownSize
import net.noresttherein.palimpsest.{?, Blank, ItemTypes, Sure}
import net.noresttherein.palimpsest.RuntimeType.Specialized.{Fun1Res, Fun1Vals, Fun2}
import net.noresttherein.palimpsest.seqs.{AptSeq, StableSeq}
import net.noresttherein.palimpsest.iterables.AptCompanion.CanFitFrom
import net.noresttherein.palimpsest.iterators.AptIterator

import scala.collection.{mutable, GenIterable, GenTraversableOnce, LinearSeq}
import scala.collection.generic.CanBuildFrom


trait SingletonTemplate[+E, +Repr] extends IterableTemplate[E, Repr] with OfKnownSize {

	/** Applies the given function to the first element in this collection and returns the result.
	  * Useful especially in non-specialized caller context, where `f(head)` would result in boxing, while
	  * the implementation will likely be specialized. While benefit in case of a single element will
	  * likely be minimal, the alternative of invoking `head` directly would still need a virtual method call.
	  * While generally useful, it is defined here to resolve conflicts between independent definition of this
	  * method by several mixins. Subclasses which actually use this method - in particular singleton implementations -
	  * would do well to provide specialized implementations.
	  * @return value returned by `f` for `head`
	  * @throws `NoSuchElementException` if this collection is empty
	  * @see [[net.noresttherein.palimpsest.iterables.SingletonFoundation]]
	  * @see [[net.noresttherein.palimpsest.iterables.SingletonSpecialization]]
	  */
	protected[this] def forHead[@specialized(Boolean, Unit) O](f :E=>O) :O //= f(head)


	protected def empty :Repr
	override def size = 1
	override def isEmpty = false
	override def nonEmpty = true
	override def tail :Repr = empty
	override def init :Repr = empty
	/** Returns  the single element of this collection as an unspecialized reference object. Override this if you want it to be specialized. */
	override def last :E = head
	override def headOption :Option[E] = Some(head)
	override def lastOption :Option[E]  = Some(head)
	override def hasFastSize = true
	override def hasDefiniteSize: Boolean = true
	override def ofAtLeast(items :Int) :Boolean = items <= 1

//	protected[this] override def forHead[@specialized(Fun1Res) O](f :E=>O) :O = f(head)
	override def foreach[@specialized(Unit) U](f :E=>U) :Unit = forHead(f) //f(head)
	override protected def reverseForeach(f: E => Unit): Unit = forHead(f) //foreach(f) //f(head)
	override def forall(p: E => Boolean) :Boolean = forHead(p) //p(head)
	override def exists(p: E => Boolean) :Boolean = forHead(p)

	override def find(p: E => Boolean) :Option[E] = //if (p(head)) Some(head) else None
		if (forHead(p)) Some(head) else None


//	override protected[this] def dropTake(from: Int, until: Int) :Repr =
//		if (from<=0 && until>=1) repr else empty



	override def slice(from: Int, until: Int) :Repr = if (from<=0 && until>=1) repr else empty
	override def take(n: Int) :Repr = if (n>=1) repr else empty
	override def drop(n: Int) :Repr = if (n>=1) empty else repr
	override def dropRight(n :Int) :Repr = if (n>=1) empty else repr
	override def takeRight(n :Int) :Repr = if (n>=1) repr else empty

	override def splitAt(n: Int) :(Repr, Repr) = if (n<=0) (empty, repr) else (repr, empty)

	override def takeWhile(p: E => Boolean) :Repr = if (forHead(p)) repr else empty

	override def dropWhile(p: E => Boolean) :Repr = if (forHead(p)) empty else repr

	override def span(p: E => Boolean) :(Repr, Repr) = if (forHead(p)) (repr, empty) else (empty, repr)

	override def filter(p: E => Boolean, ourTruth: Boolean): Repr =
		if (forHead(p)==ourTruth) repr else empty

	override def filter(p: E => Boolean) :Repr = //filter(p, true)
		if (forHead(p)) repr else empty

	override def filterNot(p: E => Boolean) :Repr = //filter(p, false)
		if (!forHead(p)) repr else empty

	override def map[@specialized(Fun1Vals) O, That](f: E => O)(implicit bf: CanBuildFrom[Repr, O, That]) :That =
		(bf(repr) += forHead(f)).result() //todo: maybe this could be implemented directly in the factory

	override def flatMap[U, That](f: E => GenTraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]) :That =
		(bf(repr) ++= forHead(f).seq).result()



	override def scanLeft[@specialized(Fun2) O, That](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[Repr, O, That]) :That = {
		val b = bf(repr); b.sizeHint(2)
		(b += z += op(z, head)).result()
	}


	override def scanRight[@specialized(Fun2) O, That](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[Repr, O, That]) :That = {
		val b = bf(repr); b sizeHint 2
		(b += op(head, z) += z).result()
	}


	override def sameElements[U >: E](that: GenIterable[U]) :Boolean = that match {
		case s :IndexedSeq[U] => s.size==1 && s.head==head
		case s :LinearSeq[U] => s.nonEmpty && s.head==head && s.tail.isEmpty
		case _ =>
			val it = that.iterator; it.hasNext && it.next()==head && !it.hasNext
	}


	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) :Unit =
		if (len>0 && start >=0 && start < xs.length-1)
			xs(start) = head


	override def copyToBuffer[B >: E](dest: mutable.Buffer[B]) :Unit = dest += head

	override def inverse :AptIterable[E] = this.asInstanceOf[AptIterable[E]]

	override def toString :String = stringPrefix + '(' +head +')'
}





/**
  * @author Marcin Mościcki
  */
trait SingletonSpecialization[@specialized(ItemTypes) +E, +Repr] extends IterableSpecialization[E, Repr] with SingletonTemplate[E, Repr] {

	protected[this] override def forHead[@specialized(Boolean, Unit) T](f :E => T) :T = f(head)

	override def head_? : ?[E] = Sure(head)

	override def last :E = head

	override def last_? : ?[E] = head_?

	override def find_?(p :E => Boolean, where :Boolean): ?[E] = if (p(head) == where) Sure(head) else Blank


	override def iterator: AptIterator[E] = AptIterator.one(head)

	/** Overriden for speed. */
	override def foreach[@specialized(Unit) U](f :E=>U) :Unit = f(head)

	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) :O = op(z, head)

	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O) :O = op(head, z)




	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) :Unit =
		if (start < 0)
			throw new IllegalArgumentException(s"$stringPrefix.copyToArray([], $start, $len)")
		else if (len>0 && start < xs.length-1)
			xs(start) = head

	override protected[this] def trustedCopyTo(xs: Array[E], start: Int, total: Int) :Int = {
		xs(start) = head; 1
	}


	override def copyToBuffer[B >: E](dest: mutable.Buffer[B]) :Unit = dest += head

	override def toSeq :AptSeq[E] = AptSeq.one(head)


}


/** Base class for dedicated implementations of collections containing exactly one element.
  * @tparam E element type
  * @tparam Repr specific type of the collection described by this interface, usually the self type of implementors,
  *              but it is not enforced here.
  */
abstract class SingletonFoundation[+E, +Repr] extends IterableTemplate[E, Repr] with SingletonTemplate[E, Repr] {
	override def collectFirst[B](pf: PartialFunction[E, B]) :Option[B] = {
		pf.runWith{ b: B => return Some(b) }(head)
		None
	}
}


/*
abstract class SingletonFoundation[+E, +Repr] extends IterableFoundation[E, Repr] {
	this :IterableSpecialization[E, Repr] =>

	protected def empty :Repr
	override def size = 1
	override def tail = empty
	override def init = empty
	override def last = head
	override def headOption = Some(head)
	override def lastOption = Some(head)
	override def hasFastSize = true
	override def hasDefiniteSize: Boolean = true
	override def isEmpty = false
	override def nonEmpty = false

	protected[this] override def forHead[@specialized(Fun1Res) O](f :E=>O) :O = f(head)
	override def foreach[@specialized(Unit) U](f :E=>U) :Unit = forHead(f) //f(head)
	override protected def reverseForeach(f: E => Unit): Unit = forHead(f) //foreach(f) //f(head)
	override def forall(p: E => Boolean) = forHead(p) //p(head)
	override def exists(p: E => Boolean) = forHead(p)
	override def find(p: E => Boolean) = //if (p(head)) Some(head) else None
		if (forHead(p)) Some(head) else None



	override protected[this] def dropTake(from: Int, until: Int) :Repr =
		if (from<=0 && until>=1) repr else empty

	override def slice(from: Int, until: Int) = if (from<=0 && until>=1) repr else empty
	override def take(n: Int) = if (n>=1) repr else empty
	override def drop(n: Int) = if (n>=1) empty else repr


	override def splitAt(n: Int) = if (n<=0) (empty, repr) else (repr, empty)

	override def takeWhile(p: (E) => Boolean) :Repr = if (forHead(p)) repr else empty

	override def dropWhile(p: (E) => Boolean) :Repr = if (forHead(p)) empty else repr

	override def span(p: (E) => Boolean) = if (forHead(p)) (repr, empty) else (empty, repr)

	override def filter(p: E => Boolean, where: Boolean): Repr =
		if (forHead(p)==where) repr else empty

	override def filter(p: E => Boolean) :Repr = //filter(p, true)
		if (forHead(p)) repr else empty

	override def filterNot(p: E => Boolean) :Repr = //filter(p, false)
		if (!forHead(p)) repr else empty

	override def map[@specialized(Fun1Vals) O, That](f: E => O)(implicit bf: CanBuildFrom[Repr, O, That]) = {
		val b = bf(repr); b sizeHint 1
		(b += forHead(f)).result()
	}


	override def flatMap[U, That](f: E => GenTraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]) =
		(bf(repr) ++= forHead(f).seq).result()

	override def sameElements[U >: E](that: GenIterable[U]) = that match {
		case s :IndexedSeq[U] => s.size==1 && s.head==head
		case s :LinearSeq[U] => s.nonEmpty && s.head==head && s.tail.isEmpty
		case _ =>
			val it = that.iterator; it.hasNext && it.next()==head && !it.hasNext
	}


	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) =
		if (len>0 && start >=0 && start < xs.length-1)
			xs(start) = head

	override def copyToBuffer[B >: E](dest: mutable.Buffer[B]) = dest += head

	override def iterator :FitIterator[E] = forHead[FitIterator[E]](FitIterator.apply[E])

	override def inverse = this.asInstanceOf[AptIterable[E]]

	override def toString = stringPrefix + '(' +head +')'
}
*/
