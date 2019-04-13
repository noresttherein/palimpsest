package net.turambar.palimpsest.specialty.iterables

import java.lang.Math

import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.{?, ofKnownSize, Blank, Elements, FitIterable, FitIterator, IterableSpecialization, IterableTemplate, Sure}
import net.turambar.palimpsest.specialty.Specialized.{Fun1Res, Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.seqs.{FitSeq, StableSeq}

import scala.collection.{mutable, GenIterable, GenTraversableOnce, LinearSeq}
import scala.collection.generic.CanBuildFrom


trait DoubletonTemplate[+E, +This] extends IterableTemplate[E, This] with OfKnownSize {
	protected[this] def empty :This
	//	protected[this] def singleton(value :E) :This

	override def size = 2
	override def isEmpty = false
	override def nonEmpty = false
	override def ofAtLeast(items :Int) :Boolean = items <= 2
//	override def hasFastSize = true
//	override def hasDefiniteSize = true
	override def headOption = Some(head)
	override def lastOption = Some(last)


	override def foreach[@specialized(Unit) U](f :E=>U) :Unit = { forHead(f); forLast(f) }
	override def reverseForeach(f: E => Unit): Unit = { forLast(f); forHead(f) }
	override def forall(p: E => Boolean) :Boolean = forHead(p) && forLast(p)
	override def exists(p: E => Boolean) :Boolean = forHead(p) || forLast(p)

	override def find(p: E => Boolean) :Option[E] =
		if (forHead(p)) Some(head)
		else if (forLast(p)) Some(last)
		else None

	override def find_?(p: E => Boolean, where :Boolean) : ?[E] =
		if (forHead(p) == where) Some(head)
		else if (forLast(p) == where) Some(last)
		else None

//	override protected[this] def dropTake(from: Int, until: Int) :This =
//		if (until<=from || until<=0 || from >= 2) empty
//		else if (until==1) init
//		else if (from==1) tail
//		else repr


	override def slice(from: Int, until: Int) :This =
		if (from>=2 || until<=from) empty
		else if (from==1) tail
		else if (until==1) init
		else repr

	override def take(n: Int) :This =
		if (n>=2) repr
		else if (n==1) init
		else empty

	override def drop(n: Int) :This =
		if (n>=2) empty
		else if (n==1) tail
		else repr

	override def takeRight(n: Int) :This =
		if (n>=2) repr
		else if (n==1) tail
		else empty

	override def dropRight(n: Int) :This =
		if (n>=2) empty
		else if (n==1) init
		else repr

	override def splitAt(n: Int) :(This, This) =
		if (n<=0) (empty, repr)
		else if (n==1) (init, tail)
		else (repr, empty)

	override def takeWhile(p: E => Boolean) :This =
		if (forHead(p))
			if (forLast(p)) repr else init
		else empty

	override def dropWhile(p: E => Boolean) :This =
		if (forHead(p))
			if (forLast(p)) empty else tail
		else repr

	override def span(p: E => Boolean) :(This, This) =
		if (forHead(p))
			if (forLast(p)) (repr, empty) else (init, tail)
		else
			(empty, repr)

	override def filter(p: E => Boolean, ourTruth: Boolean): This =
		if (forHead(p)==ourTruth)
			if (forLast(p)==ourTruth) repr else init
		else if (forLast(p)==ourTruth) tail
		else empty

	override def filter(p: E => Boolean) :This =
		if (forHead(p))
			if (forLast(p)) repr else init
		else if (forLast(p)) tail
		else empty

	override def filterNot(p: E => Boolean) :This =
		if (forHead(p))
			if (forLast(p)) empty else tail
		else if (forLast(p)) init
		else repr

	override def map[@specialized(Fun1Vals) O, That](f: E => O)(implicit bf: CanBuildFrom[This, O, That]) :That = {
		val b = bf(repr); b.sizeHint(2)
		(b += forHead(f) += forLast(f)).result()
	}


	override def flatMap[U, That](f: E => GenTraversableOnce[U])(implicit bf: CanBuildFrom[This, U, That]) :That =
		(bf(repr) ++= forHead(f).seq ++= forLast(f).seq).result()

	override def sameElements[U >: E](that: GenIterable[U]) :Boolean = that match {
//		case fit :FitIterable[U] if fit.specialization == specialization =>
//			sameElements(fit.asInstanceOf[FitIterable[E]])
		case s :IndexedSeq[_] => s.size==2 && s.head==head && s(1)==last
		case s :LinearSeq[_] => s.nonEmpty && s.head==head && {
			val tail = s.tail; tail.nonEmpty && tail.head==last && tail.tail.isEmpty
		}
		case _ =>
			val it = that.iterator; it.hasNext && it.next()==head && it.next()==last && !it.hasNext
	}

//	protected[this] def sameElements(that :FitIterable[E]) :Boolean

	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) :Unit = {
		require(start>=0, "negative array index in copyToArray")
		val left = Math.min(len, xs.length-start)
		if (left>0) xs(start) = head
		if (left>1) xs(start+1) = last
	}

	override def copyToBuffer[B >: E](dest: mutable.Buffer[B]) :Unit = dest += head += last

	override def toString :String = stringPrefix + '(' +head +", " + last +')'
}


/**
  * @author Marcin Mościcki
  */
trait DoubletonSpecialization[@specialized(Elements) +E, +This]
	extends IterableSpecialization[E, This] with DoubletonTemplate[E, This]
{
//	protected[this] override def forHead[@specialized(Fun1Res) O](f: E => O): O = f(head)
//	protected[this] override def forLast[@specialized(Fun1Res) O](f :E=>O) :O = f(last)
	override def head_? : ?[E] = Sure(head)
	override def last_? : ?[E] = Sure(last)

	override def find_?(p :E => Boolean): ?[E] =
		if (p(head)) Sure(head)
		else if (p(last)) Sure(last)
		else Blank

	override def find_?(p :E => Boolean, where :Boolean): ?[E] =
		if (p(head) == where) Sure(head)
		else if (p(last) == where) Sure(last)
		else Blank

	override def iterator: FitIterator[E] = FitIterator(head, last)

	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) :O =
		op(op(z, head), last)

	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O) :O = op(head, op(last, z))

	override def scanLeft[@specialized(Fun2) O, That](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[This, O, That]) :That = {
		val b = bf(repr); b sizeHint 3
		val zz = op(z, head)
		(bf(repr) += z += zz += op(zz, last)).result()
	}

	override def scanRight[@specialized(Fun2) O, That](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[This, O, That]) :That = {
		val b = bf(repr); b sizeHint 3
		val zz = op(last, z)
		(b += op(head, zz) += zz += z).result()
	}



//
//	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) =
//		if (len>0 && start >=0 && start < xs.length-1)
//			xs(start) = head

	override def copyToBuffer[B >: E](dest: mutable.Buffer[B]) :Unit = dest += head

	override def toSeq :StableSeq[E] = FitSeq.pair(head, last)
}


abstract class DoubletonFoundation[+E, +This] extends IterableFoundation[E, This] with DoubletonTemplate[E, This]

/*
abstract class DoubletonFoundation[+E, +This] extends IterableFoundation[E, This] { //with DoubletonTemplate[E, This] {
	this :IterableSpecialization[E, This] =>

	protected[this] def empty :This
//	protected[this] def singleton(value :E) :This

	override def size = 2
	override def isEmpty = false
	override def nonEmpty = false
	override def hasFastSize = true
	override def hasDefiniteSize = true
	override def headOption = Some(head)
	override def lastOption = Some(last)


	override def foreach[@specialized(Unit) U](f :E=>U) :Unit = { forHead(f); forLast(f) }
	override protected def reverseForeach(f: E => Unit): Unit = { forLast(f); forHead(f) }
	override def forall(p: E => Boolean) = forHead(p) && forLast(p)
	override def exists(p: E => Boolean) = forHead(p) || forLast(p)
	override def find(p: E => Boolean) =
		if (forHead(p)) Some(head)
		else if (forLast(p)) Some(last)
		else None


	override protected[this] def dropTake(from: Int, until: Int) :This =
		if (until<=from || until<=0 || from >= 2) empty
		else if (until==1) init
		else if (from==1) tail
		else repr

	override def splitAt(n: Int) =
		if (n<=0) (empty, repr)
		else if (n==1) (init, tail)
		else (repr, empty)

	override def takeWhile(p: (E) => Boolean) :This =
		if (forHead(p))
			if (forLast(p)) repr else init
		else empty

	override def dropWhile(p: (E) => Boolean) :This =
		if (forHead(p))
			if (forLast(p)) empty else tail
		else repr

	override def span(p: (E) => Boolean) =
		if (forHead(p))
			if (forLast(p)) (repr, empty) else (init, tail)
		else
			(empty, repr)

	override def filter(p: E => Boolean, where: Boolean): This =
		if (forHead(p)==where)
			if (forLast(p)==where) repr else init
		else if (forLast(p)==where) tail
		else empty

	override def filter(p: E => Boolean) :This =
		if (forHead(p))
			if (forLast(p)) repr else init
		else if (forLast(p)) tail
		else empty

	override def filterNot(p: E => Boolean) :This =
		if (forHead(p))
			if (forLast(p)) empty else tail
		else if (forLast(p)) init
		else repr

	override def map[@specialized(Fun1Vals) O, That](f: E => O)(implicit bf: CanBuildFrom[This, O, That]) = {
		val b = bf(repr); b.sizeHint(2)
		(b += forHead(f) += forLast(f)).result()
	}


	override def flatMap[U, That](f: E => GenTraversableOnce[U])(implicit bf: CanBuildFrom[This, U, That]) =
		(bf(repr) ++= forHead(f).seq ++= forLast(f).seq).result()

	override def sameElements[U >: E](that: GenIterable[U]) = that match {
		case s :IndexedSeq[_] => s.size==2 && s.head==head && s(1)==last
		case s :LinearSeq[_] => s.nonEmpty && s.head==head && {
			val tail = s.tail; tail.nonEmpty && tail.head==last && tail.tail.isEmpty
		}
		case _ =>
			val it = that.iterator; it.hasNext && it.next()==head && it.next()==last && !it.hasNext
	}


	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) :Unit = {
		require(start>=0, "negative array index in copyToArray")
		val left = Math.min(len, xs.length-start)
		if (left>0) xs(start) = head
		if (left>1) xs(start+1) = last
	}

	override def copyToBuffer[B >: E](dest: mutable.Buffer[B]) = dest += head += last


}
*/
