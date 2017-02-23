package net.turambar.palimpsest.specialty.iterables

import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation
import net.turambar.palimpsest.specialty.{FitIterator, Specialized}
import net.turambar.palimpsest.specialty.Specialized.{Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.seqs.{FitBuffer, FitSeq}

import scala.collection.{GenIterable, GenTraversableOnce}
import scala.collection.generic.CanBuildFrom

/**
  * @author Marcin Mościcki
  */



abstract class EmptyIterable[+E, +Repr] extends IterableFoundation[E, Repr] {
	override def size = 0
	override def isEmpty = true
	override def nonEmpty = false
	override def hasFastSize = true
	override def hasDefiniteSize = true


	override def head = throw new NoSuchElementException(s"$this.head")
	override def headOption = None
	override def last = throw new NoSuchElementException(s"$this.last")
	override def lastOption = None
	override def tail = throw new UnsupportedOperationException(s"$this.tail")
	override def init = throw new UnsupportedOperationException(s"$this.init")


	override protected[this] def dropTake(from: Int, until: Int) = repr
	override def slice(from: Int, until: Int) = repr
	override def take(n: Int) = repr
	override def drop(n: Int) = repr
	override def takeWhile(p: (E) => Boolean) = repr
	override def dropWhile(p: (E) => Boolean) = repr
	override def takeRight(n: Int) = repr
	override def dropRight(n: Int) = repr

	override def span(p: (E) => Boolean) = (repr, repr)

	override def partition(p: (E) => Boolean) = (repr, repr)

	override def foreach[@specialized(Unit) O](f: (E) => O) :Unit = ()
	override protected def reverseForeach(f: (E) => Unit) :Unit = ()

	override def forall(p: (E) => Boolean) = true
	override def exists(p: (E) => Boolean) = false
	override def find(p: (E) => Boolean) = None
	override def count(p: (E) => Boolean) = 0



	override def scanLeft[@specialized(Fun2) O, That](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[Repr, O, That]) =
		(bf(repr) += z).result()

	override def scanRight[@specialized(Fun2) O, That](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[Repr, O, That]) =
		(bf(repr) += z).result()

	override def map[@specialized(Fun1Vals) O, That](f: (E) => O)(implicit bf: CanBuildFrom[Repr, O, That]) = bf(repr).result()

	override def flatMap[U, That](f: (E) => GenTraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]) = bf(repr).result()

	override def ++[B >: E, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]) =
		(bf(repr) ++= that.seq).result()

	override def ++:[B >: E, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]) =
		(bf(repr) ++= that).result()

	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) = z
	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O) = z

	override protected[this] def filter(p: (E) => Boolean, ourTruth: Boolean) = repr
	override def filter(p: (E) => Boolean) = repr
	override def filterNot(p: (E) => Boolean) = repr

	override def iterator :FitIterator[E] = FitIterator.Empty
	override def fitIterator :FitIterator[E] = FitIterator.Empty

	override def reversed: List[E] = Nil

	override def inverse :FitSeq[E] = FitSeq.Empty
	override def toFitSeq :FitSeq[E] = FitSeq.Empty
	override def toFitBuffer[U >: E : Specialized] = FitBuffer.like[U]

	override def sameElements[U >: E](that: GenIterable[U]) = that.isEmpty

	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) :Unit = ()

	override def toString = stringPrefix + "()"
}

