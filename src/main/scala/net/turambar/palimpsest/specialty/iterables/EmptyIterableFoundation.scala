package net.turambar.palimpsest.specialty.iterables

import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.{?, Blank, FitIterator, RuntimeType}
import net.turambar.palimpsest.specialty.RuntimeType.{Fun1Vals, Fun1Res, Fun2}
import net.turambar.palimpsest.specialty.seqs.{FitBuffer, FitSeq}

import scala.collection.{GenIterable, GenTraversableOnce}
import scala.collection.generic.CanBuildFrom


trait EmptyIterableTemplate[+E, +Repr] extends IterableTemplate[E, Repr] with OfKnownSize {
//	def empty :Repr = repr

	override def size = 0
	override def isEmpty = true
	override def nonEmpty = false
	override def hasFastSize = true
	override def hasDefiniteSize = true
	override def ofAtLeast(items :Int) :Boolean = items <= 0

	override def head :E = throw new NoSuchElementException(s"$this.head")
//todo: somehow this causes conflict with the declaration in IterableSpecialization ...
//	override def head_? : ?[E] = Blank
	override def headOption :Option[E] = None
//	override protected def forHead[@specialized(Fun1Res) O](f :E => O) :O = throw new NoSuchElementException(s"$this.head")
	override def last :E = throw new NoSuchElementException(s"$this.last")
//	override def last_? : ?[E] = Blank
	override def lastOption :Option[E] = None
//	override protected def forLast[@specialized(Fun1Res) O](f :E => O) :O = throw new NoSuchElementException(s"$this.last")

	override def tail :Repr = throw new UnsupportedOperationException(s"$this.tail")
	override def init :Repr = throw new UnsupportedOperationException(s"$this.init")

	override def tails :Iterator[Repr] = FitIterator.empty[Repr]
	override def inits :Iterator[Repr] = FitIterator.empty[Repr]

	//	override protected[this] def dropTake(from: Int, until: Int) = repr
	override def slice(from: Int, until: Int) :Repr = repr
	override def take(n: Int) :Repr = repr
	override def drop(n: Int) :Repr = repr
	override def takeRight(n: Int) :Repr = repr
	override def dropRight(n: Int) :Repr = repr
	override def splitAt(n :Int) :(Repr, Repr) = (repr, repr)

	override def span(p: E => Boolean) :(Repr, Repr) = (repr, repr)
	override def takeWhile(p: E => Boolean) :Repr = repr
	override def dropWhile(p: E => Boolean) :Repr = repr

	override def partition(p: E => Boolean) :(Repr, Repr) = (repr, repr)

	override def foreach[@specialized(Unit) O](f: E => O) :Unit = ()
	override protected def reverseForeach(f: E => Unit) :Unit = ()

	override def forall(p: E => Boolean) = true
	override def exists(p: E => Boolean) = false
	override def find(p: E => Boolean) :Option[E] = None
//	override def find_?(p :E => Boolean) : ?[E] = Blank
	override def find_?(p :E => Boolean, where :Boolean): ?[E] = Blank
	override def count(p: E => Boolean) = 0



	override def scanLeft[@specialized(Fun2) O, That](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[Repr, O, That]) :That =
		(bf(repr) += z).result()

	override def scanRight[@specialized(Fun2) O, That](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[Repr, O, That]) :That =
		(bf(repr) += z).result()

	override def map[@specialized(Fun1Vals) O, That](f: (E) => O)(implicit bf: CanBuildFrom[Repr, O, That]) :That = bf(repr).result()

	override def flatMap[U, That](f: (E) => GenTraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]) :That = bf(repr).result()

	override def ++[B >: E, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]) :That =
		(bf(repr) ++= that.seq).result()

	override def ++:[B >: E, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]) :That =
		(bf(repr) ++= that).result()

	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) :O = z
	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O) :O = z

	override def filter(p: E => Boolean, ourTruth: Boolean) :Repr = repr
	override def filter(p: E => Boolean) :Repr = repr
	override def filterNot(p: E => Boolean) :Repr = repr



	override def grouped(size: Int) :Iterator[Repr] = FitIterator.empty[Repr]
//	override def sliding(size: Int) = FitIterator.empty[Repr]
	override def sliding(size: Int, step: Int) :Iterator[Repr] = FitIterator.empty[Repr]

	override def iterator :FitIterator[E] = FitIterator.Empty
	override def toIterator :FitIterator[E] = FitIterator.Empty

	override def reversed: List[E] = Nil

	override def inverse :FitSeq[E] = FitSeq.Empty
	override def toFitSeq :FitSeq[E] = FitSeq.Empty
	override def toFitBuffer[U >: E : RuntimeType] :FitBuffer[U] = FitBuffer.of[U]
	override def toSeq :FitSeq[E] = FitSeq.Empty

	override def sameElements[U >: E](that: GenIterable[U]) :Boolean = that.isEmpty

	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) :Unit = ()
	override protected[this] def trustedCopyTo(xs: Array[E], start: Int, total: Int) = 0

	override def toString :String = stringPrefix + "()"

}

/**
  * @author Marcin Mościcki
  */
abstract class EmptyIterableFoundation[+E, +Repr] extends IterableFoundation[E, Repr] with EmptyIterableTemplate[E, Repr]

