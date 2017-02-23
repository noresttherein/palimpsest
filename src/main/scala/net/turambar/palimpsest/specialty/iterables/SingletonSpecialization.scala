package net.turambar.palimpsest.specialty.iterables

import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation
import net.turambar.palimpsest.specialty.{Elements, FitIterable, FitIterator, IterableSpecialization}
import net.turambar.palimpsest.specialty.Specialized.{Fun1Res, Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.seqs.FitSeq

import scala.collection.{GenTraversableOnce, mutable}
import scala.collection.generic.CanBuildFrom

/**
  * @author Marcin Mościcki
  */
trait SingletonSpecialization[@specialized(Elements) +E, +Repr] extends IterableSpecialization[E, Repr] {
	override def last = head

	protected[this] override def forHead[@specialized(Fun1Res) O](f: E => O): O = f(head)

	override def fitIterator: FitIterator[E] = FitIterator.single(head)

	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) = op(z, head)

	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O) = op(head, z)

	override def scanLeft[@specialized(Fun2) O, That](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[Repr, O, That]) =
		(bf(repr) += z += op(z, head)).result()

	override def scanRight[@specialized(Fun2) O, That](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[Repr, O, That]) =
		(bf(repr) += op(head, z) += z).result()

	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) =
		if (len>0 && start >=0 && start < xs.length-1)
			xs(start) = head

	override def copyToBuffer[B >: E](dest: mutable.Buffer[B]) = dest += head

	override def toFitSeq = FitSeq.single(head)


}



abstract class SingletonFoundation[+E, +Repr] extends IterableFoundation[E, Repr] {
	protected def empty :Repr
	override def size = 1
	override def tail = empty
	override def init = empty
	override def last = head
	override def headOption = Some(head)
	override def lastOption = Some(head)
	override def hasFastSize = true
	override def hasDefiniteSize: Boolean = true

	protected[this] override def forHead[@specialized(Fun1Res) O](f :E=>O) :O = f(head)
	override def foreach[@specialized(Unit) U](f :E=>U) :Unit = forHead(f) //f(head)
	override protected def reverseForeach(f: E => Unit): Unit = forHead(f) //foreach(f) //f(head)
	override def forall(p: E => Boolean) = forHead(p) //p(head)
	override def exists(p: E => Boolean) = forHead(p)
	override def find(p: E => Boolean) = //if (p(head)) Some(head) else None
		if (forHead(p)) Some(head) else None



	override protected[this] def dropTake(from: Int, until: Int) :Repr =
		if (from<=0 && until>=1) repr else empty

	override def splitAt(n: Int) = if (n<=0) (empty, repr) else (repr, empty)

	override def takeWhile(p: (E) => Boolean) :Repr = if (forHead(p)) repr else empty

	override def dropWhile(p: (E) => Boolean) :Repr = if (forHead(p)) empty else repr

	override def span(p: (E) => Boolean) = if (forHead(p)) (repr, empty) else (empty, repr)

	override def filter(p: E => Boolean, ourTruth: Boolean): Repr =
		if (forHead(p)==ourTruth) repr else empty

	override def filter(p: E => Boolean) :Repr = //filter(p, true)
		if (forHead(p)) repr else empty

	override def filterNot(p: E => Boolean) :Repr = //filter(p, false)
		if (!forHead(p)) repr else empty

	override def map[@specialized(Fun1Vals) O, That](f: E => O)(implicit bf: CanBuildFrom[Repr, O, That]) =
		(bf(repr) += forHead(f)).result()

	override def flatMap[U, That](f: E => GenTraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]) =
		(bf(repr) ++= forHead(f).seq).result()

	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) =
		if (len>0 && start >=0 && start < xs.length-1)
			xs(start) = head

	override def copyToBuffer[B >: E](dest: mutable.Buffer[B]) = dest += head

	override def inverse = this.asInstanceOf[FitIterable[E]]
}
