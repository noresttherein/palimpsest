package net.noresttherein.palimpsest.iterables

import scala.collection.generic.CanBuildFrom
import scala.collection.{breakOut, GenIterable, GenTraversableOnce}
import net.noresttherein.palimpsest.RuntimeType.Specialized.{Fun1Vals, Fun2}
import net.noresttherein.palimpsest.{?, RuntimeType}
import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.seqs.{AptBuffer, AptSeq}






/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait MappedIterableTemplate[+X, +E, +This] extends IterableTemplate[E, This] {
	/** The collection, which elements after mapping comprise the elements of this collection. */
	protected[this] def source :AptIterable[X]

	/** Adapt a function working on this collection's element types to one accepting the element type of the backing collection. */
	protected[this] def forSource[@specialized(Boolean, Unit) O](f :E=>O) :X=>O

	protected[this] def mine: X=>E

	override def size :Int = source.size
	override def hasFastSize :Boolean = source.hasFastSize
	override def hasDefiniteSize: Boolean = source.hasDefiniteSize
	override def isEmpty :Boolean = source.isEmpty
	override def nonEmpty :Boolean = source.nonEmpty
	override def ofAtLeast(items :Int) :Boolean = source.ofAtLeast(items)

	override def foreach[@specialized(Unit) U](f: E => U) :Unit = source foreach forSource(f)

	override protected def reverseForeach(f: E => Unit): Unit = source reverseTraverse forSource(f)

	override def map[@specialized(Fun1Vals) O, C](f: E => O)(implicit bf: CanBuildFrom[This, O, C]) :C =
		source.map(forSource(f))(breakOut)

	override def flatMap[U, C](f: E => GenTraversableOnce[U])(implicit bf: CanBuildFrom[This, U, C]) :C =
		source.flatMap(forSource(f))(breakOut)


	override def forall(p: E => Boolean) :Boolean = source forall forSource(p)
	override def exists(p: E => Boolean) :Boolean = source exists forSource(p)
	override def count(p: E => Boolean) :Int = source count forSource(p)

	override def find(p: E => Boolean) :Option[E] = source.find(forSource(p)).map(mine)

	override def find_?(p :E=>Boolean, where :Boolean): ?[E] =
		source.find_?(forSource(p), where).map(mine) //fitMap(mine)(specialization) //not worth specializing

	override def iterator :AptIterator[E] = source.iterator.fitMap(mine)(specialization) //new MappedIterator(mine)(source.iterator)

	override def toSeq :AptSeq[E] =  source.map(mine)(breakOut) /*todo: not specialized*/

	override def toBuffer[U >: E] :AptBuffer[U] = source.map(mine)(breakOut)

	override def toFitBuffer[U >: E : RuntimeType] :AptBuffer[U] =
		(AptBuffer.builder[U].mapInput(mine) ++= source).result()

}






/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait IterableViewTemplate[+E, +This] extends MappedIterableTemplate[E, E, This] {

	/** Adapt a function working on this collection's element types to one accepting the element type of the backing collection. */
	protected[this] def forSource[@specialized(Boolean, Unit) O](f :E=>O) :E=>O = f

	protected[this] def mine: E=>E = identity

//	override def foreach[@specialized(Unit) U](f: E => U) :Unit = source foreach forSource(f)
//
//	override protected def reverseForeach(f: E => Unit): Unit = source reverseTraverse forSource(f)

//	override def map[@specialized(Fun1Vals) O, C](f: E => O)(implicit bf: CanBuildFrom[This, O, C]) :C =
//		source.map(forSource(f))(breakOut)
//
//	override def flatMap[U, C](f: E => GenTraversableOnce[U])(implicit bf: CanBuildFrom[This, U, C]) :C =
//		source.flatMap(forSource(f))(breakOut)


	override def forall(p: E => Boolean) :Boolean = source forall p //forSource(p)
	override def exists(p: E => Boolean) :Boolean = source exists p //forSource(p)
	override def count(p: E => Boolean) :Int = source count p //forSource(p)

	override def find(p: E => Boolean) :Option[E] = source.find(forSource(p)) //.map(mine)
	override def find_?(p :E=>Boolean, where :Boolean): ?[E] = source.find_?(forSource(p), where)

	override def iterator :AptIterator[E] = source.iterator


	override def toSeq :AptSeq[E] =  source.toSeq
	override def toBuffer[U >: E] :AptBuffer[U] = source.toBuffer[U]
	override def toFitBuffer[U >: E : RuntimeType] :AptBuffer[U] = source.toFitBuffer[U]

}





/** Base class for collections being bijections with another collection.
  * This class is not specialized to avoid cartesian explosion of specialized variants, but most methods
  * do not require specialization. For full specialization it will be sufficient to override
  * [[IterableMapping#head]], [[IterableMapping#last]], [[IterableMapping#scanLeft]], [[IterableMapping#scanRight]],
  * [[IterableMapping#foldLeft]], [[IterableMapping#foldRight]] and of course hotspot method [[IterableMapping#adapt]]/[[IterableMapping#mine]]
  *
  * @tparam X element type of the source collection backing this collection
  * @tparam That source collection backing this collection
  * @tparam E this collections's element type.
  * @tparam This self type of this collection
  * @author Marcin Mościcki
  */
abstract class IterableMapping[X, +That <: AptIterable[X] with IterableSpecialization[X, That], +E, +This]
	extends MappedIterableTemplate[X, E, This]
{

	override protected[this] def source :That

	/** Represent another instance of source collection obtained from delegating a method call to `source`
	  * as a view with element type `E` similar to this one.
	  */
	protected[this] def adaptSource(col :That) :This

	/** Adapt a function working on this collection's element types to one accepting the element type of the backing collection.
	  * Default implementation uses [[IterableMapping#adapt]], which isn't specialized! This is a prime candidate for being overridden.
	  */
	protected override def forSource[@specialized(Boolean, Unit) O](f :E=>O) :X=>O = { x :X => f(adapt(x)) }

	/** Map backing collection's element to this collection element type. Same as `mine`, but can be better specialized and more efficient. */
	protected[this] def adapt(x :X) :E

	/** Function mapping elements of the source collection to this collection's elements. Implemented as a `SAM` call to `adapt`. */
	protected[this] override def mine :X=>E = adapt


	//todo: specialized function composition for two-argument functions
	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) :O = source.foldLeft(z)((o :O, x :X) => op(o, mine(x)))
	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O) :O = source.foldRight(z)((x :X, o :O) => op(mine(x), o))


	override def scanLeft[@specialized(Fun2) O, C](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[This, O, C]) :C =
		source.scanLeft(z)((o :O, x :X) => op(o, mine(x)))(breakOut)

	override def scanRight[@specialized(Fun2) O, C](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[This, O, C]) :C =
		source.scanRight(z)((x :X, o :O) => op(mine(x), o))(breakOut)



	override def tail :This = adaptSource(source.tail)
	override def init :This = adaptSource(source.init)

	override def slice(from: Int, until: Int) :This = adaptSource(source.slice(from, until))
	override def take(n: Int) :This = adaptSource(source.take(n))
	override def drop(n: Int) :This = adaptSource(source.drop(n))
	override def takeRight(n: Int) :This = adaptSource(source.takeRight(n))
	override def dropRight(n: Int) :This = adaptSource(source.dropRight(n))

	override def splitAt(n: Int) :(This, This) = {
		val split = source.splitAt(n)
		(adaptSource(split._1), adaptSource(split._2))
	}

	override def span(p: E => Boolean) :(This, This) = {
		val split = source.span(forSource(p))
		(adaptSource(split._1), adaptSource(split._2))
	}

	override def takeWhile(p: E => Boolean) :This = adaptSource(source.takeWhile(forSource(p)))
	override def dropWhile(p: E => Boolean) :This = adaptSource(source.dropWhile(forSource(p)))


	override def partition(p: E => Boolean) :(This, This) = {
		val split = source.partition(forSource(p))
		(adaptSource(split._1), adaptSource(split._2))
	}

	override def filter(p: E => Boolean, ourTruth: Boolean): This =
		if (ourTruth) filter(p)
		else filterNot(p)

	override def filter(p: E => Boolean) :This = adaptSource(source filter forSource(p))
	override def filterNot(p: E => Boolean) :This = adaptSource(source filterNot forSource(p))


	/** Unspecialized mapped iterator asking for being overridden. */
	//		override def iterator :FitIterator[E] = new MappedIterator(from)(source.iterator)

	//	override protected[this] def newBuilder: AptBuilder[E, This] = source.fit

}


/** Base class for [[AptIterable]] implementations which act as adapters to another collection.
  * It isn't specialized itself, but, as all its method delegate directly to the underlying `source` collection
  * and `This` constructor method [[IterableAdapter#fromSource]], they can safely remain generic.
  * Only [[IterableAdapter#head]] and [[IterableAdapter#last]] will benefit from specialized override.
  * This class serves as a base for various adapters presenting the underlying collection as a collection of
  * a different type or changing some aspect of it. Any operations which actually differentiate it from
  * the adapted type is left for subclasses.
  * @tparam Source type of the adapted collection
  * @tparam E element type of both this collection and adapted collection
  * @tparam This self type for this collection (not enforced apart from actually being a specialized iterable).
  */
abstract class IterableAdapter[+Source<:IterableSpecialization[E, Source], +E, +This] extends IterableFoundation[E, This] {

	override def runtimeType :RuntimeType[_<:E] = source.runtimeType

	protected[this] def source :Source
	protected[this] def fromSource(other :Source) :This


	override def size :Int = source.size
	override def hasFastSize :Boolean = source.hasFastSize
	override def hasDefiniteSize: Boolean = source.hasDefiniteSize
	override def isEmpty :Boolean = source.isEmpty
	override def nonEmpty :Boolean = source.nonEmpty
	override def ofAtLeast(items :Int) :Boolean = source.ofAtLeast(items)

	override def foreach[@specialized(Unit) U](f: E => U) :Unit = source foreach f
	override protected def reverseForeach(f: E => Unit): Unit = source reverseTraverse f

	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) :O = source.foldLeft(z)(op)
	override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O) :O = source.foldRight(z)(op)
	override def fold[U >: E](z: U)(op: (U, U) => U) :U = source.fold(z)(op)

	override def forall(p: E => Boolean) :Boolean = source forall p
	override def exists(p: E => Boolean) :Boolean = source exists p
	override def count(p: E => Boolean) :Int = source count p

	override def find(p: E => Boolean) :Option[E] = source.find(p)
	override def find_?(p: E => Boolean, where :Boolean): ?[E] = source.find_?(p)

	override def scanLeft[@specialized(Fun2) O, C](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[This, O, C]) :C =
		source.scanLeft(z)(op)(breakOut)

	override def scanRight[@specialized(Fun2) O, C](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[This, O, C]) :C =
		source.scanRight(z)(op)(breakOut)


	override def tail :This = fromSource(source.tail)
	override def init :This = fromSource(source.init)

	override def slice(from: Int, until: Int) :This = fromSource(source.slice(from, until))
	override def take(n: Int) :This = fromSource(source.take(n))
	override def drop(n: Int) :This = fromSource(source.drop(n))
	override def takeRight(n: Int) :This = fromSource(source.takeRight(n))
	override def dropRight(n: Int) :This = fromSource(source.dropRight(n))
	override def splitAt(n: Int) :(This, This) = {
		val split = source.splitAt(n)
		(fromSource(split._1), fromSource(split._2))
	}

	override def span(p: E => Boolean) :(This, This) = {
		val split = source.span(p)
		(fromSource(split._1), fromSource(split._2))
	}

	override def takeWhile(p: E => Boolean) :This = fromSource(source.takeWhile(p))
	override def dropWhile(p: E => Boolean) :This = fromSource(source.dropWhile(p))


	override def partition(p: E => Boolean) :(This, This) = {
		val split = source.partition(p)
		(fromSource(split._1), fromSource(split._2))
	}

	override def filter(p: E => Boolean, ourTruth: Boolean): This = fromSource(source.filter(p, ourTruth))
	override def filter(p: E => Boolean) :This = fromSource(source filter p)
	override def filterNot(p: E => Boolean) :This = fromSource(source filterNot p)

	override def map[@specialized(Fun1Vals) O, C](f: E => O)(implicit bf: CanBuildFrom[This, O, C]) :C =
		source.map(f)(breakOut)

	override def flatMap[U, C](f: E => GenTraversableOnce[U])(implicit bf: CanBuildFrom[This, U, C]) :C =
		source.flatMap(f)(breakOut)

	override def ++[B >: E, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[This, B, That]) :That =
		(source ++ that)(breakOut)

	override def ++:[B >: E, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[This, B, That]) :That =
		(that ++: source)(breakOut)

	override def sameElements[U >: E](that: GenIterable[U]) :Boolean = source.sameElements(that)

	override def iterator :AptIterator[E] = source.iterator

	override def head :E = source.head
	override def last :E = source.last
	override def head_? : ?[E] = source.head_?
	override def last_? : ?[E] = source.last_?
	override def headOption :Option[E] = source.headOption
	override def lastOption :Option[E] = source.lastOption



	override def toSeq :AptSeq[E] = source.toSeq

	override def toFitBuffer[U >: E : RuntimeType] :AptBuffer[U] =
		source.toFitBuffer[U]

	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) :Unit = source.copyToArray(xs, start, len)


}
