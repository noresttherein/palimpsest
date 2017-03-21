package net.turambar.palimpsest.specialty

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, FilterMonadic, GenericCompanion}
import scala.collection.{GenIterable, GenTraversableOnce, IterableLike, breakOut, mutable}
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterator.MappedIterator
import net.turambar.palimpsest.specialty.Specialized.{Fun1, Fun1Res, Fun1Vals, Fun2, Fun2Vals}
import net.turambar.palimpsest.specialty.iterables.IterableFoundation
import net.turambar.palimpsest.specialty.seqs.{FitBuffer, FitList, FitSeq, SharedArray}

import scala.runtime.Nothing$



/** Base trait of specialized collections mirroring scala [[Iterable]]. Overrides methods which can benefit
  * from specialization and delegates even more of them directly to their iterator counterparts.
  * @author Marcin Mościcki
  */
trait FitIterable[@specialized(Elements) +E]
	extends FitTraversableOnce[E] with Iterable[E]
			with SpecializableIterable[E, FitIterable] with IterableSpecialization[E, FitIterable[E]]
{

	override def companion: FitCompanion[FitIterable] = FitIterable
}



object FitIterable extends InterfaceIterableFactory[FitIterable] {
	override protected[this] type RealType[@specialized(Elements) X] = FitList[X]

	override protected[this] def default :FitIterableFactory[FitList] = FitList


	def unapply[E](col :GenTraversableOnce[E]) :Option[FitIterable[E]] = col match {
		case it :FitIterable[E] => Some(it)
		case arr :mutable.WrappedArray[E] => Some(SharedArray(arr.array))
		case _ => None
	}


	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[FitIterable[_], E, FitIterable[E]] ): CanBuildFrom[FitIterable[_], E, FitIterable[E]] =
		fit.cbf





	abstract class FilterIterable[+E, +Repr] extends FilterMonadic[E, Repr] {

		override def map[@specialized(Fun1Vals) O, That](f: (E) => O)(implicit bf: CanBuildFrom[Repr, O, That]): That =  {
			val b = FitBuilder(bf(from)).mapInput(f).filterInput(predicate)
			b ++= iterable
			b.result()
		}

		override def flatMap[O, That](f: (E) => GenTraversableOnce[O])(implicit bf: CanBuildFrom[Repr, O, That]): That = {
			val b = FitBuilder(bf(from)).flatMapInput(f).filterInput(predicate)
			b ++= iterable
			b.result()
		}

		override def foreach[@specialized(Unit) U](f: (E) => U): Unit =
			iterable.iterator.filter(predicate).foreach(f)

		override def withFilter(f: (E) => Boolean): FilterIterable[E, Repr]

		protected[this] def iterable :IterableSpecialization[E, Repr]
		protected[this] def from :Repr = iterable.repr
		protected[this] def predicate :E=>Boolean
	}


	class SpecializedFilter[@specialized(Elements) +E, +Repr](
																 protected[this] val iterable :IterableSpecialization[E, Repr],
																 protected[this] val predicate :E=>Boolean)
		extends FilterIterable[E, Repr]
	{
		override def withFilter(f: (E) => Boolean): SpecializedFilter[E, Repr] =
			new SpecializedFilter(iterable, (e :E) => predicate(e) && f(e))
	}






	abstract class ElementSerializer[@specialized(Elements) E] {
		def apply(os :ObjectOutputStream, elem :E) :Unit
	}

	object ElementSerializer extends Specialize.For[ElementSerializer] {
		type OOS = ObjectOutputStream
		type S[E] = ElementSerializer[E]

		override def forByte :S[Byte] = (os :OOS, elem :Byte) => os.writeByte(elem)
		override def forShort :S[Short] = (os :OOS, elem :Short) => os.writeShort(elem)
		override def forChar :S[Char] = (os :OOS, elem :Char) => os.writeChar(elem)
		override def forInt :S[Int] = (os :OOS, elem :Int) => os.writeInt(elem)
		override def forLong :S[Long] = (os :OOS, elem :Long) => os.writeLong(elem)
		override def forFloat :S[Float] = (os :OOS, elem :Float) => os.writeFloat(elem)
		override def forDouble :S[Double] = (os :OOS, elem :Double) => os.writeDouble(elem)
		override def forBoolean :S[Boolean] = (os :OOS, elem :Boolean) => os.writeBoolean(elem)
		override def forUnit :S[Unit] = (os :OOS, elem :Unit) => ()
		override def forNothing :S[Nothing] = (os :OOS, elem :Nothing) => ()
		override def forNull :S[Null] = (os :OOS, elem :Null) => os.writeObject(null)
		override def specialized[@specialized E: Specialized]: S[E] = (os :OOS, elem :E) => os.writeObject(elem)
	}

	abstract class ElementDeserializer[@specialized(Elements) E] {
		def apply(is :ObjectInputStream) :E
	}

	object ElementDeserializer extends Specialize.For[ElementDeserializer] {
		type OIS = ObjectInputStream
		type D[E] = ElementDeserializer[E]

		override def forByte :D[Byte] = (is :OIS) => is.readByte
		override def forShort :D[Short] = (is :OIS) => is.readShort
		override def forChar :D[Char] = (is :OIS) => is.readChar
		override def forInt :D[Int] = (is :OIS) => is.readInt
		override def forLong :D[Long] = (is :OIS) => is.readLong
		override def forFloat :D[Float] = (is :OIS) => is.readFloat
		override def forDouble :D[Double] = (is :OIS) => is.readDouble
		override def forBoolean :D[Boolean] = (is :OIS) => is.readBoolean
		override def forUnit :D[Unit] = (is :OIS) => ()
		override def forNothing :D[Nothing] = (is :OIS) => throw new NoSuchElementException(s"Attempted to deserialize Nothing")
		override def forNull :D[Null] = (is: OIS) => null
		override def specialized[@specialized E: Specialized]: D[E] = (is :OIS) => is.readObject.asInstanceOf[E]
	}

















	/** Base class for collections being bijections with another collection.
	  * This class is not specialized to avoid cartesian explosion of specialized variants, but most methods
	  * do not require specialization. For full specialization it will be sufficient to override
	  * [[IterableMapping#head]], [[IterableMapping#last]], [[IterableMapping#scanLeft]], [[IterableMapping#scanRight]],
	  * [[IterableMapping#foldLeft]], [[IterableMapping#foldRight]] and of course hotspot method [[IterableMapping#my]]/[[IterableMapping#from]]
	  *
	  * @tparam X element type of the source collection backing this collection
	  * @tparam That source collection backing this collection
	  * @tparam E this collections's element type.
	  * @tparam This self type of this collection
	  * @author Marcin Mościcki
	  */
	abstract class IterableMapping[X, +That<:IterableSpecialization[X, That], +E, +This] extends IterableTemplate[E, This] {

		/** collection containing actual elements of this collection to be mapped with `my`/`from`. */
		protected[this] def source :That

		/** Represent another instance of source collection obtainined from delegating a method call to `source`
		  * as a view with element type `E` similar to this one.
		  */
		protected[this] def fromSource(col :That) :This

		/** Adapt a function working on this collection's element types to one accepting the element type of the backing collection.
		  * Default implemetnation uses [[IterableMapping#my]], which isn't specialized! This is a prime candidate for being overriden.
		  */
		protected def forSource[@specialized(Fun1Res) O](f :E=>O) :X=>O = {x :X => f(my(x)) }

		/** Map backing collection's element to this collection element type. Same as `from`, but can be better specialized and more efficient. */
		protected[this] def my(x :X) :E

		/** Function mapping elements of the source collection to this collection's elements. Implemented as a `SAM` call to `my`. */
		protected[this] def from :X=>E = my

		override def size = source.size
		override def hasFastSize = source.hasFastSize
		override def hasDefiniteSize: Boolean = source.hasDefiniteSize
		override def isEmpty = source.isEmpty
		override def nonEmpty = source.nonEmpty
		override def ofAtLeast(items :Int) = source.ofAtLeast(items)


		override def foreach[@specialized(Unit) U](f: (E) => U) :Unit = source foreach forSource(f)

		override protected def reverseForeach(f: (E) => Unit): Unit = source reverseTraverse forSource(f)

		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) = source.foldLeft(z)((o :O, x :X) => op(o, my(x)))
		override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O) = source.foldRight(z)((x :X, o :O) => op(my(x), o))

		override def forall(p: (E) => Boolean) = source forall forSource(p)
		override def exists(p: (E) => Boolean) = source exists forSource(p)
		override def count(p: (E) => Boolean) = source count forSource(p)

		override def find(p: (E) => Boolean) = source.find(forSource(p)).map(my)


		override def scanLeft[@specialized(Fun2) O, C](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[This, O, C]) =
			source.scanLeft(z)((o :O, x :X) => op(o, my(x)))(breakOut)

		override def scanRight[@specialized(Fun2) O, C](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[This, O, C]) =
			source.scanRight(z)((x :X, o :O) => op(my(x), o))(breakOut)


		override def tail = fromSource(source.tail)
		override def init = fromSource(source.init)

		override def slice(from: Int, until: Int) = fromSource(source.slice(from, until))
		override def take(n: Int) = fromSource(source.take(n))
		override def drop(n: Int) = fromSource(source.drop(n))
		override def takeRight(n: Int) = fromSource(source.takeRight(n))
		override def dropRight(n: Int) = fromSource(source.dropRight(n))
		override def splitAt(n: Int) = {
			val split = source.splitAt(n)
			(fromSource(split._1), fromSource(split._2))
		}

		override def span(p: (E) => Boolean) = {
			val split = source.span(forSource(p))
			(fromSource(split._1), fromSource(split._2))
		}

		override def takeWhile(p: (E) => Boolean) = fromSource(source.takeWhile(forSource(p)))
		override def dropWhile(p: (E) => Boolean) = fromSource(source.dropWhile(forSource(p)))


		override def partition(p: (E) => Boolean) = {
			val split = source.partition(forSource(p))
			(fromSource(split._1), fromSource(split._2))
		}

		override protected[this] def filter(p: (E) => Boolean, ourTruth: Boolean): This =
			if (ourTruth) filter(p)
			else filterNot(p)

		override def filter(p: (E) => Boolean) :This = fromSource(source filter forSource(p))
		override def filterNot(p: (E) => Boolean) = fromSource(source filterNot forSource(p))

		override def map[@specialized(Fun1Vals) O, C](f: (E) => O)(implicit bf: CanBuildFrom[This, O, C]) :C = source.map(forSource(f))(breakOut)

		override def flatMap[U, C](f: (E) => GenTraversableOnce[U])(implicit bf: CanBuildFrom[This, U, C]) :C = source.flatMap(forSource(f))(breakOut)

		/** Unspecialized mapped iterator asking for being overriden. */
//		override def iterator :FitIterator[E] = new MappedIterator(from)(source.iterator)

		//	override protected[this] def newBuilder: FitBuilder[E, This] = source.fit

		override def head = my(source.head)
		override def last = my(source.last)

		override def toFitSeq = source.toFitSeq.map(from)

		override def toFitBuffer[U >: E : Specialized] =
			source.toFitBuffer[X](source.specialization.asInstanceOf[Specialized[X]]).map(from)
	}


	/** Base class for [[FitIterable]] implementations which act as adapters to another collection.
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

		override def specialization :Specialized[_<:E] = source.specialization

		protected[this] def source :Source
		protected[this] def fromSource(other :Source) :This


		override def size = source.size
		override def hasFastSize = source.hasFastSize
		override def hasDefiniteSize: Boolean = source.hasDefiniteSize
		override def isEmpty = source.isEmpty
		override def nonEmpty = source.nonEmpty
		override def ofAtLeast(items :Int) = source.ofAtLeast(items)

		override def foreach[@specialized(Unit) U](f: (E) => U) :Unit = source foreach f
		override protected def reverseForeach(f: (E) => Unit): Unit = source reverseTraverse f

		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) = source.foldLeft(z)(op)
		override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O) = source.foldRight(z)(op)
		override def fold[U >: E](z: U)(op: (U, U) => U) = source.fold(z)(op)

		override def forall(p: (E) => Boolean) = source forall p
		override def exists(p: (E) => Boolean) = source exists p
		override def count(p: (E) => Boolean) = source count p

		override def find(p: (E) => Boolean) = source.find(p)


		override def scanLeft[@specialized(Fun2) O, C](z: O)(op: (O, E) => O)(implicit bf: CanBuildFrom[This, O, C]) =
			source.scanLeft(z)(op)(breakOut)

		override def scanRight[@specialized(Fun2) O, C](z: O)(op: (E, O) => O)(implicit bf: CanBuildFrom[This, O, C]) =
			source.scanRight(z)(op)(breakOut)


		override def tail = fromSource(source.tail)
		override def init = fromSource(source.init)

		override def slice(from: Int, until: Int) = fromSource(source.slice(from, until))
		override def take(n: Int) = fromSource(source.take(n))
		override def drop(n: Int) = fromSource(source.drop(n))
		override def takeRight(n: Int) = fromSource(source.takeRight(n))
		override def dropRight(n: Int) = fromSource(source.dropRight(n))
		override def splitAt(n: Int) = {
			val split = source.splitAt(n)
			(fromSource(split._1), fromSource(split._2))
		}

		override def span(p: (E) => Boolean) = {
			val split = source.span(p)
			(fromSource(split._1), fromSource(split._2))
		}

		override def takeWhile(p: (E) => Boolean) = fromSource(source.takeWhile(p))
		override def dropWhile(p: (E) => Boolean) = fromSource(source.dropWhile(p))


		override def partition(p: (E) => Boolean) = {
			val split = source.partition(p)
			(fromSource(split._1), fromSource(split._2))
		}

		override protected[this] def filter(p: (E) => Boolean, ourTruth: Boolean): This =
			if (ourTruth) filter(p)
			else filterNot(p)

		override def filter(p: (E) => Boolean) :This = fromSource(source filter p)
		override def filterNot(p: (E) => Boolean) = fromSource(source filterNot p)

		override def map[@specialized(Fun1Vals) O, C](f: (E) => O)(implicit bf: CanBuildFrom[This, O, C]) :C =
			source.map(f)(breakOut)

		override def flatMap[U, C](f: (E) => GenTraversableOnce[U])(implicit bf: CanBuildFrom[This, U, C]) :C =
			source.flatMap(f)(breakOut)

		override def ++[B >: E, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[This, B, That]) =
			(source ++ that)(breakOut)

		override def ++:[B >: E, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[This, B, That]) =
			(that ++: source)(breakOut)

		override def sameElements[U >: E](that: GenIterable[U]) = source.sameElements(that)

		override def iterator :FitIterator[E] = source.iterator

		override def head = source.head
		override def last = source.last
		override def headOption = source.headOption
		override def lastOption = source.lastOption



		override def toFitSeq = source.toFitSeq

		override def toFitBuffer[U >: E : Specialized] =
			source.toFitBuffer[U]

		override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) = source.copyToArray(xs, start, len)


	}

} //object FitIterable



