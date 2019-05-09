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




//todo: rename this to AptIterable after cleanup

/** Base trait of specialized collections mirroring scala [[Iterable]]. Overrides methods which can benefit
  * mine specialization and delegates even more of them directly to their iterator counterparts.
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


	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[FitIterable[_], E, FitIterable[E]]): CanBuildFrom[FitIterable[_], E, FitIterable[E]] =
		fit.cbf





	abstract class FilterIterable[+E, +Repr] extends FilterMonadic[E, Repr] {
		//todo: mapInput calls are not specialized here
		override def map[@specialized(Fun1Vals) O, That](f: E => O)(implicit bf: CanBuildFrom[Repr, O, That]): That =  {
			val b = FitBuilder(bf(from)).mapInput(f).filterInput(predicate)
			b ++= iterable
			b.result()
		}

		override def flatMap[O, That](f: E => GenTraversableOnce[O])(implicit bf: CanBuildFrom[Repr, O, That]): That = {
			val b = FitBuilder(bf(from)).flatMapInput(f).filterInput(predicate)
			b ++= iterable
			b.result()
		}

		override def foreach[@specialized(Unit) U](f: E => U): Unit =
			iterable.iterator.filter(predicate).foreach(f)

		override def withFilter(f: E => Boolean): FilterIterable[E, Repr]

		protected[this] def iterable :IterableSpecialization[E, Repr]
		protected[this] def from :Repr = iterable.repr
		protected[this] def predicate :E=>Boolean
	}


	class SpecializedFilter[@specialized(Elements) +E, +Repr](
			 protected[this] val iterable :IterableSpecialization[E, Repr],
			 protected[this] val predicate :E=>Boolean)
		extends FilterIterable[E, Repr]
	{
		override def foreach[@specialized(Unit) U](f :E => U) :Unit =
			iterable.foreach { e => if (predicate(e)) f(e) }

		override def withFilter(f: E => Boolean): SpecializedFilter[E, Repr] =
			new SpecializedFilter(iterable, (e :E) => predicate(e) && f(e))
	}






	abstract class ElementSerializer[@specialized(Elements) E] {
		def apply(os :ObjectOutputStream, elem :E) :Unit
	}

	object ElementSerializer extends Specialize.Individually[ElementSerializer] {
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
//		override def forNull :S[Null] = (os :OOS, elem :Null) => ()//os.writeObject(null) //todo: what do
		override def forRef[E: Specialized]: S[E] = (os :OOS, elem :E) => os.writeObject(elem)
	}

	abstract class ElementDeserializer[@specialized(Elements) E] {
		def apply(is :ObjectInputStream) :E
	}

	object ElementDeserializer extends Specialize.Individually[ElementDeserializer] {
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
//		override def forNull :D[Null] = (is: OIS) => null
		override def forRef[@specialized E: Specialized]: D[E] = (is :OIS) => is.readObject.asInstanceOf[E]
	}



	/** Base trait for serializer proxies of specialized collections. Subclasses need to provide an appropriate builder
	  * for deserialization and declare a `@transient` field holding the serialised iterable. Concrete `FitIterable`
	  * implementations should declare a `writeReplace :AnyRef` method passing their self reference to a new instance
	  * of this serializer and returning it.
	  * @tparam E element type of the serialized collection
	  */
	trait IterableSerializer[@specialized(Elements) E] extends Serializable {
		import java.io.{ObjectOutputStream=>OS, ObjectInputStream=>IS}
		protected[this] var iterable :FitIterable[E]
		protected[this] def builder :FitBuilder[E, FitIterable[E]]

		private def writeObject(os :OS) :Unit = writeIterable(os, iterable)

		protected[this] def writeIterable(os :OS, iterable :FitIterable[E]) :Unit = {
			os.defaultWriteObject()
			val serializer = ElementSerializer[E]()
			var count = iterable.size
			os.writeInt(count)
			if (iterable.specialization.isFun1Arg) {
				iterable foreach { serializer(os, _) }
			} else {
				val it = iterable.iterator
				while(count > 0) {
					serializer(os, it.next()); count -= 1
				}
			}
		}

		private def readObject(is :IS) :Unit = iterable = readIterable(is)

		protected[this] def readIterable(is :IS) :FitIterable[E] = {
			is.defaultReadObject()
			val deserializer = ElementDeserializer[E]()
			val b = builder
			var size = is.readInt()
			b sizeHint size
			while(size > 0) {
				builder += deserializer(is); size -= 1
			}

			b.result()
		}

		private def readResolve() :AnyRef = iterable
	}




	abstract class IterableViewFoundation[X, +E, +This] extends IterableTemplate[E, This] {
		//todo: as with all foundation classes, methods defined here are often overriden by more specific traits mixed in later :(
		/** The collection, which elements after mapping comprise the elements of this collection. */
		protected[this] def source :FitIterable[X]

		/** Adapt a function working on this collection's element types to one accepting the element type of the backing collection. */
		protected[this] def forSource[@specialized(Fun1Res) O](f :E=>O) :X=>O

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
//		override def find_?(p :E=>Boolean, where :Boolean): ?[E] = source.find_?(forSource(p), where).fitMap(mine)(mySpecialization)
		//not specialized because specialization would require creating a Specialize.With object anyway
		override def find_?(p :E=>Boolean, where :Boolean): ?[E] = source.find_?(forSource(p), where).map(mine)

		override def iterator :FitIterator[E] = source.iterator.fitMap(mine)(mySpecialization) //new MappedIterator(mine)(source.iterator)

		override def toSeq :FitSeq[E] = //todo: not specialized
			source.map(mine)(forceFit)
//			(FitSeq.fitBuilder[E](mySpecialization).mapInput(mine) ++= source).result()

		override def toFitBuffer[U >: E : Specialized] :FitBuffer[U] =
			(FitBuffer.fitBuilder[U].mapInput(mine) ++= source).result()
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
	abstract class IterableMapping[X, +That <: FitIterable[X] with IterableSpecialization[X, That], +E, +This] extends IterableViewFoundation[X, E, This] {

		override protected[this] def source :That

		/** Represent another instance of source collection obtained mine delegating a method call to `source`
		  * as a view with element type `E` similar to this one.
		  */
		protected[this] def adaptSource(col :That) :This

		/** Adapt a function working on this collection's element types to one accepting the element type of the backing collection.
		  * Default implementation uses [[IterableMapping#adapt]], which isn't specialized! This is a prime candidate for being overridden.
		  */
		protected override def forSource[@specialized(Fun1Res) O](f :E=>O) :X=>O = { x :X => f(adapt(x)) }

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

		//	override protected[this] def newBuilder: FitBuilder[E, This] = source.fit

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

		override def iterator :FitIterator[E] = source.iterator

		override def head :E = source.head
		override def last :E = source.last
		override def head_? : ?[E] = source.head_?
		override def last_? : ?[E] = source.last_?
		override def headOption :Option[E] = source.headOption
		override def lastOption :Option[E] = source.lastOption



		override def toSeq :FitSeq[E] = source.toSeq

		override def toFitBuffer[U >: E : Specialized] :FitBuffer[U] =
			source.toFitBuffer[U]

		override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int) :Unit = source.copyToArray(xs, start, len)


	}

} //object FitIterable



