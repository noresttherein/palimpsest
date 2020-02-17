package net.noresttherein.palimpsest.iterables

import java.io.{ObjectInputStream, ObjectOutputStream}

import net.noresttherein.palimpsest._
import net.noresttherein.palimpsest.iterables.AptCompanion.CanFitFrom
import net.noresttherein.palimpsest.RuntimeType.Specialized.{Fun1Vals, Fun2}
import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.seqs.{ArrayPlus, AptBuffer, AptList, AptSeq, SharedArray}

import scala.annotation.unspecialized
import scala.collection.{breakOut, immutable, mutable, GenIterable, GenTraversableOnce}
import scala.collection.generic.{CanBuildFrom, FilterMonadic}




//todo: rename this to AptIterable after cleanup

/** Base trait of specialized collections mirroring scala [[Iterable]]. Overrides methods which can benefit
  * from specialization and delegates even more of them directly to their iterator counterparts.
  * @author Marcin Mościcki
  */
trait AptIterable[@specialized(ItemTypes) +E]
	extends Vals[E] with Iterable[E] with SpecializableIterable[E, AptIterable]
	   with IterableSpecialization[E, AptIterable[E]] with CloneableIterable[E, AptIterable[E]]
{

	override def companion: AptCompanion[AptIterable] = AptIterable
}





object AptIterable extends InterfaceIterableFactory[AptIterable] {
	override protected[this] type RealType[@specialized(ItemTypes) X] = AptList[X]

	override protected[this] def default :AptIterableFactory[AptList] = AptList


	def unapply[E](col :GenTraversableOnce[E]) :Option[AptIterable[E]] = col match {
		case it :AptIterable[E] => Some(it)
		case arr :mutable.WrappedArray[E] => Some(SharedArray(arr.array))
		case _ => None
	}

	def from[@specialized(ItemTypes) E](newIterator :() => AptIterator[E]) :AptIterable[E] = new FromIterator(newIterator)


	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[AptIterable[_], E, AptIterable[E]]): CanBuildFrom[AptIterable[_], E, AptIterable[E]] =
		fit.cbf







	abstract class FilterIterable[+E, +Repr] extends FilterMonadic[E, Repr] {
		//todo: mapInput calls are not specialized here
		override def map[@specialized(Fun1Vals) O, That](f: E => O)(implicit bf: CanBuildFrom[Repr, O, That]): That =  {
			val b = AptBuilder(bf(from)).mapInput(f).filterInput(predicate)
			b ++= iterable
			b.result()
		}

		override def flatMap[O, That](f: E => GenTraversableOnce[O])(implicit bf: CanBuildFrom[Repr, O, That]): That = {
			val b = AptBuilder(bf(from)).flatMapInput(f).filterInput(predicate)
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


	class SpecializedFilter[@specialized(ItemTypes) +E, +Repr](
			 protected[this] val iterable :IterableSpecialization[E, Repr],
			 protected[this] val predicate :E=>Boolean)
		extends FilterIterable[E, Repr]
	{
		override def foreach[@specialized(Unit) U](f :E => U) :Unit =
			iterable.foreach { e => if (predicate(e)) f(e) }

		override def withFilter(f: E => Boolean): SpecializedFilter[E, Repr] =
			new SpecializedFilter(iterable, (e :E) => predicate(e) && f(e))
	}






	abstract class ElementSerializer[@specialized(ItemTypes) -E] {
		def apply(os :ObjectOutputStream, elem :E) :Unit
	}

	object ElementSerializer extends Specialize.Individually[ElementSerializer] {
		type OOS = ObjectOutputStream
		type S[E] = ElementSerializer[E]

		override val forByte :S[Byte] = (os :OOS, elem :Byte) => os.writeByte(elem)
		override val forShort :S[Short] = (os :OOS, elem :Short) => os.writeShort(elem)
		override val forChar :S[Char] = (os :OOS, elem :Char) => os.writeChar(elem)
		override val forInt :S[Int] = (os :OOS, elem :Int) => os.writeInt(elem)
		override val forLong :S[Long] = (os :OOS, elem :Long) => os.writeLong(elem)
		override val forFloat :S[Float] = (os :OOS, elem :Float) => os.writeFloat(elem)
		override val forDouble :S[Double] = (os :OOS, elem :Double) => os.writeDouble(elem)
		override val forBoolean :S[Boolean] = (os :OOS, elem :Boolean) => os.writeBoolean(elem)
		override val forUnit :S[Unit] = (os :OOS, elem :Unit) => ()
		override val forNothing :S[Nothing] = forUnit //(os :OOS, elem :Nothing) => ()
		override def forRef[E: RuntimeType]: S[E] = (os :OOS, elem :E) => os.writeObject(elem)
	}

	abstract class ElementDeserializer[@specialized(ItemTypes) +E] {
		def apply(is :ObjectInputStream) :E
	}

	object ElementDeserializer extends Specialize.Individually[ElementDeserializer] {
		type OIS = ObjectInputStream
		type D[E] = ElementDeserializer[E]

		override val forByte :D[Byte] = (is :OIS) => is.readByte
		override val forShort :D[Short] = (is :OIS) => is.readShort
		override val forChar :D[Char] = (is :OIS) => is.readChar
		override val forInt :D[Int] = (is :OIS) => is.readInt
		override val forLong :D[Long] = (is :OIS) => is.readLong
		override val forFloat :D[Float] = (is :OIS) => is.readFloat
		override val forDouble :D[Double] = (is :OIS) => is.readDouble
		override val forBoolean :D[Boolean] = (is :OIS) => is.readBoolean
		override val forUnit :D[Unit] = (is :OIS) => ()
		override val forNothing :D[Nothing] = (is :OIS) => throw new NoSuchElementException("ElementDeserializer[Nothing](_)")
		override def forRef[@specialized E: RuntimeType]: D[E] = (is :OIS) => is.readObject.asInstanceOf[E]
	}



	/** Base trait for serializer proxies of specialized collections. Subclasses need to provide an appropriate builder
	  * for deserialization and declare a `@transient` field holding the serialised iterable. Concrete `AptIterable`
	  * implementations should declare a `writeReplace :AnyRef` method passing their self reference to a new instance
	  * of this serializer and returning it.
	  * @tparam E element type of the serialized collection
	  */
	trait IterableSerializer[@specialized(ItemTypes) E, This <: AptIterable[E]] extends Serializable {
		import java.io.{ObjectInputStream => IS, ObjectOutputStream => OS}
		@transient
		protected[this] var self :This
		protected[this] def builder :AptBuilder[E, This]

		private def writeObject(os :OS) :Unit = writeIterable(os, self)

		protected[this] def writeIterable(os :OS, iterable :AptIterable[E]) :Unit = {
			os.defaultWriteObject()
			val serializer = ElementSerializer[E]()
			var count = iterable.size
			os.writeInt(count)
			if (iterable.runtimeType.isFun1Arg) {
				iterable foreach { serializer(os, _) }
			} else {
				val it = iterable.iterator
				while(count > 0) {
					serializer(os, it.next()); count -= 1
				}
			}
		}

		private def readObject(is :IS) :Unit = self = readIterable(is)

		//todo: check if this gets specialized!
		protected[this] def readIterable(is :IS) :This = {
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

		private def readResolve() :AnyRef = self
	}





	private[palimpsest] class FromIterator[@specialized(ItemTypes) +E](newIterator :() => AptIterator[E]) extends AptIterable[E] {
		@unspecialized
		override protected def reverseForeach(f :E => Unit) :Unit = reversed.foreach(f)

		@unspecialized
		override def iterator :AptIterator[E] = newIterator()
	}


} //object AptIterable










/** Common 'implementation' mixin for immutable collections defining `stable`, `carbon` and `clone()` to independently
  * to return this collection (`repr`). As it doesn't actually extend any standard immutable interfaces, subclasses
  * will generally extend also [[net.noresttherein.palimpsest.iterables.StableIterable]], but it is not enforced or
  * strictly required.
  */
trait StableIterableTemplate[+E, +Repr <: StableIterable[E]] extends CloneableIterable[E, Repr] {
	/** Returns `this.repr`. */
	override def stable :Repr = repr

	/** Returns `this.repr`. */
	override def carbon :Repr = repr

	/** Returns `this.repr`. */
	override def clone() :Repr = repr
}



/** Base trait for all immutable collection classes. Note that it is not specialized regarding its parameter unlike
  * the base `AptIterable`. This doesn't impact specialization of actual implementations, as the proper version
  * of each specialized `AptIterable` method will be executed, providing subclasses extend the specialized version
  * of `AptIterable` separately. The only consequence for client code is that having this trait as part of method
  * signature doesn't make it a candidate for specialization. In order to enforce specialization of such methods, other
  * specialized interfaces must be included either as parameters or the result type.  The limitation for implementing
  * classes on the other hand is the requirement of having `AptIterable` and its specialized base traits before
  * this trait in the linearization of each class. In order not to separate a specialized version of any trait from its
  * generic supertype, which would break the wirering of synthetic specialized methods, each extending trait and class
  * must mix this trait ''after'' `AptIterable` (or some of its derived specialized traits), which will ensure that
  * all specialized base traits of this trait precede it in linearization.
  */
trait StableIterable[+E]
	extends immutable.Iterable[E] with StableIterableTemplate[E, StableIterable[E]]
	   with AptIterable[E] with IterableSpecialization[E, StableIterable[E]] with SpecializableIterable[E, StableIterable]
{
	override def companion :AptCompanion[StableIterable] = StableIterable
}



object StableIterable extends InterfaceIterableFactory[StableIterable] {
	override protected[this] type RealType[@specialized(ItemTypes) X] = ArrayPlus[X]

	override protected def default :AptIterableFactory[RealType] = ArrayPlus

	override implicit def canBuildFrom[E](implicit fit :CanFitFrom[StableIterable[_], E, StableIterable[E]])
			:CanBuildFrom[StableIterable[_], E, StableIterable[E]] =
		fit.cbf
}








/** Base trait for all mutable collection classes. Note that it is not specialized regarding its parameter unlike
  * the base `AptIterable`. This doesn't impact specialization of actual implementations, as the proper version
  * of each specialized `AptIterable` method will be executed, providing subclasses extend the specialized version
  * of `AptIterable` separately. The only consequence for client code is that having this trait as part of method
  * signature doesn't make it a candidate for specialization. In order to enforce specialization of such methods, other
  * specialized interfaces must be included either as parameters or the result type.  The limitation for implementing
  * classes on the other hand is the requirement of having `AptIterable` and its specialized base traits before
  * this trait in the linearization of each class. In order not to separate a specialized version of any trait from its
  * generic supertype, which would break the wirering of synthetic specialized methods, each extending trait and class
  * must mix this trait ''after'' `AptIterable` (or some of its derived specialized traits), which will ensure that
  * all specialized base traits of this trait precede it in linearization.
  */
trait MutableIterable[E]
	extends mutable.Iterable[E] with CloneableIterable[E, MutableIterable[E]]
		with AptIterable[E] with IterableSpecialization[E, MutableIterable[E]] with SpecializableIterable[E, MutableIterable]
{
	override def companion :AptCompanion[MutableIterable] = MutableIterable

}



object MutableIterable extends InterfaceIterableFactory[MutableIterable] {
	override protected[this] type RealType[@specialized(ItemTypes) X] = SharedArray[X]

	override protected def default :AptIterableFactory[RealType] = SharedArray

	override implicit def canBuildFrom[E](implicit fit :CanFitFrom[MutableIterable[_], E, MutableIterable[E]])
	:CanBuildFrom[MutableIterable[_], E, MutableIterable[E]] =
		fit.cbf
}





