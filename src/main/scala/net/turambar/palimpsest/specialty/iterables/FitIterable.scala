package net.turambar.palimpsest.specialty.iterables

import java.io.{ObjectInputStream, ObjectOutputStream}

import net.turambar.palimpsest.specialty._
import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.{Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.seqs.{ArrayPlus, FitBuffer, FitList, FitSeq, SharedArray}

import scala.annotation.unspecialized
import scala.collection.{breakOut, immutable, mutable, GenIterable, GenTraversableOnce}
import scala.collection.generic.{CanBuildFrom, FilterMonadic}




//todo: rename this to AptIterable after cleanup

/** Base trait of specialized collections mirroring scala [[Iterable]]. Overrides methods which can benefit
  * from specialization and delegates even more of them directly to their iterator counterparts.
  * @author Marcin Mościcki
  */
trait FitIterable[@specialized(Elements) +E]
	extends FitTraversableOnce[E] with Iterable[E] with SpecializableIterable[E, FitIterable]
	   with IterableSpecialization[E, FitIterable[E]] with CloneableIterable[E, FitIterable[E]]
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

	def from[@specialized(Elements) E](newIterator :() => FitIterator[E]) :FitIterable[E] = new FromIterator(newIterator)


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
		override def forRef[E: RuntimeType]: S[E] = (os :OOS, elem :E) => os.writeObject(elem)
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
		override def forRef[@specialized E: RuntimeType]: D[E] = (is :OIS) => is.readObject.asInstanceOf[E]
	}



	/** Base trait for serializer proxies of specialized collections. Subclasses need to provide an appropriate builder
	  * for deserialization and declare a `@transient` field holding the serialised iterable. Concrete `FitIterable`
	  * implementations should declare a `writeReplace :AnyRef` method passing their self reference to a new instance
	  * of this serializer and returning it.
	  * @tparam E element type of the serialized collection
	  */
	trait IterableSerializer[@specialized(Elements) E] extends Serializable {
		import java.io.{ObjectInputStream => IS, ObjectOutputStream => OS}
		protected[this] var iterable :FitIterable[E]
		protected[this] def builder :FitBuilder[E, FitIterable[E]]

		private def writeObject(os :OS) :Unit = writeIterable(os, iterable)

		protected[this] def writeIterable(os :OS, iterable :FitIterable[E]) :Unit = {
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





	private[palimpsest] class FromIterator[@specialized(Elements) +E](newIterator :() => FitIterator[E]) extends FitIterable[E] {
		@unspecialized
		override protected def reverseForeach(f :E => Unit) :Unit = reversed.foreach(f)

		@unspecialized
		override def iterator :FitIterator[E] = newIterator()
	}


} //object FitIterable










/** Common 'implementation' mixin for immutable collections defining `stable`, `carbon` and `clone()` to independently
  * to return this collection (`repr`). As it doesn't actually extend any standard immutable interfaces, subclasses
  * will generally extend also [[net.turambar.palimpsest.specialty.iterables.StableIterable]], but it is not enforced or
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
  * the base `FitIterable`. This doesn't impact specialization of actual implementations, as the proper version
  * of each specialized `FitIterable` method will be executed, providing subclasses extend the specialized version
  * of `FitIterable` separately. The only consequence for client code is that having this trait as part of method
  * signature doesn't make it a candidate for specialization. In order to enforce specialization of such methods, other
  * specialized interfaces must be included either as parameters or the result type.  The limitation for implementing
  * classes on the other hand is the requirement of having `FitIterable` and its specialized base traits before
  * this trait in the linearization of each class. In order not to separate a specialized version of any trait from its
  * generic supertype, which would break the wirering of synthetic specialized methods, each extending trait and class
  * must mix this trait ''after'' `FitIterable` (or some of its derived specialized traits), which will ensure that
  * all specialized base traits of this trait precede it in linearization.
  */
trait StableIterable[+E]
	extends immutable.Iterable[E] with StableIterableTemplate[E, StableIterable[E]]
	   with FitIterable[E] with IterableSpecialization[E, StableIterable[E]] with SpecializableIterable[E, StableIterable]
{
	override def companion :FitCompanion[StableIterable] = StableIterable
}



object StableIterable extends InterfaceIterableFactory[StableIterable] {
	override protected[this] type RealType[@specialized(Elements) X] = ArrayPlus[X]

	override protected def default :FitIterableFactory[RealType] = ArrayPlus

	override implicit def canBuildFrom[E](implicit fit :CanFitFrom[StableIterable[_], E, StableIterable[E]])
			:CanBuildFrom[StableIterable[_], E, StableIterable[E]] =
		fit.cbf
}








/** Base trait for all mutable collection classes. Note that it is not specialized regarding its parameter unlike
  * the base `FitIterable`. This doesn't impact specialization of actual implementations, as the proper version
  * of each specialized `FitIterable` method will be executed, providing subclasses extend the specialized version
  * of `FitIterable` separately. The only consequence for client code is that having this trait as part of method
  * signature doesn't make it a candidate for specialization. In order to enforce specialization of such methods, other
  * specialized interfaces must be included either as parameters or the result type.  The limitation for implementing
  * classes on the other hand is the requirement of having `FitIterable` and its specialized base traits before
  * this trait in the linearization of each class. In order not to separate a specialized version of any trait from its
  * generic supertype, which would break the wirering of synthetic specialized methods, each extending trait and class
  * must mix this trait ''after'' `FitIterable` (or some of its derived specialized traits), which will ensure that
  * all specialized base traits of this trait precede it in linearization.
  */
trait MutableIterable[E]
	extends mutable.Iterable[E] with CloneableIterable[E, MutableIterable[E]]
		with FitIterable[E] with IterableSpecialization[E, MutableIterable[E]] with SpecializableIterable[E, MutableIterable]
{
	override def companion :FitCompanion[MutableIterable] = MutableIterable

}



object MutableIterable extends InterfaceIterableFactory[MutableIterable] {
	override protected[this] type RealType[@specialized(Elements) X] = SharedArray[X]

	override protected def default :FitIterableFactory[RealType] = SharedArray

	override implicit def canBuildFrom[E](implicit fit :CanFitFrom[MutableIterable[_], E, MutableIterable[E]])
	:CanBuildFrom[MutableIterable[_], E, MutableIterable[E]] =
		fit.cbf
}





