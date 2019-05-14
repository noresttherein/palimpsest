package net.turambar.palimpsest.specialty.seqs

import scala.collection.generic.CanBuildFrom
import scala.reflect._
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.{IterableFoundation, SpecializableIterable, SpecializedIterableFactory}
import net.turambar.palimpsest.specialty._

import scala.annotation.unspecialized
import scala.collection.{IndexedSeqOptimized, LinearSeq, LinearSeqLike}




//todo: rename ArraySection
/** Common interface of specialized sequences backed by arrays. The backing array is by default shared whenever possible,
  * so all slicing methods (including the `drop` and `take` family) operate in constant type and work
  * as ''views'' on the parent sequence - any modifications made to one collection will be visible in the other.
  */
trait ArrayView[@specialized(Elements) +E]
	extends LinearSeq[E] with LinearSeqLike[E, ArrayView[E]] with IndexedSeqOptimized[E, ArrayView[E]] //generic interface extracted so it doesn't override FitIndexedSeq methods
	   with FitIndexedSeq[E] with ArrayViewLike[E, ArrayView[E]] with SpecializableIterable[E, ArrayView]
{
	@unspecialized override protected[this] def thisCollection :ArrayView[E] = this
	@unspecialized override protected[this] def toCollection(repr :ArrayView[E]) :ArrayView[E] = repr
	@unspecialized override def seq :ArrayView[E] = this


	override protected[this] def at(idx: Int): E = array(headIdx + idx)



	override protected[this] def startsWithUnchecked(that :SliceLike[E, _], offset :Int) :Boolean = that match {
		case a :ArrayView[_] =>
			var me = offset; var she = a.headIdx; val myEnd = me + a.length
			val my = array; val her = a.arr.asInstanceOf[Array[E]]
			while(me<myEnd && my(me)==her(she)) { me+=1; she += 1 }
			me==myEnd
		case _ =>
			super.startsWithUnchecked(that, offset)
	}
	
	
	
	override def companion: FitCompanion[ArrayView] = ArrayView


	protected[this] def writeReplace :AnyRef =
		if (length==array.length) this
		else {
			val b = newBuilder; b.sizeHint(length)
			(b ++= this).result()
		}

}





abstract class ArrayViewFactory[S[@specialized(Elements) X] <: ArrayView[X] with SpecializableIterable[X, S]]
	extends SpecializedIterableFactory[S]
{ outer =>

	/** An immutable, empty, erased instance of the collection. */
	val Empty: S[Nothing] = empty



	/** Creates an empty specialized sequence. If `E` is statically known to be a specializable (primitive) type,
	  * a proper specialized subclass of `S[E]` will be returned. However, when used from a generic type
	  * where `E` is not instantiated, a generic, erased version will be picked regardless of the 'runtime'
	  * value of `E`
	  *
      * @tparam E element type of the sequence
      * @return an empty sequence.
	  */
	@inline final override def empty[@specialized(Elements) E]: S[E] = using(RuntimeType.arrayFor[E], 0, 0)
	
	
	
	/** Creates an empty specialized sequence proper for the given element type.
	  * Returned sequence will be a specialized variant of `S[E]` iff `elementType` is a synthetic class
	  * representing a specializable primitive (such as `Integer.TYPE` and other java primitives with the exception of `void`).
      * @param elementType exact component type of the backing array of the created sequence.
      * @tparam E element type of the created sequence (and array)
      * @return an empty sequence backed by an empty array with component type equal to the given class.
	  */
	@inline final def emptyOfClass[E](elementType :Class[E]) :S[E] = emptyOf(RuntimeType.ofClass(elementType))
	
	
	/** Creates an empty specialized sequence based on implicit class information for `E`.
	  * Returned sequence will be a specialized variant of `S[E]` '''iff''' `E` is an inbuilt scala 'AnyVal' type.
	  * @tparam E element type of the created sequence (and array)
	  * @return an empty sequence backed by an empty array with component type equal to the given class.
	  */
	@inline final override def emptyOf[E :RuntimeType] :S[E] =
		ForArray(new ArrayBounds[E])


	/** Create a sequence with contents given in this array.
	  * Unless this is a companion of an immutable sequence type, returned sequence will be a direct view on the array,
	  * with any modifications made through the created sequence or outside code being mutually visible.
	  * While this method is not `@specialized`, a proper specialized version of the class will be chosen based
	  * on the component type of the array. If the array contains primitive types (`AnyVal`s),
	  * `S[E]` will avoid boxing/unboxing wherever possible.
	  *
	  * Use [[copy]] if you want the resulting sequence to own a copy of the array.
	  * @param contents array containing elements of the created sequence.
	  * @return a specialized sequence of length equal to the length of the given array, consisting initially of the elements of the array.
	  * @see [[copy]]
	  */
	def apply[E](contents :Array[E]) :S[E] = apply(ArrayBounds.share(contents))
	
	/** Create a sequence with contents of a range from the given array.
	  * Unless this is a companion of an immutable sequence type, returned sequence will be a direct view on the array,
	  * with any modifications made through the created sequence or outside code being mutually visible.
	  * While this method is not `@specialized`, a proper specialized version of the class will be chosen based
	  * on the component type of the array. If the array contains primitive types (`AnyVal`s),
	  * `S[E]` will avoid boxing/unboxing wherever possible.
	  *
	  * Use [[copy]] if you want the resulting sequence to own a copy of the array.
	  * @param contents array containing elements of the created sequence.
	  * @param offset index in the array of the first element of the sequence
	  * @param length requested length of the sequence,
	  *               specifying the number of elements in the array starting from `offset` to include in the returned sequence.
	  * @return a specialized sequence of length equal to the length of the given array, consisting initially of the elements of the array.
	  * @see [[copy]]
	  */
	def apply[E](contents :Array[E], offset :Int, length :Int) :S[E] = apply(ArrayBounds.share(contents, offset, length))
	
	
	/** Create a sequence with contents of a range from the given array.
	  * Unless this is a companion of an immutable sequence type, returned sequence will be a direct view on the array,
	  * with any modifications made through the created sequence or outside code being mutually visible.
	  * While this method is not `@specialized`, a proper specialized version of the class will be chosen based
	  * on the component type of the array. If the array contains primitive types (`AnyVal`s),
	  * `S[E]` will avoid boxing/unboxing wherever possible.
	  *
	  * Use [[copy]] if you want the resulting sequence to own a copy of the array.
	  * @param from index in the array of the first element of the sequence, defining its lower bound
	  * @param contents array containing elements of the created sequence.
	  * @param until index in the array of the first element ''after'' the contents of the sequence, defining its upper bound.
	  * @return a specialized sequence of length equal to the length of the given array, consisting initially of the elements of the array.
	  * @see [[copy]]
	  */
	def apply[E](from :Int, contents :Array[E], until :Int) :S[E] = apply(ArrayBounds.share(from, contents, until))

	protected[seqs] def apply[E](contents :ArrayBounds[E]) :S[E] = ForArray(contents)(contents.specialization)

	/** Create a specialized sequence backed by an array of the given type, initialized by runtime default value for `E`.
	  * This method is intended to be used only for primitive types, and calling it for user-defined value types
	  * will result in an exception. Use the variant with an explicit default value if you wish to do so.
	  * @param size the size of the created sequence.
	  * @tparam E an inbuilt scala 'AnyVal' subtype.
	  * @return a specialized sequence backed by the array of the given length.
	  * @see [[of(Int, _)]]
	  */
	@inline final def of[E <:AnyVal :RuntimeType](size :Int) :S[E] = {
		val buffer = RuntimeType.arrayFor[E](size)
		if (!buffer.getClass.getComponentType.isPrimitive)
			throw new IllegalArgumentException(s"$this.of[${RuntimeType[E]}]: not a primitive component type")
		shared(new ArrayBounds[E](buffer, 0, size))
	}
	
	/** Create a specialized sequence backed by an array of the given size, initialized with the given default value.
	  * The component type `E` of the backing array will be determined by either implicit [[ClassTag]] for `E`, or
	  * or runtime class of the given default value. Note that if `value` is a boxed primitive,
	  * but no implicit or explicit `elementType` argument is provided, created sequence
	  * will be instead backed by an 'unboxed' array, and an appropriate specialized subclass will be returned.
	  * @param size requested size of the sequence.
	  * @param value initial value of all elements in the sequence.
	  * @param elementType component type of the backing array
	  * @tparam E element type of the sequence.
	  * @return a sequence with `size` copies of `value`
	  */
	@inline final def of[E](size :Int, value :E)(implicit elementType :ClassTag[E] = ClassTag(RuntimeType.UnboxedClass(value.getClass))) :S[E] = {
		val array = new Array[E](size)
		val spec = RuntimeType.of(elementType)
		if (size > 0 && value != spec.default)
			arrayFill(array, value)
		shared(new ArrayBounds[E](array, 0, size)(spec))
	}

	
	/** Create a specialized sequence backed by an array of the given type, initialized by runtime default value for `E`.
	  * This method is intended to be used only for primitive types, and calling it for user-defined value types
	  * will result in an exception. Use the variant with an explicit default value if you wish to do so.
	  * @param size the size of the created sequence.
	  * @tparam E an inbuilt scala 'AnyVal' subtype.
	  * @return a specialized sequence backed by the array of the given length.
	  * @see [[of(Int, _)]]
	  */
	def ofClass[E<:AnyVal](elementType :Class[E], size :Int) :S[E] = of(size)(RuntimeType.ofClass(elementType))
	
	/** Create a specialized sequence backed by an array of the given size, initialized with the given default value.
	  * The component type `E` of the backing array will be determined by either implicit [[ClassTag]] for `E`, or
	  * or runtime class of the given default value. If `value` is a boxed primitive, created sequence
	  * will be backed by either 'unboxed' or 'boxed' array, depending on `elementType`,
	  * and an appropriate specialized subclass will be returned.
	  * @param size requested size of the sequence.
	  * @param value initial value of all elements in the sequence.
	  * @param elementType component type of the backing array
	  * @tparam E element type of the sequence.
	  * @return a sequence with `size` copies of `value`
	  */
	def ofClass[E](elementType :Class[E], size :Int, value :E) :S[E] = of(size, value)(ClassTag[E](elementType))


	/** Create a sequence of the given elements, backed by a new array with component type equal to the given array.
	  * Backing array for the created sequence will be an exact copy of `contents` and, in case of reference types,
	  * include exactly the same objects.
      * @param contents elements to be included in the sequence
      * @tparam E element type of the sequence.
      * @return a specialized version of `S[E]` appropriate for the component type of the given array.
	  */
	@inline final def copy[E](contents :Array[E]) :S[E] =
		shared(ArrayBounds.copy(contents))
	
	/** Create a sequence of the with elements from a given section of an array,
	  * backed by a new array with component type equal to the given array.
	  * Backing array for the created sequence will be a new array of size equal to `length min (contents.length-offset)`.
	  * @param contents array with elements to be included in the sequence
	  * @param offset index of the first element to include in the sequence.
	  * @param length number of elements from the array to include.
	  * @tparam E element type of the sequence.
	  * @return a specialized version of `S[E]` appropriate for the component type of the given array.
	  */
	@inline final def copy[E](contents :Array[E], offset :Int, length :Int) :S[E] =
		shared(ArrayBounds.copy(contents, offset, length))
	
	
	/** Create a sequence of the with elements from a given section of an array,
	  * backed by a new array with component type equal to the given array.
	  * Backing array for the created sequence will be a new array of size equal to `untilIndex-fromIndex`
	  * (after trimming both to valid index values for the given array).
	  * @param contents array with elements to be included in the sequence
	  * @param fromIndex index in the array of the first element to include in the sequence.
	  * @param untilIndex index in the array of the first element after the contents of the sequence.
	  * @tparam E element type of the sequence..
	  * @return a specialized version of `S[E]` appropriate for the component type of the given array.
	  */
	@inline final def copy[E](fromIndex :Int, contents :Array[E], untilIndex :Int) :S[E] =
		shared(ArrayBounds.copy(fromIndex, contents, untilIndex))


	private[seqs] def shared[E](contents :ArrayBounds[E]) :S[E] = ForArray(contents)(contents.specialization)

	
	private[this] final val ForArray = new Specialize.With[S, ArrayBounds] {
		override def specialized[@specialized E: RuntimeType](param: ArrayBounds[E]): S[E] =
			using(param.array, param.start, param.length)
	}


	/** Delegates to this factory's [[ArrayViewFactory#using]], which creates a new array view of corresponding specialization
	  * and parameters. Doesn't validate input and trusts the caller to be in an appropriate specialization context, hence access restriction.
	  */
	@inline final private[palimpsest] def view[@specialized(Elements) E] (array :Array[E], offset :Int, length :Int) :S[E] =
		using(array, offset, length)

	protected def using[@specialized(Elements) E](array :Array[E], offset :Int, length :Int) :S[E]


	/** Specialized implementation of the standard companion `newBuilder` method.
	  * Returned builder will be specialized - and build a specialized sequence - only if
	  * this method is called from specialized code (or when `E` is statically known to be a specializable type).
	  * For this reason, if you want to ensure a specialized instance from within generic code,
	  * it is safer to use [[fitBuilder]] instead.
      * @tparam E element type of the built sequence
      * @return a builder for `S[E]` specialized according to the variant of this method.
	  */
	@inline override final def newBuilder[@specialized(Elements) E]: FitBuilder[E, S[E]] =
		new ArrayViewBuilder[E](RuntimeType[E].emptyArray.asInstanceOf[Array[E]])


	/** A builder for a sequence backed by an array, which specialization - and resulting component type - are given
	  * implicitly. Note that there will ''always'' be an implicit value for `Specialized[E]`, at worst representing
	  * a completely erased sequence, backed by an array of 'AnyRef'. However, if any information about type `E`
	  * is available at the calling point - such as a `ClassTag`, `TypeTag`, or specialized calling code, created
	  * builder and resulting sequence will be specialized accordingly.
	  * @tparam E element type of the sequence
	  * @param specialization implicit specialization information used to determine both
	  *                       proper specialized variant of this sequence and backing array component type.
	  */
	@inline final override def fitBuilder[E](implicit specialization :RuntimeType[E]) :FitBuilder[E, S[E]] =
		specialization.call(SpecificBuilder)
	//todo: rename ^this to build[E]

	private[this] final val SpecificBuilder :Specialize[SpecializedBuilder] = new Specialize[SpecializedBuilder] {
		override def specialized[@specialized E](implicit spec :RuntimeType[E]) =
			new ArrayViewBuilder[E](spec.emptyArray.asInstanceOf[Array[E]])
	}


//	/** Builder factory method enforcing the specialization of the resulting sequence based on implicit class
//	  * information for `E`. Returned builder - and built sequence - will be specialized only if `ClassTag[E]`
//	  * represents a specializable primitive.
//      *
//	  * @param elementType backing array component type, used also to determine proper specialization of the sequence.
//	  * @tparam E element type of the sequence
//	  * @return a specialized builder proper for the given element type.
//	  */
//	@inline final def buildAs[E](implicit elementType :ClassTag[E]) :FitBuilder[E, S[E]] =
//		SpecificBuilder[E]()

	/** Builder for a sequence backed by an array of the given component type. Returned builder - and built sequence -
	  * will be specialized if and only if `elementType` is a java primitive (such as `Integer.TYPE`, not `Integer.class`).
	  * @param elementType component type of the array used by the builder and the final sequence.
	  * @return a specialized builder proper for the given element type.
	  */
	@inline final def buildAsClass[E](elementType :Class[E]) :FitBuilder[E, S[E]] =
		RuntimeType.ofClass(elementType).call(SpecificBuilder)



	@inline final def newReverseBuilder[@specialized(Elements) E] :FitBuilder[E, S[E]] = new ReverseArrayViewBuilder[E]()

	@inline final def buildReversed[E](ofType :RuntimeType[E]) :FitBuilder[E, S[E]] = ofType.call(NewReverseBuilder)

	protected[this] final val NewReverseBuilder :Specialize[SpecializedBuilder] = new Specialize[SpecializedBuilder] {
		override def specialized[@specialized E : RuntimeType]: SpecializedBuilder[E] = newReverseBuilder[E]
	}




	override def canFitFromRef[E :RuntimeType] :CanFitFrom[S[_], E, S[E]] =
		new CanBuildRefArray[E]


	protected class CanBuildRefArray[E :RuntimeType] extends CanBuildSpecialized[E] {

		override def apply(from: S[_]): FitBuilder[E, S[E]] =
			from.fitBuilder[E](specialization)

		override def apply(): FitBuilder[E, S[E]] = //newBuilder[E]
			new ArrayViewBuilder[E](runtimeType.emptyArray.asInstanceOf[Array[E]])


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[CanBuildRefArray[_]]

		override def equals(that :Any) :Boolean = that match {
			case cbf :CanBuildRefArray[_] => (cbf eq this) || elementType==cbf.elementType && super.equals(cbf)
			case _ => false
		}

		override def toString = s"$outer.CBF[${runtimeType.classTag}]"
	}



	protected sealed class ArrayViewBuilder[@specialized(Elements) E](protected[this] final var array :Array[E])
		extends IterableFoundation[E, SharedArrayBuffer[E]] with FitBuilder[E, S[E]] with DefaultArrayBuffer[E]
	{
		def this()(implicit elementType :ClassTag[E]) = this(new Array[E](0))

		protected[this] final var len = 0
		protected[palimpsest] final var headIdx=0
		override protected var unmodifiable: Boolean = false


		override def sizeHint(targetSize :Int) :Unit =
			if (targetSize>array.length)
				reserve(targetSize-array.length)

		override def typeHint[L <: E](implicit specialization: RuntimeType[L]): FitBuilder[E, S[E]] =
			if (length>0 || specialization =:= specialization)
				this
			else if (specialization.isValueType)
				specialization.asInstanceOf[RuntimeType[E]].call(NewBuilder)
			else
				this
//				new ArrayViewBuilder[L](specialization.emptyArray.asInstanceOf[Array[L]])


		@unspecialized
		override def ++=(elems: FitTraversableOnce[E]) :this.type = { appendAll(elems); this }


		override def result(): S[E] = { //todo: verify this is properly specialized
			val res = outer.using(array, headIdx, length)
			unmodifiable = true
			res
		}


		override def origin: AnyRef = outer

		override def stringPrefix = s"$outer.Builder[$specialization]"
	}





	protected final class ReverseArrayViewBuilder[@specialized(Elements) E](buffer :GrowingArrayBuffer[E] = new GrowingArrayBuffer[E]())
		extends FitBuilder[E, S[E]]
	{

		override def addOne :E=>Unit = { e :E => e +=: buffer }


		override def sizeHint(expect: Int): Unit =
			if (buffer.arr.length<expect)
				buffer.reserveFront(expect - buffer.headIdx - buffer.length)


		override def +=(elem: E): this.type = { elem +=: buffer; this}


		override def clear(): Unit = buffer.clear()

		override def result(): S[E] = {
			buffer.freeze()
			outer.using(buffer.arr.asInstanceOf[Array[E]], buffer.headIdx, buffer.length)
		}

		override def toString = s"$outer.reverseBuilder[$specialization]"
	}
	
/*
	private class InverseArrayBuilder[@specialized(Elements) E](buffer :GrowingArrayBuffer[E]) extends FitBuilder[E, S[E]] {
		override val addOne :E=>Unit = { e :E => e +=: buffer }
		
		override def ++=(xs: TraversableOnce[E]): this.type = xs match {
			case fit :FitItems[E] =>
				fit traverse addOne; this
			case _ => xs foreach addOne; this
				
		}
		
		override def +=(elem: E): this.type = { elem +=: buffer; this }
		
		override def result(): S[E] = {
			val res = outer.using(buffer.arr.asInstanceOf[Array[E]], buffer.offset, buffer.length)
			buffer.freeze()
			res
		}
		
		override def reverseResult: FitBuilder[E, S[E]] = new Array

		override def size: Int = ???
		
		override def clear(): Unit = buffer.clear()
		
		override def sizeHint(expect: Int): Unit =
			if (expect > buffer.length)
				buffer.reserveFront(expect - buffer.length)
	}

*/

}







/** Factory of specialized sequences backed by arrays, with fast slicing and random access. */
object ArrayView extends ArrayViewFactory[ArrayView] {
	type Stable[@specialized(Elements) +E] = StableArray[E]
	type Mutable[@specialized(Elements) E] = SharedArray[E]


	@inline def Acc[E :RuntimeType] :ArrayView[E] = ArrayPlus.emptyOf[E]


	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ArrayView[_], E, ArrayView[E]]): CanBuildFrom[ArrayView[_], E, ArrayView[E]] =
		fit.cbf


	override protected def using[@specialized(Elements) E](array: Array[E], offset: Int, length: Int): ArrayView[E] =
		new UnknownArrayView(array, offset, length)
//		SharedArray.view(array, offset, length)
	


	/** `ArrayView` implementation serving as an immutable view over an array possibly mutable by other collections.
	  *  Needed for some `toSeq` implementations to not inadvertently expose the array to mutation or give false promises.
	  *  Used by [[net.turambar.palimpsest.specialty.ArrayIterator]].
	  */
	private[palimpsest] final class UnknownArrayView[@specialized(Elements) E](
            final override protected[this] val array :Array[E],
            final override protected[palimpsest] val headIdx :Int,
            final override val length :Int
		) extends ArrayView[E]
	{
		override protected def section(from :Int, until :Int) :ArrayView[E] = new UnknownArrayView(array, from, until - from)
	}
}




