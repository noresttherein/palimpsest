package net.turambar.palimpsest.specialty.seqs

import scala.collection.generic.CanBuildFrom
import scala.reflect._

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation
import net.turambar.palimpsest.specialty.{ArrayBounds, Elements, FitBuilder, FitCompanion, Specialize, Specialized, SpecializedIterableFactory, arrayFill}





/** Common interface of specialized sequences backed by arrays.
  * The backing array is shared whenever possible, so all slicing methods
  * (including the `drop` and `take` family) operate in constant type and work
  * as ''views'' on the parent sequence - any modifications made to one will be visible in the other.
  */
trait ArrayView[@specialized(Elements) +E]
	extends FitSeq[E] with ArrayViewLike[E, ArrayView]
{
	override def companion: FitCompanion[ArrayView] = ArrayView
}





abstract class ArrayViewFactory[S[@specialized(Elements) X] <: ArrayView[X] with ArrayViewLike[X, S]]
	extends SpecializedIterableFactory[S]
{ outer =>
	
	override val Empty: S[Nothing] = empty
	
	/** Creates an empty specialized sequence. If `E` is statically known to be a specializable (primitive) type,
	  * a proper specialized subclass of `S[E]` will be returned. However, when used from a generic type
	  * where `E` is not instantiated, a generic, erased version will be picked regardless of the 'runtime'
	  * value of `E`
	  *
      * @tparam E element type of the sequence
      * @return an empty sequence.
	  */
	@inline final override def empty[@specialized(Elements) E]: S[E] = using(Specialized.erasedArray[E], 0, 0)
	
	/** Creates an empty specialized sequence proper for the given element type.
	  * Returned sequence will be a specialized variant of `S[E]` iff `elementType` is a syntetic class
	  * reresenting a specializable primitive (such as `Integer.TYPE` and other java primitives with the exception of `void`).
      * @param elementType exact component type of the backing array of the created seqeuence.
      * @tparam E element type of the created sequence (and array)
      * @return an empty sequence backed by an empty array with component type equal to the given class.
	  */
	@inline final def emptyOfClass[E](elementType :Class[E]) :S[E] = emptyOf(ClassTag(elementType))
	
	
	/** Creates an empty specialized sequence based on implicit class information for `E`.
	  * Returned sequence will be a specialized variant of `S[E]` iff `E` is an inbuilt scala 'AnyVal' type.
	  * @tparam E element type of the created sequence (and array)
	  * @return an empty sequence backed by an empty array with component type equal to the given class.
	  */
	@inline final def emptyOf[E :ClassTag] :S[E] = ForArray(new ArrayBounds(new Array[E](0)))(Specialized.of[E])


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
	@inline final def of[E <:AnyVal :ClassTag](size :Int) :S[E] = {
		val buffer = new Array[E](size)
		if (!buffer.getClass.getComponentType.isPrimitive)
			throw new IllegalArgumentException(s"$this.of[${classTag[E]}]: not a primitive component type")
		shared(ArrayBounds.share[E](buffer))
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
	@inline final def of[E](size :Int, value :E)(implicit elementType :ClassTag[E]=ClassTag(Specialized.UnboxedClass(value.getClass))) :S[E] =
		shared(ArrayBounds.share[E](arrayFill(new Array[E](size), value)))
	
	
	/** Create a specialized sequence backed by an array of the given type, initialized by runtime default value for `E`.
	  * This method is intended to be used only for primitive types, and calling it for user-defined value types
	  * will result in an exception. Use the variant with an explicit default value if you wish to do so.
	  * @param size the size of the created sequence.
	  * @tparam E an inbuilt scala 'AnyVal' subtype.
	  * @return a specialized sequence backed by the array of the given length.
	  * @see [[of(Int, _)]]
	  */
	def ofClass[E<:AnyVal](elementType :Class[E], size :Int) :S[E] = of(size)(ClassTag(elementType))
	
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
	def ofClass[E](elementType :Class[E], size :Int, value :E) :S[E] = of(size, value)(ClassTag(elementType))


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


	protected[seqs] def shared[E](contents :ArrayBounds[E]) :S[E] = ForArray(contents)(contents.specialization)

	
	private[this] final val ForArray = new Specialize.With[S, ArrayBounds] {
		override def specialized[@specialized E: Specialized](param: ArrayBounds[E]): S[E] =
			using(param.array, param.start, param.length)
	}


	/** Delegates to this factory's [[ArrayViewFactory#using]], which creates a new array view of corresponding specialization
	  * and parameters. Doesn't validate input and trusts the caller to be in an appropriate specialization context, hence access restriction.
	  */
	@inline final private[palimpsest] def view[@specialized(Elements) E] (array :Array[E], offset :Int, length :Int) =
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
		specializedBuilder[E]
	
	/** A builder of a sequence backed by an array with component type `E` as defined by implicit `ClassTag`,
	  * but specialized based on the specialization context of the caller.
	  * Created builder, and the resulting sequence, will be specialized only if a specialized variant
	  * of this method is called, due to `E` being statically known to be a specializable type,
	  * or being called from code already specialized for `E`.
	  * This means that it is possible to create a specialized sequence backed by a boxed type and vice versa,
	  * an erased sequence backed by a primitive array, if the class tag and specialization information don't coincide.
	  * While both cases will produce completely valid sequences, it is usually not what you want. For this reason,
	  * prefer generally either [[newBuilder]] or [[fitBuilder]] over this.
      * @tparam E
      * @return
	  */
	@deprecated("use fitBuilder", "palimpsest")
	def specializedBuilder[@specialized(Elements) E](implicit special :Specialized[E]) :FitBuilder[E, S[E]] =
		new ArraySeqBuilder[E](special.emptyArray.asInstanceOf[Array[E]])
	
	/** A builder for a sequence backed by an array, which specialization - and resulting component type - are given
	  * implicitly. Note that there will ''always'' be an implicit value for `Specialized[E]`, at worst representing
	  * a completely erased sequence, backed by an array of 'AnyRef'. However, if any information about type `E`
	  * is available at the calling point - such as a `ClassTag`, `TypeTag`, or specialized calling code, created
	  * builder and resulting sequence will be specialized accordingly.
	  * @tparam E element type of the sequence
	  * @param specialization implicit specialization information used to determine both
	  *                       proper specialized variant of this sequence and backing array component type.
	  */
	@inline final override def fitBuilder[E](implicit specialization :Specialized[E]) :FitBuilder[E, S[E]] =
		specialization.call(NewBuilder)
	//todo: rename ^this to build[E]
	
//	/** Generic, unspecialized builder of associated sequence type.
//	  * Always prefer [[newBuilder]] or [[fitBuilder]] whenether any information about element type is present.
//	  * This method exists specifically as the target of unspecialized calls to [[SpecializedTraversableTemplate#genericBuilder]]
//	  * from unspecialized `CanBuildFrom` factories when operating on specialized sequences through standard, unspecialized
//	  * scala interfaces. For this reason `FitBuilder` provides two methods which can be used to obtain a specialized
//	  * version. One is [[FitBuilder#fit]], which will try to provide an appropriate specialized version of itself,
//	  * the other is [[FitBuilder#source]], wich is a multipurpose identifier allowing colllections to recognize their 'own'
//	  * builders, if they so implement it.
//	  *
//	  * @return an erased version of the builder for element type `E`.
//	  */
//	override def genericBuilder[E]: FitBuilder[E, S[E]] = new SharedArrayBuilder[E](Specialized.erasedArray)
	
//	protected def erasedBuilder[E] :FitBuilder[E, S[E]]
	
	/** Builder factory method enforcing the specialization of the resulting sequence based on implicit class
	  * information for `E`. Returned builder - and built sequence - will be specialized only if `ClassTag[E]`
	  * represents a specializable primitive.
      *
	  * @param elementType backing array component type, used also to determine proper specialization of the sequence.
	  * @tparam E element type of the sequence
	  * @return a specialized builder proper for the given element type.
	  */
	@inline final def as[E](implicit elementType :ClassTag[E]) :FitBuilder[E, S[E]] = //CustomBuilder(elementType)
		NewBuilder()

	/** Builder for a sequence backed by an array of the given component type. Returned builder - and built sequence -
	  * will be specialized if and only if `elementType` is a java primitive (such as `Integer.TYPE`, not `Integer.class`).
	  * @param elementType component type of the array used by the builder and the final sequence.
	  * @return a specialized builder proper for the given element type.
	  */
	@inline final def asClass[E](elementType :Class[E]) :FitBuilder[E, S[E]] =
		Specialized.ofClass(elementType).call(NewBuilder)


	
	@inline final def newReverseBuilder[@specialized(Elements) E] :FitBuilder[E, S[E]] = new ReverseArraySeqBuilder[E]()
	
	@inline final def buildReversed[E](ofType :Specialized[E]) :FitBuilder[E, S[E]] = ofType.call(NewReverseBuilder)
	
	protected[this] final val NewReverseBuilder :Specialize[SpecializedBuilder] = new Specialize[SpecializedBuilder] {
		override def specialized[@specialized E : Specialized]: SpecializedBuilder[E] = newReverseBuilder[E]
	}


	
	
	implicit def CanBuildArray[E <: AnyRef :ClassTag] :CanFitFrom[S[_], E, S[E]] =
		new CanBuildArray[E]


	protected class CanBuildArray[@specialized E :ClassTag]
		extends CanFitFrom[S[_], E, S[E]] with CanBuildFrom[S[_], E, S[E]]
	{
		//intermediate method necessary, as `def specialization` is not specialized
		private[this] def mySpecialization = Specialized[E]
		
		override def specialization: Specialized[E] = mySpecialization

		override def elementType = classTag[E].runtimeClass

		override def apply(from: S[_]): FitBuilder[E, S[E]] =
			from.specializedBuilder[E](mySpecialization)

		override def apply(): FitBuilder[E, S[E]] =
			specializedBuilder[E](mySpecialization)

		override def canEqual(that :Any) = that.isInstanceOf[CanBuildArray[_]]

		override def equals(that :Any) = that match {
			case cbf :CanBuildArray[_] => (cbf eq this) || elementType==cbf.elementType && super.equals(cbf)
			case _ => false
		}

		override def toString = s"$outer.CBF[$classTag]"
	}



	protected class ArraySeqBuilder[@specialized(Elements) E](protected[this] final var array :Array[E])
		extends IterableFoundation[E, SharedArrayBuffer[E]] with FitBuilder[E, S[E]] with DefaultArrayBuffer[E]
	{
		
		def this()(implicit elementType :ClassTag[E]) = this(new Array[E](0))
		
		protected[this] final var len = 0
		protected[seqs] final var offset=0
		override protected var unmodifiable: Boolean = false
		
		/** Overriden due to conflict in inheritance. */
		override protected[this] def mySpecialization: Specialized[E] = Specialized[E]
		
		override def result(): S[E] = {
			val res = outer.using(array, offset, length)
			unmodifiable = true
			res
		}
		
//		override def ++=(xs: TraversableOnce[E]): ArraySeqBuilder.this.type = super.++=(xs)
		
		override def sizeHint(targetSize :Int) :Unit =
			if (targetSize>array.length)
				reserve(targetSize-array.length)
		
		override def count: Int = length
		
		override def source: Any = outer
		
		override def typeHint[L <: E](implicit specialization: Specialized[L]): FitBuilder[E, S[E]] =
			if (length>0 || specialization =:= mySpecialization) this
			else if (specialization.isValueType)
				specialization.asInstanceOf[Specialized[E]].call(NewBuilder)
			else {
				this
			}
		
		override def stringPrefix = s"$outer.Builder[$mySpecialization]"
			
	}
	
	protected class ReverseArraySeqBuilder[@specialized(Elements) E](buffer :GrowingArrayBuffer[E] = new GrowingArrayBuffer[E]())
		extends FitBuilder[E, S[E]]
	{
		override def addOne :E=>Unit = { e :E => e +=: buffer }
		
		override def +=(elem: E): this.type = { elem +=: buffer; this}
		
		override def count: Int = buffer.length
		
		override def result(): S[E] = {
			buffer.freeze()
			outer.using(buffer.arr, buffer.offset, buffer.length)
		}
		
		override def clear(): Unit = buffer.clear()
		
		override def sizeHint(expect: Int): Unit =
			if (buffer.arr.length<expect)
				buffer.reserveFront(expect - buffer.offset - buffer.length)
		
		override def toString = s"$outer.reverseBuilder[$mySpecialization]"
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
	@inline def Acc[E :Specialized] :ArrayView[E] = ArrayPlus.Acc[E]

	override protected def using[@specialized(Elements) E](array: Array[E], offset: Int, length: Int): ArrayView[E] =
		SharedArray.view(array, offset, length)
	

	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ArrayView[_], E, ArrayView[E]]): CanBuildFrom[ArrayView[_], E, ArrayView[E]] =
		fit.cbf
}




