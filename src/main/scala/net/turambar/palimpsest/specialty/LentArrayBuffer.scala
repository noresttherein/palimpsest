package net.turambar.palimpsest.specialty

import java.util

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.reflect.ClassTag

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation


/** A view of an `Array[E]` as a buffer. The buffer is 'lent' in the sense that can neither modify the underlying
  * array outside of the given range, nor exchange it for another instance, essentially guarantying that whoever
  * bestowed the underlying array on us will see all the changes made via this instance.
  *
  * @param array underlying array used for storage which real runtime element type might actually be a supertype of E (up to `Any/Object`).
  * @param offset index in the array where the data starts.
  * @param len number of elements in the array considered as initial contents.
  * @param lowerBound minimum index in the array which can't be exceeded by prepending elements to this buffer.
  * @param higherBound maximum index in the array which can't be exceeded by appending elements to this buffer.
  * @tparam E element type before erasure
  */
class LentArrayBuffer[@specialized(Elements) E] protected[specialty]
		(protected[this] final var array :Array[E], protected[specialty] final var offset :Int, protected[this] final var len :Int, lowerBound :Int, higherBound :Int)
	extends IterableFoundation[E, LentArrayBuffer[E]] with SharedArrayBuffer[E]
	        with SharedArrayLike[E, LentArrayBuffer] with mutable.BufferLike[E, LentArrayBuffer[E]]
{
	def this(array :Array[E]) = this(array, 0, 0, 0, array.length)


	@inline
	final private def capacity = higherBound-lowerBound

	override def reserve(required: Int): Int =
		if (capacity-len < required)
			throw new IndexOutOfBoundsException(s"Can't extend $stringPrefix past preconfigured index of $higherBound (capacity: ${higherBound-lowerBound})")
		else if (higherBound-end < required) {
			System.arraycopy(array, offset, array, lowerBound, len)
			offset = lowerBound
			higherBound
		} else higherBound

	override def reserveFront(required: Int): Unit =
		if (capacity-len < required)
			throw new IndexOutOfBoundsException(s"Can't extend $stringPrefix before preconfigured index of $lowerBound (capacity: ${higherBound-lowerBound})")
		else if (offset<required) {
			System.arraycopy(array, offset, array, higherBound - len, len)
			offset = higherBound-len
		}

	override protected[this] def dropSuffix(splitIndex: Int, required: Int): ArrayView[E] = {
		val tail = ArrayView.copy[E](array, splitIndex, len-splitIndex)
		len = splitIndex
		reserve(required)
		tail
	}


//	override protected[this] def factory: ArrayViewFactory[LentArrayBuffer] = LentArrayBuffer
	override def companion = LentArrayBuffer
	
	override protected def section(from: Int, until: Int): LentArrayBuffer[E] =
		new LentArrayBuffer(array, offset+from, until-from, lowerBound, higherBound)


	override protected[this] def typeStringPrefix = "LentBuffer"
}


object LentArrayBuffer extends ArrayViewFactory[LentArrayBuffer] {
	

	/** Creates an empty buffer of the given capacity, using an array of the type specified by implicit `ClassTag[E]`. */
	def emptyOf[E :ClassTag](capacity :Int) :LentArrayBuffer[E] =
		shared(ArrayBounds.share(new Array[E](capacity))).empty()

	/** Creates an empty buffer writing to the given array, starting from the first index. */
	def upon[E](buffer :Array[E]) :LentArrayBuffer[E] =
		shared(ArrayBounds.share(buffer)).empty()

	/** Creates an empty buffer using the given array limited by the given bounds.
	  * The buffer will start appending from `lowerBound` and will not grow past `upperBound-lowerBound` bytes.
	  */
	def upon[E](buffer :Array[E], lowerBound :Int, upperBound :Int=Int.MaxValue) :LentArrayBuffer[E] =
		shared(ArrayBounds.share(buffer, lowerBound, upperBound)).empty()

	/** Creates a `FixedArrayBuffer` using the given array, with content spanning `length` bytes from byte `offset`, which will not grow past the given bounds. */
	def apply[E](buffer :Array[E], offset :Int, length :Int, lowerBound :Int, upperBound :Int) :LentArrayBuffer[E] = {
		val slice = ArrayBounds.share(lowerBound, buffer, upperBound)
		val start = offset max slice.start min slice.end
		val len = length min (slice.end-start) max 0
		
		val res = shared(slice)
		res.trimStart(start-slice.start)
		res.remove(length, Int.MaxValue)
		res
	}


	/** Create an instance with lower bound and upper bounds defined by initial offset and size; must be adjusted to required size later. */
	override protected def using[@specialized(Elements) E](array: Array[E], offset: Int, length: Int): LentArrayBuffer[E] =
		new LentArrayBuffer[E](array, offset, length, offset, length)
	
	
	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[LentArrayBuffer[_], E, LentArrayBuffer[E]]): CanBuildFrom[LentArrayBuffer[_], E, LentArrayBuffer[E]] =
		fit.cbf
}