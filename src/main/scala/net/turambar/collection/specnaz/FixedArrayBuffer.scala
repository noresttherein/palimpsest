package net.turambar.collection.specnaz

import java.util

import net.turambar.collection.specnaz.SpecCompanion.{ArraySection, SpecBuilder}

import scala.collection.mutable
import scala.reflect.ClassTag


/** A view of an `Array[E]` as a buffer. The buffer is fixed in the sense that can neither modify the underlying
  * array outside of the given range, nor exchange it for another instance, essentially guarantying that whoever
  * bestowed the underlying array on us will see all the changes made via this instance.
  *
  * @param buffer underlying array used for storage which real runtime element type might actually be a supertype of E (up to `Any/Object`).
  * @param startIdx index in the array where the data starts.
  * @param initialSize number of elements in the array considered as initial contents.
  * @param lowerBound minimum index in the array which can't be exceeded by prepending elements to this buffer.
  * @param higherBound maximum index in the array which can't be exceeded by appending elements to this buffer.
  * @tparam E element type before erasure
  */
class FixedArrayBuffer[@specialized(Reified) E] protected[specnaz]
//		(protected[this] final var array :Array[E], protected[specnaz] final var offset :Int, protected[this] final var len :Int, min :Int, max :Int)
		(buffer :Array[E], startIdx :Int, initialSize :Int, lowerBound :Int, higherBound :Int)
	extends SharedArrayBuffer[E]
		with ArraySliceLike[E, FixedArrayBuffer] with mutable.BufferLike[E, FixedArrayBuffer[E]]
{
	def this(array :Array[E]) = this(array, 0, 0, 0, array.length)

	array = buffer
	offset = startIdx
	len = initialSize

	@inline
	final private def capacity = higherBound-lowerBound

	override protected def reserve(required: Int): Unit =
		if (capacity-len < required)
			throw new IndexOutOfBoundsException(s"Can't extend $stringPrefix past preconfigured index of $higherBound (capacity: ${higherBound-lowerBound})")
		else if (higherBound-end < required) {
			Array.copy(array, offset, array, lowerBound, len)
			offset = lowerBound
		}

	override protected def reserveFront(required: Int): Unit =
		if (capacity-len < required)
			throw new IndexOutOfBoundsException(s"Can't extend $stringPrefix before preconfigured index of $lowerBound (capacity: ${higherBound-lowerBound})")
		else if (offset<required) {
			Array.copy(array, offset, array, higherBound - len, len)
			offset = higherBound-len
		}

	override protected[this] def dropSuffix(splitIndex: Int, required: Int): SharedArray[E] = {
		val tail = SharedArray.copy[E](array, splitIndex, len-splitIndex)
		len = splitIndex
		reserve(required)
		tail
	}


	override protected[this] def factory: SharedArrayFactory[FixedArrayBuffer] = FixedArrayBuffer

	override protected def subseq(from: Int, until: Int): FixedArrayBuffer[E] =
		new FixedArrayBuffer(array, offset+from, until-from, lowerBound, higherBound)





	override protected[this] def typeStringPrefix = "FixedBuffer"
}


object FixedArrayBuffer extends SharedArrayFactory[FixedArrayBuffer] {


	/** Creates an empty buffer of the given capacity, using an array of the type specified by implicit `ClassTag[E]`. */
	def emptyOf[E :ClassTag](capacity :Int) :FixedArrayBuffer[E] =
		shared(ArraySection.share(new Array[E](capacity))).empty()

	/** Creates an empty buffer writing to the given array, starting from the first index. */
	def upon[E](buffer :Array[E]) :FixedArrayBuffer[E] =
		shared(ArraySection.share(buffer)).empty()

	/** Creates an empty buffer using the given array limited by the given bounds.
	  * The buffer will start appending from `lowerBound` and will not grow past `upperBound-lowerBound` bytes.
	  */
	def upon[E](buffer :Array[E], lowerBound :Int, upperBound :Int=Int.MaxValue) :FixedArrayBuffer[E] =
		shared(ArraySection.share(buffer, lowerBound, upperBound)).empty()

	/** Creates a `FixedArrayBuffer` using the given array, with content spanning `length` bytes from byte `offset`, which will not grow past the given bounds. */
	def apply[E](buffer :Array[E], offset :Int, length :Int, lowerBound :Int, upperBound :Int) :FixedArrayBuffer[E] = {
		val slice = ArraySection.share(lowerBound, buffer, upperBound)
		val start = offset max slice.start min slice.end
		val len = length min (slice.end-start) max 0
		
		val res = shared(slice)
		res.trimStart(start-slice.start)
		res.remove(length, Int.MaxValue)
		res
	}


	/** Create an instance with lower bound and upper bounds defined by initial offset and size; must be adjusted to required size later. */
	override protected def using[@specialized(Reified) E](array: Array[E], offset: Int, length: Int): FixedArrayBuffer[E] =
		new FixedArrayBuffer[E](array, offset, length, offset, length)


}