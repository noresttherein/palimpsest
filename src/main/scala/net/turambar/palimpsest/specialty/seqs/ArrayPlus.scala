package net.turambar.palimpsest.specialty.seqs

import java.lang.System.arraycopy

import scala.collection.generic.CanBuildFrom
import scala.collection.{breakOut, immutable, GenTraversableOnce}
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.{IterableFoundation, SpecializableIterable}
import net.turambar.palimpsest.specialty.seqs.StableSeq.MakeStableIndexed
import net.turambar.palimpsest.specialty.{concat, newArray, ofKnownSize, slowcopy, Elements, FitBuilder, FitCompanion, RuntimeType}

import scala.annotation.unspecialized

/** An immutable, specialized view on a section of an array with O(n) recursive concatenation/extension.
  * It is a variation of mutable buffer implementation backed by a growing array with amortized O(1) append,
  * using ownership passing to achieve ''effective'' immutability. A new instance obtained from the companion factory
  * always owns its buffer; first append/prepend operation writes the new data to the shared buffer and passes the
  * ownership of the suffix/prefix of the array to freshly created instance, providing adequate space exists in the
  * array. If appended/prepended data doesn't fit in the buffer, an appropriately larger array is allocated as in
  * a mutable growing buffer, and passed to the new instance after copying all data, with ownership of the old buffer
  * staying with the original sequence. Ownership of the prefix and suffix is tracked independently,
  *
  *
  * It is thus as efficient as its mutable counterpart in one single, but very common scenario, where the final sequence
  * is built by recursively growing an accumulator sequence, for example like:
  * {{{
  * (ArrayPlus.empty /: seqs){ (acc, elems) => acc :++ elems }
  * }}}
  * As such, its mainly a means for more convenient building of the final sequence without an intermediate
  * builder / linked list, rather than an implementation suited towards common concatenation,
  * as only the result of the concatenation is guaranteed to be expandable in amortized constant time per element.
  * All intermediate values of the accumulator in the above case will cause buffer copying on subsequent expansion.
  *
  *
  * While neither the range, nor the array or its contents within the range of this sequence can be mutated
  * by either this instance or any external source, the contents of the array outside of the  given section
  * are not immutable. Whenever an element is appended/prepended or this instance is concatenated
  * with another sequence, if the backing array has enough space in the corresponding fragment
  * (preceding or succeeding this section) '''and''' this instance ''owns'' the corresponding section
  * of the array (that is, either its whole suffix or whole prefix), new content is simply copied to the
  * underlying array and a view over the whole, extended section is returned as the result sequence, now
  * becoming the whole owner of the underlying section.
  *
  * This class is both effectively immutable and thread safe; the only mutable state are ownership
  * flags which can only be changed from `true` to `false`.
  *
  * @author Marcin Mościcki
  */
sealed class ArrayPlus[@specialized(Elements) E] protected[seqs](
		buffer :Array[E],
		offset :Int,
		len :Int,
		/** Ownership flag granting this instance the right to write to the backing array past its index range. Single use only.*/
		private[this] var mutable :Boolean = false
	)
	extends IterableFoundation[E, ArrayPlus[E]] with MakeStableIndexed[E] with StableSeq[E]
	   with ArrayView[E] with ArrayViewLike[E, ArrayPlus[E]]
	   with SpecializableIterable[E, ArrayPlus]
{

	@unspecialized
	override def seq :ArrayPlus[E] = this

	override def companion: FitCompanion[ArrayPlus] = ArrayPlus



	@inline final override protected[this] def array :Array[E] = buffer

	@inline final override protected[palimpsest] def headIdx :Int = offset

	@inline final override def length :Int = len

	@inline final override protected def section(from: Int, until: Int): ArrayPlus[E] =
		new ArrayPlus[E](buffer, offset + from, until - from, false)


	override def clone(): ArrayPlus[E] =
		if (buffer.length>0 && buffer.length / len >= 2)
			new ArrayPlus(toArray, 0, len, true)
		else this
	

	/** Returns the value of the `mutable` flag and atomically sets it to false. This method will return true at most once. */
	@inline final private[this] def canPassArray :Boolean =
		mutable && synchronized {                   //once the flag is false it will always remain false, so either
			val res = mutable; mutable = false; res //we read false and it's correct, or true and we synchronize to be sure
		}



	@unspecialized
	override def +:[U >: E, That](elem: U)(implicit bf: CanBuildFrom[ArrayPlus[E], U, That]): That =
		bf match {
			case spec :CanFitFrom[_, _, _] if spec.honorsBuilderFrom =>
				prepend(elem, spec.elementType).asInstanceOf[That]
			case _ => bf(this) match {
				case builder :FitBuilder[_, _] if builder.origin eq ArrayPlus =>
					prepend(elem, builder.elementType).asInstanceOf[That]
				case builder =>
					builder sizeHint len + 1
					builder += elem ++= this
					builder.result()
			}
		}



	@unspecialized
	override def :+[U >: E, That](elem: U)(implicit bf: CanBuildFrom[ArrayPlus[E], U, That]): That =
		bf match {
			case spec :CanFitFrom[_, _, _] if spec.honorsBuilderFrom =>
				append(elem, spec.elementType).asInstanceOf[That]
			case _ => bf(this) match {
				case builder :FitBuilder[_, _] if builder.origin eq ArrayPlus =>
					append(elem, builder.elementType).asInstanceOf[That]
				case builder =>
					builder sizeHint len + 1
					builder += elem ++= this
					builder.result()
			}
		}



	override def ++[U >: E, That](that: GenTraversableOnce[U])(implicit bf: CanBuildFrom[ArrayPlus[E], U, That]): That =
		bf match {
			case _ if !ofKnownSize(that) =>
				concat(this, that.seq)(bf(this))
			case own :CanFitFrom[_, _, _] if own.honorsBuilderFrom =>
				append(that, own.elementType).asInstanceOf[That]
			case _ => bf(this) match {
				case own :FitBuilder[_, _] if own.origin eq ArrayPlus => //non-specialized cbf will produce an erased builder.
					append(that, own.elementType).asInstanceOf[That]
				case builder =>
					concat(this, that.seq)(builder)
			}
		}
	
	
	override def ++:[U >: E, That](that: TraversableOnce[U])(implicit bf: CanBuildFrom[ArrayPlus[E], U, That]): That =
		bf match {
			case _ if !ofKnownSize(that) =>
				concat(that.seq, this)(bf(this))
			case own :CanFitFrom[_, _, _] if own.honorsBuilderFrom =>
				prepend(that, own.elementType).asInstanceOf[That]
			case _ => bf(this) match {
				case own :FitBuilder[_, _] if own.origin eq ArrayPlus => //non-specialized cbf will produce an erased builder
					prepend(that, own.elementType).asInstanceOf[That]
				case builder =>
					concat(that.seq, this)(builder)
			}
		}





	/** Create a new collection of the same type (but possibly different array component type) containing the given
	  * element followed by all elements from this sequence. Uses the default growing/concatenation semantics.
	  * @param elem new element to add before all elements of this collection.
	  * @param elementType requested component type of the new array. May be ignored if `elem` can be assigned to current array.
	  */
	@unspecialized
	private final def prepend[U >: E](elem :U, elementType :Class[_]) :ArrayPlus[E] = {
		var capacity = buffer.length
		var newElementType = buffer.getClass.getComponentType
		//todo: better check, maybe use specialization?
		if (newElementType.isAssignableFrom(elementType) || newElementType.isAssignableFrom(elem.getClass)) {
			if (offset > 0) {
				if (canPassArray) {
					val newOffset = offset - 1
					buffer(newOffset) = elem.asInstanceOf[E]
					return new ArrayPlus[E](buffer, newOffset, len + 1, true)
				}
			} else if (len == 0 & capacity > 0 && canPassArray) {
				val newOffset = capacity - 1
				buffer(newOffset) = elem.asInstanceOf[E]
				return new ArrayPlus[E](buffer, newOffset, 1, true)
			}
		} else
			newElementType = elementType

		//caution: unsynchronized mutable field access! We don't much care though if we read stale 'true' value
		//caution: as the only purpose is guaranteeing O()) amortized first grow on any side.
		//caution: What we don't want is preserve the unused space on instances sliced from other sequences;
		//caution: in that case though the `mutable` field is always false since construction.
		val reserve = //free capacity at the back that needs preserving.
			if (mutable) capacity - offset - len
			else 0

		capacity = nextArrayLength(1, reserve)
		val newBuffer = newArray(newElementType, capacity).asInstanceOf[Array[E]]
		val newLength = len + 1
		var newOffset = capacity - newLength
		if (newOffset >= reserve) //may not hold if we reach the maximum array length limit
			newOffset -= reserve

		if (newElementType eq elementType)
			slowcopy(buffer, offset, newBuffer, newOffset + 1, len)
		else
	        arraycopy(buffer, offset, newBuffer, newOffset + 1, len)
		newBuffer(newOffset) = elem.asInstanceOf[E]

		new ArrayPlus[E](newBuffer, newOffset, newLength, true)
	}



	/** Create a new collection of the same type (but possibly different array component type) containing the given
	  * elements followed by all elements from this sequence. Uses the default growing/concatenation semantics.
	  * Implementation is almost the same as with a single element.
	  * @param elems new elements to add before all elements of this collection.
	  * @param elementType requested component type of the new array. May be ignored if `elem` can be assigned to current array.
	  */
	@unspecialized
	private final def prepend[U >: E](elems :GenTraversableOnce[U], elementType :Class[_]) :ArrayPlus[E] = {
		val extras = elems.size
		if (extras == 0)
			return this

		var capacity = buffer.length
		var newElementType = buffer.getClass.getComponentType
		//todo: better check, maybe use specialization?
		if (newElementType.isAssignableFrom(elementType)) {
			if (offset >= extras) {
				if (canPassArray) {
					val newOffset = offset - extras
					elems.copyToArray(buffer.asInstanceOf[Array[U]], newOffset)
					return new ArrayPlus[E](buffer, newOffset, len + extras, true)
				}
			} else if (len == 0 & capacity >= extras && canPassArray) {
				val newOffset = capacity - extras
				elems.copyToArray(buffer.asInstanceOf[Array[U]], newOffset)
				return new ArrayPlus[E](buffer, newOffset, extras, true)
			}
		} else
			  newElementType = elementType

		//caution: unsynchronized mutable field access! We don't much care though if we read stale 'true' value
		//caution: as the only purpose is guaranteeing O()) amortized first grow on any side.
		//caution: What we don't want is preserve the unused space on instances sliced from other sequences;
		//caution: in that case though the `mutable` field is always false since construction.
		val reserve = //free capacity at the back that needs preserving.
			if (mutable) capacity - offset - len
			else 0

		capacity = nextArrayLength(extras, reserve)
		val newBuffer = newArray(newElementType, capacity).asInstanceOf[Array[E]]
		val newLength = len + extras
		var newOffset = capacity - newLength
		if (newOffset >= reserve) //may not hold if we reach the maximum array length limit
			newOffset -= reserve

		if (newElementType eq elementType)
			slowcopy(buffer, offset, newBuffer, newOffset + len, len)
		else
			arraycopy(buffer, offset, newBuffer, newOffset + len, len)
		elems.copyToArray(newBuffer.asInstanceOf[Array[U]], newOffset)

		new ArrayPlus[E](newBuffer, newOffset, newLength, true)
	}



	/** Create a new collection of the same type (but possibly different array component type) containing
	  * all elements from this sequence followed by the given element. Uses the default growing/concatenation semantics.
	  * @param elem new element to add after all elements of this collection.
	  * @param elementType requested component type of the new array. May be ignored if `elem` can be assigned to current array.
	  */
	@unspecialized
	private final def append[U >: E](elem :U, elementType :Class[_]) :ArrayPlus[E] = {
		var newOffset = offset
		val elemOffset = newOffset + len
		var capacity = buffer.length
		var newElementType = buffer.getClass.getComponentType
		//todo: better check, maybe use specialization?
		if (newElementType.isAssignableFrom(elementType) || newElementType.isAssignableFrom(elem.getClass)) {
			if (elemOffset < capacity) {
				if (canPassArray) {
					buffer(elemOffset) = elem.asInstanceOf[E]
					return new ArrayPlus[E](buffer, offset, len + 1, true)
				}
			} else if (len == 0 & capacity > 0 && canPassArray) {
				buffer(0) = elem.asInstanceOf[E]
				return new ArrayPlus[E](buffer, 0, 1, true)
			}
		} else
			newElementType = elementType

		if (!mutable) //caution: unsynchronized mutable field access. See prepend for explanation
			newOffset = 0
		capacity = nextArrayLength(1, newOffset)
		val newBuffer = newArray(newElementType, capacity).asInstanceOf[Array[E]]
		val newLength = len + 1
		if (capacity - newLength < newOffset) //will happen if we reach maximum array length
			newOffset = 0

		if (elementType eq newElementType)
			slowcopy(buffer, offset, newBuffer, newOffset, len)
		else
			arraycopy(buffer, offset, newBuffer, newOffset, len)
		newBuffer(newOffset + len) = elem.asInstanceOf[E]

		new ArrayPlus[E](newBuffer, newOffset, len + 1, true)
	}



	/** Create a new collection of the same type (but possibly different array component type) containing all elements
	  * from this sequence followed by the given collection. Uses the default growing/concatenation semantics.
	  * Implementation is almost the same as with a single element.
	  * @param that new elements to add after all elements of this collection. Must have idempotent size method.
	  * @param elementType requested component type of the new array.
	  */
	@unspecialized
	private def append[U >: E](that: GenTraversableOnce[U], elementType :Class[_]) :ArrayPlus[E] = {
		val extras = that.size
		if (extras == 0)
			return this
		var newOffset = offset
		val thatOffset = newOffset + len
		var capacity = buffer.length
		var newElementType = buffer.getClass.getComponentType

		if (newElementType isAssignableFrom elementType) {
			if (capacity - thatOffset >= extras) {
				if (canPassArray) {
					that.copyToArray(buffer.asInstanceOf[Array[U]], thatOffset)
					new ArrayPlus(buffer, newOffset, len + extras, true)
				}
			} else if (len == 0 && capacity >= extras && canPassArray) {
				that.copyToArray(buffer.asInstanceOf[Array[U]])
				new ArrayPlus(buffer, 0, extras, true)
			}
		} else
			newElementType = elementType

		if (!mutable) //caution: unsynchronized mutable field access. See prepend(elem, class) for explanation
			newOffset = 0
		capacity = nextArrayLength(extras, newOffset)
		val newBuffer = newArray(newElementType, capacity).asInstanceOf[Array[E]]
		val newLength = len + extras
		if (capacity - newLength < newOffset)
			newOffset = 0

		if (elementType eq newElementType)
			slowcopy(buffer, offset, newBuffer, newOffset, len)
		else
			arraycopy(buffer, offset, newBuffer, newOffset, len)
		that.copyToArray(newBuffer.asInstanceOf[Array[U]], newOffset + len)

		new ArrayPlus(newBuffer, newOffset, newLength, true)
	}
	




	/** Computes the new array size when the current array won't fit required elements.
	  * This is complicated by the fact that the buffer can grow (and have free space) from both ends and we can't
	  * eliminate the free space on the opposite side to the elements added, or extending the buffer alternately
	  * from both sides would require reallocation with each operation. Neither can we always keep the padding,
	  * as instances which result from slicing a sequence rather than extending it can have arbitrary large unused space
	  * while not being held up to the amortized grow cost guarantee.
	  *
	  * The grow thus happens in terms of front and back capacities defined as the current size of this sequence plus
	  * the number of free array cells on the corresponding side. So, front capacity equals `headIdx + length`,
	  * while back capacity equals `array.length - headIdx`. When extending the buffer we consider the appropriate
	  * current capacity as the basis for grow calculation, but tack on the reserved space on the opposite side
	  * of the same size as in this instance if possible. The result is equivalent to dropping the opposite reserve,
	  * extending the buffer in the traditional manner and adding back the dropped empty section.
	  * @param extras number of extra elements which need to be added to either side of the sequence. Must be positive.
	  * @param reserved number of free cells on the opposite side to the planned extension which should be reserved
	  *                 in the new array for future grow. Must be non-negative.
	  * @return recommended length for the new array, guaranteed to hold all current elements plus `extras`.
	  * @throws IllegalStateException if `this.length + extras` exceeds the maximum sequence length
	  *                               (slightly smaller than Int.MaxValue).
	  */
	final private[this] def nextArrayLength(extras :Int, reserved :Int = 0) :Int = {
		val current = buffer.length
		var capacity = //new capacity discounting the reserved space
			if (current - reserved >= extras) //reallocation due to immutability or different component types
				reserved + (len + extras << 1) //smaller than current * 2, so can't overflow past the negative range
			else
	            (current - reserved) << 1 //like above, won't overflow past the negative Int range

		if (capacity < 0 | capacity > ArrayPlus.MaxLength) { //can't overflow past the negative Int range
			if (ArrayPlus.MaxLength - len < extras)
				throw new IllegalStateException("Maximum array length reached")
			else
				ArrayPlus.MaxLength
		} else {
			val keep = len + reserved //current size + free space on the opposite side that we want to keep
			if (capacity < ArrayPlus.MinCapacity)
				capacity = ArrayPlus.MinCapacity

			if (capacity - keep >= extras)
				capacity
			else { //doubling the capacity isn't enough
				if (ArrayPlus.MaxLength - keep < extras) //can't fit current elements and extras while retaining the reserve...
					if (ArrayPlus.MaxLength - len < extras) //can't fit all elements even with shrinking the reserve
						throw new IllegalStateException("Maximum array length reached")
					else
						ArrayPlus.MaxLength
				else
					keep + extras
			}
		}
	}



	override def typeStringPrefix: String = "Array+"
}








/** Factory for specialized sequences implemented as views on an a section of an array which pass
  * the ownership of the backing array on concatenation, making
  */
object ArrayPlus extends ArrayViewFactory[ArrayPlus] { factory =>

	@inline private final val MaxLength = Int.MaxValue - 8
	@inline private final val MinCapacity = 16


	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ArrayPlus[_], E, ArrayPlus[E]]): CanBuildFrom[ArrayPlus[_], E, ArrayPlus[E]] =
		fit.cbf



	protected[seqs] override def apply[E](contents: ArrayBounds[E]): ArrayPlus[E] = shared(contents.copy)


	protected def using[@specialized(Elements) E](array: Array[E], offset: Int, length: Int): ArrayPlus[E] =
		new ArrayPlus[E](array, offset, length, true)
	
}






