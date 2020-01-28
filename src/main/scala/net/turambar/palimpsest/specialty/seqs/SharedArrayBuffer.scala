package net.turambar.palimpsest.specialty.seqs


import java.lang.System.arraycopy

import scala.annotation.{tailrec, unspecialized}
import scala.collection.generic.CanBuildFrom
import scala.collection.{mutable, IndexedSeqLike}
import scala.reflect.ClassTag
import net.turambar.palimpsest.specialty
import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.{CloneableIterable, FitCompanion, IterableFoundation, SpecializableIterable}
import net.turambar.palimpsest.specialty.{arrayFill, newArray, ofKnownSize, ItemTypes, FitTraversableOnce, RuntimeType}
import net.turambar.palimpsest.specialty.seqs.SharedArrayBuffer.nextCapacity







/** Base trait for specialized buffer implementations backed by an array.
  * As this trait is specialized regarding its element type, the exact type of the underlying array
  * will differ between specialized versions and, for `AnyRef` subtypes,
  * it might be any type `Array[T] forSome { type T&gt;:E &lt;:AnyRef }`. While this class manages
  * the buffer size itself, the responsibility and method (if any) of extending or truncating the underlying array
  * is left to subclasses.
  *
  * @tparam E element type before erasure
  */
trait SharedArrayBuffer[@specialized(ItemTypes) E]
//	extends ArrayBufferFoundation[E]
	extends FitBuffer[E] with mutable.BufferLike[E, SharedArrayBuffer[E]]
	   with SharedArray[E] with ArrayViewLike[E, SharedArrayBuffer[E]] with SpecializableIterable[E, SharedArrayBuffer]//SharedArrayLike[E, SharedArrayBuffer] //with ArrayBufferLike[E, SharedArrayBuffer]
	   with ValSeqLike[E, SharedArrayBuffer[E]] with CloneableIterable[E, SharedArrayBuffer[E]]
{

	//todo: extract variables to an erased base class ?
	protected[seqs] def arr_=(a :Array[E]) :Unit = array = a
	protected[this] var array :Array[E]
	protected[palimpsest] var headIdx :Int
	protected[this] var len :Int //todo: replace len with endIdx
//	protected var baseOffset :Int



	@inline final def length :Int = len
//	@inline final protected[this] def end = headIdx + len


	override protected def section(from: Int, until: Int): SharedArrayBuffer[E] =
		new LentArrayBuffer[E](array, headIdx+from, until-from, headIdx, headIdx+length)




	override def ++=(xs: TraversableOnce[E]): this.type = xs match {
		case _ if xs.isEmpty => this

		case fit :FitTraversableOnce[E] =>
			this ++= fit

		case _ if ofKnownSize(xs) =>
			directAppend(xs)
			this
		case _ =>
			var a = array
			var offset = headIdx
			var i = offset + len; var nextcheck = i
			for(elem <- xs) {
				if (i==nextcheck) {
					nextcheck = reserve()
					a = array
					i += headIdx-offset //in case reserve() shifted the contents
					offset = headIdx
				}
				a(i) = elem
				i += 1
			}
			len = i-offset
			this
	}


	protected def directAppend(elems :TraversableOnce[E]) :Unit = {
		val count = elems.size
		reserve(count)
		elems.copyToArray(array, headIdx + len)
		len += count
	}


	//can be extracted to unspecialized class
	override def ++=:(xs: TraversableOnce[E]): this.type = xs match {
		case _ if xs.isEmpty => this
		case items :FitTraversableOnce[E] =>
			items ++=: this
		case _ if xs.isTraversableAgain && xs.hasDefiniteSize =>
			val added = xs.size
			reserveFront(added)
			headIdx -= added; len += added
			xs.copyToArray(array, headIdx)
			this
		case _ =>
			val prefix = newBuffer
			prefix ++= xs
			prefix ++=: this
			this

	}



	override def insertAll(idx: Int, elems: Traversable[E]): Unit =
		if (idx<0 || idx>len)
			throw new IndexOutOfBoundsException(stringPrefix+"("+idx+")")
		else if (elems.nonEmpty)
			 if (ofKnownSize(elems)) {
				 val added = elems.size
				 shiftAside(idx, added)
				 elems.copyToArray(array, headIdx + idx)
			 } else  {
				 val tail = dropSuffix(idx, len + 1)
				 this ++= elems
				 this ++= tail
			 }



	override def remove(n: Int, count: Int): Unit = {
		val removed = count min (len - n) max 0
		if (n < 0 || n > len)
			throw new IndexOutOfBoundsException(s"$stringPrefix[$length].remove($n, $count)")
		else if (removed > 0)
			shiftIn(n, count)

	}




	override def +=(elem: E): this.type = {
		reserve()
		array(headIdx + len) = elem
		len += 1
		this
	}

	override def +=(elem1: E, elem2: E, elems: E*): this.type = {
		if (ofKnownSize(elems))
			reserve(elems.size + 2)
		else
	        reserve(2)
		val hd = headIdx; val size = len; val end = hd + size
		array(end) = elem1
		array(end + 1) = elem2
		len = size + 2
		this ++= elems
	}


	protected[this] def appendAll(elems :FitTraversableOnce[E]) :Unit =
		if (elems.hasFastSize) {
			directAppend(elems)
		} else {
			var a = array
			var o = headIdx
			var i = o + len; val it = elems.toIterator
			var nextcheck = i
			while(it.hasNext) { //we are using an iterator to avoid boxing of function calls
				if (i == nextcheck) { //call to reserve() requires several static forwarders
					nextcheck = reserve()
					a = array
					i += headIdx - o //reserve might have shift the contents
					o = headIdx
				}
				a(i) = it.next()
				i += 1
			}
			len = i - o
		}

	override def ++=(elems: FitTraversableOnce[E]): this.type = { appendAll(elems); this }


	override def +=:(elem: E): this.type = {
		reserveFront()
		val hd = headIdx - 1
		headIdx = hd
		array(hd) = elem
		len += 1
		this
	}




	override def -=(x: E): this.type = {
		val idx = indexOf(x)
		if (idx >= 0) remove(idx)
		this
	}

	override def -=(elem1: E, elem2: E, elems: E*): this.type =
		minus(FitSeq.two(elem1, elem2), elems)

	override def --=(xs: TraversableOnce[E]): this.type = minus(FitSeq.Empty, xs)



	override def remove(n: Int): E = {
		val removed = apply(n)
		shiftIn(n, 1)
		removed
	}



	override def clear(): Unit = {
		len = 0
	}

	/** Lazybones variant of `clear()` returning this instance. */
	@unspecialized
	final def cleared() :this.type = {
		clear(); this
	}


	@unspecialized
	protected[this] def minus(elems1 :FitSeq[E], elems2 :TraversableOnce[E]) :this.type = {
		val removedIndices = indicesOf(elems1, elems2)
		if (removedIndices.nonEmpty) {
			val cuts = removedIndices.toFitSeq.sorted
			copyWithout(cuts, array, headIdx)
			len -= removedIndices.size
		}
		this
	}

	@unspecialized
	protected[this] def copyWithout(removedIndices :FitSeq[Int], toArray :Array[E], toOffset :Int) :Unit = {
		@tailrec def copy(fromEnd :Int, cutNo :Int, toEnd :Int) :Unit =
			if (cutNo < 0) {
				val hd = headIdx; val count = fromEnd - hd
				arraycopy(array, hd, toArray, toEnd - count, count)
			} else {
				val removed = removedIndices(cutNo)
				val from = removed + 1
				val count = fromEnd - from
				val to = toEnd - count
				arraycopy(array, from, toArray, to, count)
				copy(removed, cutNo - 1, to)
			}
		val cuts = removedIndices.length; val len = length
		copy(headIdx + len, cuts - 1, toOffset + len - cuts)
	}



	/** Callback method to be implemented by subclasses called as a result of inserting new content between existing
	  * elements in this buffer when it is not possible to simply shift the suffix due to lack of space in the underlying array
	  * or unknown number of elements to be inserted.
	  * This methods drops in place all elements in this buffer after index `splitIndex` (in the sequence, not the underlying array),
	  * adjusting `this.length` accordingly, and returns a sequence containing the dropped suffix - that is
	  * all elements between `splitIndex` and `length`. Returned sequence and `this` should '''not''' share an underlying array -
	  * any modification of `this` should not affect the contents of the returned sequence.
	  * Additionally, it should ensure required capacity in this buffer: (`offset+requiredCapacity <= array.length)`.
	  * Subclasses are free to juggle the contents as they see fit, arbitrarily modifying `offset`, `length` and `array`
	  * in order to do so; if for any reason it is impossible, an exception should be thrown.
	  *
	  * @param splitIndex index in this sequence at which new elements will be inserted.
	  * @param requiredCapacity final required minimal size of the buffer defined as number of valid array indices after `offset`
	  * @return a sequence independent on this containing elements `splitIndex..length` of this buffer.
	  */
	@unspecialized
	protected[this] def dropSuffix(splitIndex :Int, requiredCapacity :Int=len) :ArrayView[E] = {
		val hd = headIdx; val size = len
		val tail =
			if (splitIndex >= size / 2 && array.length - hd >= requiredCapacity) {
				//append to current array, copy suffix from splitIndex to a new array
				val remainder = size - splitIndex
				if (remainder>0) {
					val copy = newArray[E](array.getClass.getComponentType, remainder)
					arraycopy(array, hd + splitIndex, copy, 0, remainder)
					newArrayView(copy, 0, remainder)
				}else
					ArrayView.Empty
			} else {
				//copy prefix until splitIndex into a new array and leave the suffix in current array
				val old = array
				array = newArray[E](old.getClass.getComponentType, math.max(headIdx + requiredCapacity, array.length))
				arraycopy(old, hd, array, hd, splitIndex)
				newArrayView(old, hd + splitIndex, size - splitIndex)
			}
		len = splitIndex
		tail
	}





	/** Create space for new elements in the middle of the buffer.
	  * This is a callback from [[SharedArrayBuffer#insertAll]] which allows subclasses
	  * to implement their own buffer management strategy. After the function returns, `length`
	  * should be increased by `count` and both `array` and `offset` be adjusted to reflect the widening of the buffer,
	  * with elements currently at indices `[0..idx)` occupying cells `[offset..offset+idx)` in the underlying array,
	  * and current elements from `[idx..length)` occupying cells `[offset+idx+count..offset+length)`.
	  * If for any reason it is impossible to create a continuous gap at these indices, including possible reallocation
	  * of the underlying array, an exception should be thrown.
	  * @param idx index in the buffer where the first element will be inserted
	  * @param count number of inserted elements/size of the created gap.
	  */
	protected[this] def shiftAside(idx :Int, count :Int) :Unit = { //unspecialized
		val hd = headIdx; val size = len
		val spaceBack = array.length - hd - size
		if (idx != size || spaceBack < count) //otherwise we simply append
			if (idx == 0 && hd >= count) {
				headIdx = hd - count
			} else if (hd >= count && (idx <= size / 2 || spaceBack < count)) {
				val pos = hd - count
				arraycopy(array, hd, array, pos, idx)
				headIdx = pos
			} else if (spaceBack >= count) {
				arraycopy(array, hd + idx, array, hd + idx + count, size - idx)
			} else {
				if (idx <= size / 2) {
					val capacity = nextCapacity(size, count, spaceBack)
					var first = capacity - size - count
					if (first > spaceBack)
						first -= spaceBack
					val a = newArray[E](array.getClass.getComponentType, capacity)
					arraycopy(array, hd, a, first, idx)
					arraycopy(array, hd + idx, a, first + idx + count, size - idx)
					array = a
					headIdx = first
				} else {
					val capacity = nextCapacity(size, count, hd)
					var first = hd
					if (count > capacity - hd - size) {
						first = hd
						headIdx = hd
					}
					val a = newArray[E](array.getClass.getComponentType, capacity)
					arraycopy(array, hd, a, first, idx)
					arraycopy(array, hd + idx, a, first + idx + count, size - idx)
					array = a
				}
			}
		len = size + count
	}



	/** Cover the gap created by removal of elements from the buffer by shifting remaining items into their place.
	  * This is a callback from [[SharedArrayBuffer#remove(Int, Int)]] which allows the subclasses to execute
	  * their strategy of buffer management. The function should shift elements right, left or both
	  * to eliminate the gap in indices `[idx..idx+count)` and restore the continuity of data.
	  * After the execution of this method, `len` and `offset` should be adjusted to reflect performed changes.
	  * @param idx public index in the buffer of the beginning of the gap
	  * @param count length of the gap.
	  */
	protected[this] def shiftIn(idx :Int, count :Int) :Unit = //unspecialized
		if (idx == 0) {
			headIdx += count
			len -= count
		} else if (idx + count == len)
			len -= count
		else if (idx < (len - idx - count))
			shiftRight(idx, count)
		else
			shiftLeft(idx, count)


	/** Callback from `remove` methods shifting back the suffix to replace removed elements within the array.
	  * This method should copy in place elements `downto+by..length` to positions `downto..length-by` and reduce
	  * length accordingly.
	  * @param downto index of the first removed element
	  * @param by number of elements removed at that index.
	  */
	protected[this] def shiftLeft(downto :Int, by :Int) :Unit = { //unspecialized
		val from = downto + by
		arraycopy(array, headIdx+from, array, headIdx+downto, len-from)
		len -= by
	}

	protected[this] def shiftRight(from :Int, upby :Int) :Unit = { //unspecialized
		arraycopy(array, headIdx, array, headIdx+upby, from)
		headIdx += upby
		len -= upby
	}



	/** Ensure available space for `capacity` elements after the current contents of this buffer.
	  * Might involve allocating a new array or moving elements. This method is called whenever
	  * new elements are appended to the buffer, and any pending change of the bounds (`offset` and `length`) of the buffer
	  * triggers a call to either this method or [[SharedArrayBuffer#reserveFront]] in order to allow subclasses to
	  * implement their own buffer management/allocation strategy.
	  * If this method returns without an exception, it is assumed that `offset+length+capacity <= array.length`.
	  * @param required non-negative extra space needed after current end of the buffer (zero denoting simple permission check).
	  * @return index in the array past the new value of `offset+length` up to which (exclusively)
	  *         this buffer can write within a single operation without a further check of capacity with this method.
	  */
	def reserve(required :Int=1) :Int = {
		val a = array
		val reserved = headIdx
		val want = reserved + len
		if (required > a.length - want) {
			val capacity = SharedArrayBuffer.nextCapacity(len, required, reserved)
			val extended = newArray[E](a.getClass.getComponentType, capacity)
			if (required > capacity - want) {
				arraycopy(a, reserved, extended, 0, len)
				headIdx = 0
			} else
				arraycopy(a, reserved, extended, headIdx, len)
			array = extended
		}
		array.length
	}

	/** Ensure available space for `capacity` elements before the current contents of this buffer.
	  * Might involve allocating a new array or moving elements. This function is called whenever
	  * new elements are prepended to the buffer, and no change to this buffer's bounds is made without
	  * a call to either this method or [[SharedArrayBuffer#reserve]] in order to allow subclasses to
	  * implement their own buffer allocation/management strategy.
	  * If this method returns without an exception, it is assumed that `offset >= capacity`.
	  * @param required non-negative extra space needed before `offset`, with zero used as a permission check.
	  */
	def reserveFront(required :Int=1) :Unit = {
		val a = array
		val free = headIdx
		if (required > free) {
			val size = len
			val current = free + size
			val capacity = SharedArrayBuffer.nextCapacity(size, required, a.length - current)
			val extended = newArray[E](a.getClass.getComponentType, capacity)
			if (required > capacity - current) {
				val hd = capacity - size
				arraycopy(a, free, extended, hd, size)
				headIdx = hd
			} else
				arraycopy(array, headIdx, extended, capacity-len, len)
			array = extended
		}
	}




	/** A quick access to a specialized sequence backed by the given section of the array for subclasses.
	  * Created for the benefit of non-specialized methods. 
	  */
	protected[this] def newArrayView(array :Array[E], offset :Int, length :Int) :ArrayView[E] = //specialized
		SharedArray.view(array, offset, length)




	override def companion: FitCompanion[SharedArrayBuffer] = SharedArrayBuffer
//
	override protected[this] def typeStringPrefix = "ArrayBuffer"

	override protected[this] def debugPrefix = "SharedArrayBuffer"
}






/** Full implementation of a buffer backed by a growing array.
  * Defined as a trait to allow variation of behaviour in subclasses (specialized class can't extend other specialized version of a class).
  *
  * @tparam E element type before erasure (and specialization)
  */
trait DefaultArrayBuffer[@specialized(ItemTypes) E]
	extends SharedArrayBuffer[E]
{
	
	
	/** A flag which, when set, prohibits this instance from any modification of the underlying array.
	  * This is usually a result of either buffer passing or initial reusing of the underlying array for
	  * slices of this buffer. All 'reserve' methods should honor it and resort to reallocation of data.
	  */
	protected var immutable :Boolean

	/** Prevents any future modification of this buffer and its underlying data. */
	protected[seqs] def freeze() :Unit = immutable = true


	override def reserve(required: Int=1): Int = {
		val a = array
		val reserved = headIdx
		val want = reserved + len
		if (required > a.length - want || immutable) {
			val capacity = SharedArrayBuffer.nextCapacity(len, required, reserved)
			val extended = newArray[E](a.getClass.getComponentType, capacity)
			if (required > capacity - want) {
				arraycopy(a, reserved, extended, 0, len)
				headIdx = 0
			} else
				  arraycopy(a, reserved, extended, headIdx, len)
			array = extended
			immutable = false
		}
		array.length
	}


	override def reserveFront(required: Int=1): Unit = {
		val a = array
		val free = headIdx
		if (required > free || immutable) {
			val size = len
			val current = free + size
			val capacity = SharedArrayBuffer.nextCapacity(size, required, a.length - current)
			val extended = newArray[E](a.getClass.getComponentType, capacity)
			if (required > capacity - current) {
				val hd = capacity - size
				arraycopy(a, free, extended, hd, size)
				headIdx = hd
			} else
				  arraycopy(array, headIdx, extended, capacity-len, len)
			array = extended
			immutable = false
		}
	}



	@unspecialized
	override protected[this] def dropSuffix(splitIndex :Int, capacity :Int=len) :ArrayView[E] =
		if (immutable) {
			val tail = newArrayView(array, headIdx + splitIndex, len - splitIndex)
			len = splitIndex
			reserve(capacity - len)
			tail
		} else
			super.dropSuffix(splitIndex, capacity)



	protected[this] override def shiftAside(idx :Int, count :Int) :Unit =
		if (immutable) {
			if (idx == 0) {
				reserveFront(count)
				headIdx -= count
			} else if (idx == len) {
				reserve(count)
			} else {
				val oldArray = array
				val hd = headIdx; val size = len
				val capacity = nextCapacity(size, count, oldArray.length - size) //preseerve both front and back.
				val dst = newArray[E](oldArray.getClass.getComponentType, capacity)
				var start = hd
				if (capacity - count - size < start) {
					headIdx = 0
					start = 0
				}
				arraycopy(oldArray, hd, dst, start, idx)
				arraycopy(oldArray, idx, dst, start + idx + count, size - idx)
				array = dst
				immutable = false
			}
			len += count
		} else
			super.shiftAside(idx, count)


	protected[this] override def shiftIn(idx :Int, count :Int) :Unit = //unspecialized
		if (idx == 0) {
			headIdx += count
			len -= count
		} else if (idx + count == len)
			len -= count
		else if (immutable) {
			val hd = headIdx; val size = len
			val from = idx + count
			val amount = size - from
			len = size - count
			val shift = newArray[E](array.getClass.getComponentType, size) //todo: determine optimal size
			arraycopy(array, hd, shift, 0, idx)
			arraycopy(array, hd + from, shift, idx, amount)
			immutable = false
			array = shift
		} else if (idx < (len - idx - count))
            shiftRight(idx, count)
		else
			shiftLeft(idx, count)




	override protected[this] def minus(elems1: FitSeq[E], elems2: TraversableOnce[E]): this.type = {
		val removedIndices = indicesOf(elems1, elems2)
		if (removedIndices.nonEmpty) {
			val cuts = removedIndices.toFitSeq.sorted
			if (immutable) {
				val to = newArray[E](array.getClass.getComponentType, len - removedIndices.size)
				copyWithout(cuts, to, 0)
				array = to
				headIdx = 0
				len = array.length
				immutable = false
			}else {
				copyWithout(cuts, array, headIdx)
				len -= removedIndices.size
			}
		}
		this
	}




	override def overwrite :FitBuffer[E] = {
		if (immutable)
			realloc()
		new LentArrayBuffer(array, headIdx, headIdx + len)
	}

	override def overwrite(start :Int, length :Int) :FitBuffer[E] =
		if (start < 0 || length < 0 || len - length < start)
			throw new IndexOutOfBoundsException(s"$stringPrefix{$size}.overwrite($start, $length)")
		else if (immutable) {
			realloc()
			new LentArrayBuffer[E](array, 0, 0, 0, len)
		} else
			new LentArrayBuffer[E](array, headIdx + start, headIdx + length)



	override def update(fromIndex :Int, value :E, count :Int) :Unit =
		if (count > 0) {
			if (fromIndex < 0 || fromIndex > len - count)
				throw new IndexOutOfBoundsException(s"$stringPrefix<$length>.update($fromIndex, ???, $count)")
			if (immutable)
				realloc()
			val start = headIdx + fromIndex
			arrayFill(array, value, start, start + count)
		}

	override def update(idx :Int, elems :TraversableOnce[E]) :Unit =
		if (idx < 0) throw new IndexOutOfBoundsException(s"$stringPrefix<$length>.update($idx, ...)")
		else if (idx < length) {
			if (immutable)
				realloc()
			elems.copyToArray(array, headIdx + idx, length - idx)
		}

	final override def update(idx: Int, elem: E): Unit =
		if (idx < 0 || idx >= length)
			throw new IndexOutOfBoundsException(idx.toString)
		else {
			if (immutable)
				reserve(0)
			array(headIdx + idx) = elem
		}

	protected[this] final override def set(idx :Int, elem :E) :Unit = {
		if (immutable)
			reserve(0)
		array(headIdx + idx) = elem
	}



	override def transform(f: E => E) :this.type = {
		if (immutable)
			realloc()
		var i = headIdx; val lim = i + len; val a = array
		while(i < lim) {
			a(i) = f(a(i)); i += 1
		}
		this
	}

	private def realloc() :Unit = {
		val hd = headIdx; val size = len
		val copy = newArray[E](array.getClass.getComponentType, size)
		arraycopy(array, hd, copy, 0, size)
		array = copy
		headIdx = 0
	}

	override protected def section(from: Int, until: Int): SharedArrayBuffer[E] =
//		new LentArrayBuffer[E](array, from, until, headIdx, length)
		new GrowingArrayBuffer[E](array, headIdx + from, until - from, immutable)


	override def toFitBuffer[U >: E : RuntimeType]: SharedArrayBuffer[U] =
		if (storageClass isAssignableFrom RuntimeType[U].runType)
			new GrowingArrayBuffer[E](array, headIdx, length).asInstanceOf[SharedArrayBuffer[U]]
		else new GrowingArrayBuffer[U]() ++= this

}


/** A specialized buffer implementation which will swap the underlying array for a larger instance once its capacity
  * is exceeded. This class does not alter or add any functions of its base class `DefaultArrayBuffer` and relies on it
  * fully for implementation.
  *
  * @param array initial underlying buffer, for erased type `E` may be any of its super types.
  * @param headIdx offset at which actual data starts in the array
  * @param len initial size of the buffer
  * @param immutable can this buffer modify the given array, or is it shared in read-only mode.
  * @tparam E element type before erasure.
  */
class GrowingArrayBuffer[@specialized(ItemTypes) E](
		protected[this] override final var array :Array[E],
		protected[palimpsest] override final var headIdx :Int,
		protected[this] override final var len :Int,
		protected override final var immutable :Boolean=false)
	extends IterableFoundation[E, SharedArrayBuffer[E]] with DefaultArrayBuffer[E]
{

	def this(array :Array[E]) = this(array, 0, array.length)

	def this(storageType :Class[E]) = this(Array.empty[E](ClassTag(storageType)))

	def this() = this(RuntimeType.arrayOf[E])
	
}







/** Factory for specialized growing buffers backed by arrays. */
object SharedArrayBuffer extends ArrayViewFactory[SharedArrayBuffer] {
	@inline private final val MaxLength = Int.MaxValue - 8
	@inline private final val MinCapacity = 16

//	/** Create an empty buffer of the given specialization. This is the same as `of[E]`. */
//	override def of[E :RuntimeType] :SharedArrayBuffer[E] = shared(new ArrayBounds[E])

	/** Create an empty buffer with the given capacity, using as the element type implicit class information for `E`.
	  * @param sizeHint predicted future size of the buffer
	  * @tparam E element type
	  * @return an empty buffer specialized accordingly to the given class tag.
	  */
	def ofCapacity[E :RuntimeType](sizeHint :Int) :SharedArrayBuffer[E] =
		shared(new ArrayBounds(RuntimeType.arrayOf[E](sizeHint), 0, 0))
	
	/** Create an empty buffer reusing the given array.
	  * Created buffer will start appending from index `0` in the array, but reallocate it when its capacity is exceeded.
	  * @param array an array to use as the initial storage for the buffer (which contents will be ignored)
	  * @tparam E element type.
	  * @return an empty buffer specialized accordingly to the component type of the given array.
	  */
	def upon[E](array :Array[E]) :SharedArrayBuffer[E] =
		shared(ArrayBounds.share(array, 0, 0))
	
	/** Create an empty buffer reusing the given array.
	  * Created buffer will start appending from index `offset`, which makes it possible to ensure free
	  * space for prepending elements.
	  * @param array an array to use as the initial storage for the buffer
	  * @param offset an index in the array which should map to index `0` of the returned buffer.
	  * @return an empty buffer specialized accordingly to the component type of the given array.
	  */
	def upon[E](array :Array[E], offset :Int) :SharedArrayBuffer[E] =
		shared(ArrayBounds.share(array, offset, 0))


	override protected def using[@specialized(ItemTypes) E](array: Array[E], offset: Int, length: Int): SharedArrayBuffer[E] =
		new GrowingArrayBuffer[E](array, offset, length)

	final val DefaultSize = 8
	
	
	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[SharedArrayBuffer[_], E, SharedArrayBuffer[E]]): CanBuildFrom[SharedArrayBuffer[_], E, SharedArrayBuffer[E]] =
		fit.cbf





	/** Calculates recommended new buffer capacity when growing the buffer. It tries to grow the buffer in powers of two,
	  * but will return a smaller value if maximum array length would be exceeded. The `reserved` parameter specifies
	  * the extra space, apart from the `size` elements, currently used by the buffer, which the algorithm should
	  * preserve if possible. If however `size + extra + reserved` exceeds maximum array length, a smaller value will
	  * be returned, although no lesser than `size + extra`.
	  * @param size current number of elements in the buffer; must be non-negative.
	  * @param extra number of new elements to be added; must be non-negative;
	  * @param reserved number of reserved space in the buffer apart from the given `size`; must be non-negative.
	  * @return A number no lesser than `size + extra` and no greater than maximum possible array size.
	  * @throws IllegalStateException if `size + extra &gt; MaxLength`.        
	  */
	@inline private[seqs] final def nextCapacity(size :Int, extra :Int, reserved :Int) :Int = {
		import java.lang.Integer.highestOneBit
		if (extra > MaxLength - size)
			throw new IllegalStateException("Can't allocate an array of " + (size.toLong + extra) + " elements")
		val required = size + extra
		var capacity = highestOneBit(required) //largest power of two no greater than required
		if (capacity < required) capacity <<= 1
		if (capacity < 0 | capacity > MaxLength - reserved)  
			MaxLength //we can't fit size + extra + reserve, but we can fit size + extra at least
		else 
			capacity + reserved

	}

}

