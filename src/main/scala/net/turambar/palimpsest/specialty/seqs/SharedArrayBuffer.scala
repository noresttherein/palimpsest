package net.turambar.palimpsest.specialty.seqs

import scala.annotation.{tailrec, unspecialized}
import scala.collection.generic.CanBuildFrom
import scala.collection.{IndexedSeqLike, mutable}
import scala.reflect.ClassTag
import net.turambar.palimpsest.specialty
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.IterableFoundation
import net.turambar.palimpsest.specialty.{ofKnownSize, ArrayBounds, Elements, FitCompanion, FitTraversableOnce, SpecializableIterable, Specialized}








/** Base trait for specialized buffer implementations backed by an array.
  * As this trait is specialized regarding its element type, the exact type of the underlying array
  * will differ between specialized versions and, for `AnyRef` subtypes,
  * it might be any type `Array[T] forSome { type T&gt;:E &lt;:AnyRef }`. While this class manages
  * the buffer size itself, the responsibility and method (if any) of extending or truncating the underlying array
  * is left to subclasses.
  *
  * @tparam E element type before erasure
  */
trait SharedArrayBuffer[@specialized(Elements) E]
//	extends ArrayBufferFoundation[E]
	extends FitBuffer[E] with mutable.BufferLike[E, SharedArrayBuffer[E]]
			with SharedArray[E] with SharedArrayLike[E, SharedArrayBuffer] //with ArrayBufferLike[E, SharedArrayBuffer]
//            with SpecializedTraversableTemplate[E, SharedArrayBuffer]
{



	protected[seqs] def arr_=(a :Array[E]) :Unit = array = a
	protected[this] var array :Array[E]
	protected[seqs] var headIdx :Int
	protected[this] var len :Int
//	protected var baseOffset :Int



	@inline final def length = len
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

	//can be extracted to unspecialized class
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
//			logicalOffset += added
			this
		case _ =>
			val prefix = newBuffer
			prefix ++= xs
			prefix ++=: this
			this

	}



	//can be extracted to unspecialized class
	override def insertAll(idx: Int, elems: Traversable[E]): Unit =
		if (idx<0 || idx>len)
			throw new IndexOutOfBoundsException(stringPrefix+"("+idx+")")
		else if (elems.nonEmpty)
			 if (specialty.ofKnownSize(elems)) {
				 val added = elems.size
				 shiftAside(idx, added)
				 elems.copyToArray(array, headIdx+idx)
			 } else  {
				 val tail = dropSuffix(idx, len+1)
				 this ++= elems
				 this ++= tail
			 }



	//can be extracted to unspecialized class
	override def remove(n: Int, count: Int): Unit = {
		val removed = count min (len - n) max 0
		if (n < 0 || n > len)
			throw new IndexOutOfBoundsException(s"$stringPrefix[$length].remove($n, $count)")
		else if (removed>0)
			shiftIn(n, count)

	}




	override def +=(elem: E): this.type = {
		reserve()
		array(headIdx+len) = elem
		len += 1
		this
	}

	override def +=(elem1: E, elem2: E, elems: E*): this.type = {
		if (specialty.ofKnownSize(elems))
			reserve(elems.size+2)
		else
	        reserve(2)
		array(headIdx+len) = elem1
		array(headIdx+len+1) = elem2
		len += 2
		this ++= elems
	}


	protected[this] def appendAll(elems :FitTraversableOnce[E]) :Unit =
		if (elems.hasFastSize) {
			directAppend(elems)
		} else {
			var a = array
			var o = headIdx
			var i = o+len; val it=elems.toIterator //todo: specialization
			var nextcheck = i
			while(it.hasNext) { //we are using an iterator to avoid boxing of function calls
				if (i==nextcheck) { //call to reserve() requires several static forwarders
					nextcheck = reserve()
					a = array
					i += headIdx-o //reserve might have shift the contents
					o = headIdx
				}
				a(i) = it.next()
				i += 1
			}
			len = i-o
		}

	override def ++=(elems: FitTraversableOnce[E]): this.type = { appendAll(elems); this }


	override def +=:(elem: E): this.type = {
		reserveFront()
		headIdx -= 1
		array(headIdx) = elem
		len += 1
		this
	}




	override def -=(x: E): this.type = {
		val idx = indexOf(x)
		if (idx>0) remove(idx)
		this
	}

	override def -=(elem1: E, elem2: E, elems: E*): this.type =
		minus(FitSeq.pair(elem1, elem2), elems)

	//can be extracted to unspecialized class
	override def --=(xs: TraversableOnce[E]): this.type = minus(Seq(), xs)



	override def remove(n: Int): E = {
		val removed = apply(n)
		shiftIn(n, 1)
		removed
	}



	//can be extracted to unspecialized class
	override def clear(): Unit = {
		len=0
	}

	/** Lazybones variant of `clear()` returning this instance. */
	def cleared() :this.type = {
		clear(); this
	}



	//can be extracted to unspecialized class
	protected[this] def minus(elems1 :Traversable[E], elems2 :TraversableOnce[E]) :this.type = {
		val removedIndices = indicesOf(elems1, elems2)
		if (removedIndices.nonEmpty) {
			val cuts = removedIndices.toList.sorted
			copyWithout(cuts, array, headIdx)
			len -= removedIndices.size
		}
		this
	}

	//can be extracted to unspecialized class
	@unspecialized
	protected[this] def copyWithout(removedIndices :List[Int], toArray :Array[E], toOffset :Int) :Unit = {
		@tailrec def copy(from :Int, cuts :List[Int], to :Int) :Unit =
			if (cuts.isEmpty)
				System.arraycopy(array, from, toArray, to, len-from)
			else {
				val nextRemoved = cuts.head
				val section = nextRemoved-from
				System.arraycopy(array, from, toArray, to, section)
				copy(nextRemoved+1, cuts.tail, to+section)
			}
		copy(headIdx, removedIndices, toOffset)
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
		val tail =
			if (splitIndex >= length/2 && array.length-headIdx >= requiredCapacity) {
				//append to current array, copy suffix from splitIndex to a new array
				val remainder = len-splitIndex
				if (remainder>0) {
					val copy = new Array[E](remainder)
					System.arraycopy(array, headIdx + splitIndex, copy, 0, remainder)
					newArrayView(copy, 0, remainder)
				}else
					ArrayView.Empty
			} else {
				//copy prefix until splitIndex into a new array and leave the suffix in current array
				val old = array
				array = new Array[E](math.max(headIdx + requiredCapacity, array.length))
				System.arraycopy(old, headIdx, array, headIdx, splitIndex)
				newArrayView(old, headIdx+splitIndex, len-splitIndex)
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
		import SharedArrayBuffer.nextCapacity
		val spaceBack = array.length-headIdx-len
		if (idx==len && spaceBack>=count)
			len += count
		else if (idx==0 && headIdx >= count) {
			headIdx -= count
			len += count
		} else if (headIdx >= count && (idx <= len / 2 || spaceBack < count)) {
			val pos = headIdx - count
			System.arraycopy(array, headIdx, array, pos, idx)
			headIdx = pos
			len += count
		} else if (spaceBack >= count) {
			System.arraycopy(array, headIdx+idx, array, headIdx+idx+count, len-idx)
			len += count
		} else {
			if (idx<=len/2) {
				val capacity = nextCapacity(headIdx+len, len+count) + spaceBack
				val first = capacity - spaceBack - len - count
				val a = new Array[E](capacity)
				System.arraycopy(array, headIdx, a, first, idx)
				System.arraycopy(array, headIdx+idx, a, first+idx+count, len-idx)
				array = a
				headIdx = first
				len += count
			} else {
				val capacity = nextCapacity(len+spaceBack, len+count) + headIdx
				val a = new Array[E](capacity)
				System.arraycopy(array, headIdx, a, headIdx, idx)
				System.arraycopy(array, headIdx+idx, a, headIdx+idx+count, len-idx)
				array = a
				len += count
			}
		}
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
		if (idx==0) {
			headIdx += count
			len -= count
		} else if (idx+count==len)
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
	//can be extracted to unspecialized class
	protected[this] def shiftLeft(downto :Int, by :Int) :Unit = { //unspecialized
		val from = downto+by
		System.arraycopy(array, headIdx+from, array, headIdx+downto, len-from)
		len -= by
	}

	protected[this] def shiftRight(from :Int, upby :Int) :Unit = { //unspecialized
		System.arraycopy(array, headIdx, array, headIdx+upby, from)
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
	  * @return index in the array past the new value of `offset+length` upto which (exclusively)
	  *         this buffer can write within a single operation without a further check of capacity with this method.
	  */
	def reserve(required :Int=1) :Int = { //unspecialized
		var capacity = array.length-headIdx
		val requested = len + required
		if (requested > capacity) {
			capacity = SharedArrayBuffer.nextCapacity(capacity, requested)
			val extended = new Array[E](headIdx + capacity)
			System.arraycopy(array, headIdx, extended, headIdx, len)
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
	def reserveFront(required :Int=1) :Unit = //unspecialized
		if (required > headIdx) {
			val current = headIdx + length
			var capacity = SharedArrayBuffer.nextCapacity(current, length+required)
			val extended = new Array[E](capacity + (array.length-current))   //we have an implicit class tag,
			System.arraycopy(array, headIdx, extended, capacity-len, len) //so it will turn out properly specialized
			array = extended
			headIdx = capacity-len
		}




	/** A quick access to a specialized sequence backed by the given section of the array for subclasses. */
	protected[this] def newArrayView(array :Array[E], offset :Int, length :Int) :ArrayView[E] = //specialized
		SharedArray.view(array, offset, length)




	override def companion: FitCompanion[SharedArrayBuffer] = SharedArrayBuffer
//
	override protected[this] def typeStringPrefix = "SharedBuffer"
}






/** Full implementation of a buffer backed by a growing array.
  * Defined as a trait to allow variation of behaviour in subclasses (specialized class can't extend other specialized version of a class).
  *
  * @tparam E element type before erasure (and specialization)
  */
trait DefaultArrayBuffer[@specialized(Elements) E]
	extends SharedArrayBuffer[E]
{
	
	
	/** A flag which, when set, prohibits this instance from any modification of the underlying array.
	  * This is usually a result of either buffer passing or initial reusing of the underlying array for
	  * slices of this buffer. All 'reserve' methods should honour it and resign to reallocation of data.
	  */
	protected var unmodifiable :Boolean
	
	/** Prevents any future modification of this buffer and its underlying data. */
	protected[seqs] def freeze() :Unit = unmodifiable = true
	
	//can be extracted and made unspecialized
	override def reserve(required: Int=1): Int = {
		var capacity = array.length-headIdx
		if (required > capacity - len || unmodifiable) {
			capacity += 1
			do { capacity *= 2 } while (capacity < len + required)
			val extended = new Array[E](headIdx + capacity)
			System.arraycopy(array, headIdx, extended, headIdx, len)
			array = extended
		}
		array.length
	}
	
	//can be extracted and made unspecialized
	override def reserveFront(required: Int=1): Unit =
		if (required > headIdx || unmodifiable) {
			var capacity = headIdx+len+1
			do { capacity *= 2 } while (capacity<len+required)
			val extended = new Array[E](capacity + (array.length-endIdx))   //we have an implicit class tag,
			System.arraycopy(array, headIdx, extended, capacity-len, len) //so it will turn out properly specialized
			array = extended
			headIdx = capacity-len
		}
	

	@unspecialized
	override protected[this] def dropSuffix(splitIndex :Int, capacity :Int=len) :ArrayView[E] =
		if (unmodifiable) {
			val tail = newArrayView(array, headIdx+splitIndex, len-splitIndex)
			//			val tail = new GrowingArrayBuffer[E](array, offset+splitIndex, len-splitIndex, true)
			len = splitIndex
			reserve(capacity-len)
			tail
		} else
			super.dropSuffix(splitIndex, capacity)

	
	
	override protected[this] def shiftLeft(downto: Int, by: Int): Unit =
		if (unmodifiable) {
			unmodifiable = false
			val from = downto + by
			val amount = len-from
			len -= by
			val shift = new Array[E](len)
			Array.copy(array, headIdx, shift, 0, downto)
			Array.copy(array, headIdx+from, shift, downto, amount)
		}else
			super.shiftLeft(downto, by)

	
	
	override protected[this] def minus(elems1: Traversable[E], elems2: TraversableOnce[E]): this.type = {
		val removedIndices = indicesOf(elems1, elems2)
		if (removedIndices.nonEmpty) {
			val cuts = removedIndices.toList.sorted
			if (unmodifiable) {
				val to = new Array[E](len-removedIndices.size)
				copyWithout(cuts, to, 0)
				array = to
				headIdx=0
				len = array.length
				unmodifiable = false
			}else {
				copyWithout(cuts, array, headIdx)
				len -= removedIndices.size
			}
		}
		this
	}






	final override def update(idx: Int, elem: E): Unit =
		if (idx<0 || idx>=length)
			throw new IndexOutOfBoundsException(idx.toString)
		else {
			reserve(0)
			array(headIdx+idx) = elem
		}

//	@inline override protected[this] def set(idx: Int, elem: E): Unit = {
//		reserve(0)
//		array(offset+idx) = elem
//	}


	override def transform(f: (E) => E) = {
		reserve(0)
		var i = headIdx; val lim = i+len; val a = array
		while(i<lim) {
			a(i) = f(a(i)); i+=1
		}
		this
	}

	override protected def section(from: Int, until: Int): SharedArrayBuffer[E] =
//		new LentArrayBuffer[E](array, from, until, headIdx, length)
		new GrowingArrayBuffer[E](array, headIdx + from, until - from, unmodifiable)


	override def toFitBuffer[U >: E : Specialized]: SharedArrayBuffer[U] =
		if (storageClass isAssignableFrom Specialized[U].runType)
			new GrowingArrayBuffer[E](array, headIdx, length).asInstanceOf[SharedArrayBuffer[U]]
		else new GrowingArrayBuffer[U]() ++= this

}


/** A specialized buffer implementation which will swap the underlying array for a larger instance once its capacity
  * is exceeded.
  *
  * @param array initial underlying buffer, for erased type `E` may be any of its super types.
  * @param headIdx offset at which actual data starts in the array
  * @param len initial size of the buffer
  * @param unmodifiable can this buffer modify the given array, or is it shared in read-only mode.
  * @tparam E element type before erasure.
  */
class GrowingArrayBuffer[@specialized(Elements) E](
													  protected[this] final var array :Array[E],
													  protected[seqs] final var headIdx :Int,
													  protected[this] final var len :Int,
													  protected final var unmodifiable :Boolean=false)
	extends IterableFoundation[E, SharedArrayBuffer[E]] with DefaultArrayBuffer[E]
{
	def this(array :Array[E]) = this(array, 0, array.length)

	def this(storageType :Class[E]) = this(Array.empty[E](ClassTag(storageType)))

	def this() = this(Specialized.arrayFor[E])
	
}







/** Factory for specialized growing buffers backed by arrays. */
object SharedArrayBuffer extends ArrayViewFactory[SharedArrayBuffer] {
	
	/** Create an empty buffer with the given capacity, using as the element type implicit class information for `E`.
	  * @param sizeHint predicted future size of the buffer
	  * @tparam E element type
	  * @return an empty buffer specialized accordingly to the given class tag.
	  */
	def emptyOf[E :ClassTag](sizeHint :Int) :SharedArrayBuffer[E] =
		shared(new ArrayBounds(new Array[E](sizeHint), 0, 0))
	
	/** Create an empty buffer reusing the given array.
	  * Created buffer will start appending from index `0` in the array, but reallocate it when its capacity is exceeded.
	  * @param array an array to use as the initial storage for the buffer (which contents will be ignored)
	  * @tparam E element type.
	  * @return an empty buffer specialized accordingly to the component type of the given array.
	  */
	def upon[E](array :Array[E]) :SharedArrayBuffer[E] =
		shared(new ArrayBounds(array, 0, 0))
	
	/** Create an empty buffer reusing the given array.
	  * Created buffer will start appending from index `offset`, which makes it possible to ensure free
	  * space for prepending elements.
	  * @param array an array to use as the initial storage for the buffer
	  * @param offset an index in the array which should map to index `0` of the returned buffer.
	  * @return an empty buffer specialized accordingly to the component type of the given array.
	  */
	def upon[E](array :Array[E], offset :Int) :SharedArrayBuffer[E] =
		shared(ArrayBounds.share(array, offset, 0))


	override protected def using[@specialized(Elements) E](array: Array[E], offset: Int, length: Int): SharedArrayBuffer[E] =
		new GrowingArrayBuffer[E](array, offset, length)

	final val DefaultSize = 8
	
	
	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[SharedArrayBuffer[_], E, SharedArrayBuffer[E]]): CanBuildFrom[SharedArrayBuffer[_], E, SharedArrayBuffer[E]] =
		fit.cbf


	@inline private[seqs] final def nextCapacity(current :Int, required :Int) :Int = {
		import java.lang.Integer.highestOneBit
		var capacity = highestOneBit(required)
		if (capacity<required) capacity <<= 1
		if (capacity<0) capacity = Int.MaxValue
		capacity
	}
}

