package net.turambar.palimpsest.specialty.seqs

import scala.annotation.unspecialized
import scala.collection.generic.CanBuildFrom
import scala.collection.{IndexedSeqLike, mutable}
import scala.reflect.ClassTag

import net.turambar.palimpsest.specialty
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation
import net.turambar.palimpsest.specialty.{ArrayBounds, Elements, FitCompanion, FitItems, Specialized}






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
	        extends FitBuffer[E] with SharedArray[E] with SharedArrayLike[E, SharedArrayBuffer]
	        with mutable.BufferLike[E, SharedArrayBuffer[E]]
{
	
	
	
	
	protected[this] var array :Array[E]
	protected[seqs] var offset :Int
	protected[this] var len :Int
	
	@inline final def length = len
	@inline final protected[this] def end = offset + len
	
	override def ++=(xs: TraversableOnce[E]): this.type = xs match {
		case _ if xs.isEmpty => this
		
		case fit :FitItems[E] =>
			this ++= fit
		
		case _ :IndexedSeqLike[_, _] =>
			directAppend(xs)
			this
		case _ =>
			var a = array
			var o = offset
			var i = o + len; var nextcheck = i
			for(elem <- xs) {
				if (i==nextcheck) {
					nextcheck = reserve()
					a = array
					i += offset-o //in case reserve() shifted the contents
					o = offset
				}
				a(i) = elem
				i += 1
			}
			len = i-o
			this
	}
	
	//can be extracted to unspecialized class
	protected def directAppend(elems :TraversableOnce[E]) :Unit = {
		val count = elems.size
		reserve(count)
		elems.copyToArray(array, offset + len)
		len += count
	}
	
	
	//can be extracted to unspecialized class
	override def ++=:(xs: TraversableOnce[E]): this.type = xs match {
		case _ if xs.isEmpty => this
		case items :FitItems[E] =>
			items ++=: this
		case _ if xs.isTraversableAgain && xs.hasDefiniteSize =>
			val added = xs.size
			reserveFront(added)
			offset -= added; len += added
			xs.copyToArray(array, offset)
			this
		case _ =>
			val prefix = newBuffer
			prefix ++= xs
			prefix ++=: this
			this
		
	}
	
	
	
	
	//can be extracted to unspecialized class
	override def insertAll(n: Int, elems: Traversable[E]): Unit =
		if (n<0 || n>len)
			throw new IndexOutOfBoundsException(stringPrefix+"("+n+")")
		else if (elems.nonEmpty)
			     if (specialty.ofKnownSize(elems)) {
				     val added = elems.size
				     if (array.length-end>=added) {
					     reserve(added)
					     val pos = offset+n
					     System.arraycopy(array, pos, array, pos+added, len - n)
					     elems.copyToArray(array, pos)
					     len += added
				     }else {
					     val tail = dropSuffix(n, len + added)
					     elems.copyToArray(array, end)
					     tail.copyToArray(array, end+added)
					     len += tail.size + added
				     }
			     } else  {
				     val tail = dropSuffix(n, len+1)
				     this ++= elems
				     this ++= tail
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
	protected[this] def dropSuffix(splitIndex :Int, requiredCapacity :Int=len) :ArrayView[E]
	
	
	
	//can be extracted to unspecialized class
	protected[this] def minus(elems1 :Traversable[E], elems2 :TraversableOnce[E]) :this.type = {
		val removedIndices = indicesOf(elems1, elems2).toSeq.sorted.view
		if (removedIndices.nonEmpty) {
			copyWithout(removedIndices, array, offset)
			len -= removedIndices.length
		}
		this
	}
	
	//can be extracted to unspecialized class
	protected[this] def copyWithout(removedIndices :Seq[Int], @unspecialized to :Array[E], from :Int) :Unit = {
		(removedIndices zip removedIndices.tail).zipWithIndex foreach { case ((start, end), shift) =>
			System.arraycopy(array, offset + start + 1, to, from + start - shift, end - start - 1)
		}
		val last = removedIndices.last
		System.arraycopy(array, last+1, to, from + last - removedIndices.size, len-last-1)
	}
	
	
	
	//can be extracted to unspecialized class
	protected[this] final def cleanUpAfterRemoval(n :Int) :Unit = {
		if (n == 0) {
			offset += 1
			len -=1
		} else if (n == len - 1) {
			len -= 1
		} else {
			shiftLeft(n, 1)
		}
	}
	
	//can be extracted to unspecialized class
	override def remove(n: Int, count: Int): Unit = {
		val removed = count min (len - n) max 0
		if (n < 0 || n > len)
			throw new IndexOutOfBoundsException(s"$stringPrefix[$length].remove($n, $count)")
		else if (removed>0) {
			if (n == 0) {
				offset += removed
				len -= removed
			} else if (removed == len-n) {
				len -= removed
			}else {
				shiftLeft(n, removed)
			}
		}
	}
	
	/** Callback from `remove` methods shifting back the suffix to replace removed elements within the array.
	  * This method should copy in place elements `downto+by..length` to positions `downto..length-by` and reduce
	  * length accordingly.
	  * @param downto index of the first removed element
	  * @param by number of elements removed at that index.
	  */
	//can be extracted to unspecialized class
	protected[this] def shiftLeft(downto :Int, by :Int) :Unit = {
		val from = downto+by
		System.arraycopy(array, offset+from, array, offset+downto, len-from)
		len -= by
	}
	
	
	//can be extracted to unspecialized class
	override def clear(): Unit = {
		len=0
	}
	
	/** Lazybones variant of `clear()` returning this instance. */
	def empty() :this.type = {
		clear(); this
	}
	
	/** Ensure available space for `capacity` elements after the current contents of this buffer.
	  * Might involve allocating a new array or moving elements.
	  * If this method returns without an exception, it is assumed that `offset+length+capacity <= array.length`.
	  * @return index in the array past `offset+length` upto which (exclusively) this buffer can write within a single operation
	  *         without a further check of capacity with this method.
	  */
	def reserve(capacity :Int=1) :Int
	
	/** Ensure available space for `capacity` elements before the current contents of this buffer.
	  * Might involve allocating a new array or moving elements.
	  * If this method returns without an exception, it is assumed that `offset >= capacity`.
	  */
	def reserveFront(capacity :Int=1) :Unit
	
	
	
	
	
	//	protected[this] var array :Array[E]
//	protected[seqs] var offset :Int
//	protected[this] var len :Int
//
//	@inline final def length = len
//	@inline final protected[this] def end = offset + len



	override def +=(elem: E): this.type = {
		reserve()
		array(offset+len) = elem
		len += 1
		this
	}

	override def +=(elem1: E, elem2: E, elems: E*): this.type = {
		if (specialty.ofKnownSize(elems))
			reserve(elems.size+2)
		else
	        reserve(2)
		array(offset+len) = elem1
		array(offset+len+1) = elem2
		len += 2
		this ++= elems
	}

	//
	override def ++=(elems: FitItems[E]): this.type =
		if (elems.hasFastSize) {
			directAppend(elems); this
		}else {
			var a = array
			var o = offset
			var i = o+len; val it=elems.toIterator //todo: specialization
			var nextcheck = i
			while(it.hasNext) { //we are using an iterator to avoid boxing of function calls
				if (i==nextcheck) { //call to reserve() requires several static forwarders
					nextcheck = reserve()
					a = array
					i += offset-o //reserve might have shift the contents
					o = offset
				}
				a(i) = it.next()
				i += 1
			}
			len = i-o
			this
		}
	

	override def +=:(elem: E): this.type = {
		reserveFront()
		offset -= 1
		array(offset) = elem
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
		cleanUpAfterRemoval(n)
		removed
	}
	
	/** A quick access to a specialized sequence backed by the given section of the array for subclasses. */
	protected[this] def newArrayView(array :Array[E], offset :Int, length :Int) :ArrayView[E] =
		SharedArray.view(array, offset, length)
	
	
//	this.companion
//	this.typeStringPrefix
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
		var capacity = array.length-offset
		if (required > capacity - len || unmodifiable) {
			capacity += 1
			do { capacity *= 2 } while (capacity < len + required)
			val extended = new Array[E](offset + capacity)
			System.arraycopy(array, offset, extended, offset, len)
			array = extended
		}
		array.length
	}
	
	//can be extracted and made unspecialized
	override def reserveFront(required: Int=1): Unit =
		if (required > offset || unmodifiable) {
			var capacity = offset+len+1
			do { capacity *= 2 } while (capacity<len+required)
			val extended = new Array[E](capacity + (array.length-end))   //we have an implicit class tag,
			System.arraycopy(array, offset, extended, capacity-len, len) //so it will turn out properly specialized
			array = extended
			offset = capacity-len
		}
	
	
	protected[this] def dropSuffix(splitIndex :Int, capacity :Int=len) :ArrayView[E] =
		if (unmodifiable) {
			val tail = newArrayView(array, offset+splitIndex, len-splitIndex)
			//			val tail = new GrowingArrayBuffer[E](array, offset+splitIndex, len-splitIndex, true)
			len = splitIndex
			reserve(capacity-len)
			tail
		} else {
			val tail =
				if (splitIndex >= length/2 && array.length-offset >= capacity) {
					//append to current array, copy suffix from splitIndex to a new array
					val remainder = len-splitIndex
					if (remainder>0) {
						val copy = new Array[E](remainder)
						System.arraycopy(array, offset + splitIndex, copy, 0, remainder)
						//						ArrayView[E](copy)
						newArrayView(copy, 0, remainder)
					}else
						 ArrayView.Empty //new GrowingArrayBuffer[E](array, len, 0, true)
				} else {
					//copy prefix until splitIndex into a new array and leave the suffix in current array
					val old = array
					array = new Array[E](math.max(offset+capacity, array.length))
					System.arraycopy(old, offset, array, offset, splitIndex)
					//					ArrayView[E](old, offset+splitIndex, len-splitIndex)
					newArrayView(old, offset+splitIndex, len-splitIndex)
				}
			len = splitIndex
			tail
		}
	
	
	override protected[this] def shiftLeft(downto: Int, by: Int): Unit =
		if (unmodifiable) {
			unmodifiable = false
			val from = downto + by
			val amount = len-from
			len -= by
			val shift = new Array[E](len)
			Array.copy(array, offset, shift, 0, downto)
			Array.copy(array, offset+from, shift, downto, amount)
		}else{
			super.shiftLeft(downto, by)
		}
	
	
	override protected[this] def minus(elems1: Traversable[E], elems2: TraversableOnce[E]): this.type = {
		val removedIndices = indicesOf(elems1, elems2).toSeq.sorted.view
		if (removedIndices.nonEmpty) {
			if (unmodifiable) {
				val to = new Array[E](len-removedIndices.length)
				copyWithout(removedIndices, to, 0)
				array = to
				offset=0
				len = array.length
				unmodifiable = false
			}else {
				copyWithout(removedIndices, array, offset)
				len -= removedIndices.length
			}
		}
		this
	}
	
	
	
	
	
	
	
	@inline override protected[this] def set(idx: Int, elem: E): Unit = {
		reserve(0)
		array(offset+idx) = elem
	}

	
	override protected def section(from: Int, until: Int): SharedArrayBuffer[E] =
		new GrowingArrayBuffer[E](array, offset + from, until - from, unmodifiable)


	override def toFitBuffer[U >: E : Specialized]: SharedArrayBuffer[U] =
		if (storageClass isAssignableFrom Specialized[U].runType)
			new GrowingArrayBuffer[E](array, offset, length).asInstanceOf[SharedArrayBuffer[U]]
		else new GrowingArrayBuffer[U]() ++= this

}


/** A specialized buffer implementation which will swap the underlying array for a larger instance once its capacity
  * is exceeded.
  *
  * @param array initial underlying buffer, for erased type `E` may be any of its super types.
  * @param offset offset at which actual data starts in the array
  * @param len initial size of the buffer
  * @param unmodifiable can this buffer modify the given array, or is it shared in read-only mode.
  * @tparam E element type before erasure.
  */
class GrowingArrayBuffer[@specialized(Elements) E](
		protected[this] final var array :Array[E],
		protected[seqs] final var offset :Int,
		protected[this] final var len :Int,
		protected final var unmodifiable :Boolean=false)
	extends IterableFoundation[E, SharedArrayBuffer[E]] with DefaultArrayBuffer[E]
{
	def this(array :Array[E]) = this(array, 0, array.length)

	def this(storageType :Class[E]) = this(Array.empty[E](ClassTag(storageType)))

	def this() = this(Specialized.erasedArray[E])
	
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

	final val DefaultSize = 16
	
	
	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[SharedArrayBuffer[_], E, SharedArrayBuffer[E]]): CanBuildFrom[SharedArrayBuffer[_], E, SharedArrayBuffer[E]] =
		fit.cbf
}

