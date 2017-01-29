package net.turambar.collection.specnaz

import SpecCompanion.{ArraySection, SpecBuilder, SpecCanBuildFrom}

import scala.collection.generic.{GenericCompanion, GenericTraversableTemplate}
import scala.collection.{GenTraversableOnce, IndexedSeqLike, mutable}
import scala.reflect.{ClassTag, classTag}
import scala.annotation.{tailrec, unspecialized}


/** Base trait for specialized buffer implementations backed by an array.
  * As this trait is specialized regarding its element type, the exact type of the underlying array
  * will differ between specialized versions and, for `AnyRef` subtypes,
  * it might be any type `Array[T] forSome { type T&gt;:E &lt;:AnyRef }`. While this class manages
  * the buffer size itself, the responsibility and method (if any) of extending or truncating the underlying array
  * is left to subclasses.
  *
  * @tparam E element type before erasure
  */
trait SharedArrayBuffer[@specialized(Reified) E]
	extends SpecBuffer[E] with ArraySlice[E] with ArraySliceLike[E, SharedArrayBuffer]
	with mutable.BufferLike[E, SharedArrayBuffer[E]]
{

	protected[this] final var array :Array[E] = _
	protected[specnaz] final var offset :Int = _
	protected[this] final var len :Int = _

	@inline final def length = len
	@inline final protected[this] def end = offset+length



	override def +=(elem: E): this.type = {
		reserve()
		array(offset+len) = elem
		len += 1
		this
	}

	override def +=(elem1: E, elem2: E, elems: E*): this.type = {
		if (hasFastSize(elems)) reserve(elems.size+2)
		array(offset+len) = elem1
		array(offset+len+1) = elem2
		len += 2
		this ++= elems
	}

	override def ++=(xs: TraversableOnce[E]): this.type =
		if (hasFastSize(xs)) {
			val count = xs.size
			reserve(count)
			xs.copyToArray(array, offset + len)
			len += count
			this
		} else {
			var i=offset+len
			for(elem <- xs) {
				reserve(1)
				array(i) = elem
				i += 1
			}
			len = i-offset
			this
		}


	override def +=:(elem: E): this.type = {
		reserveFront()
		offset -= 1
		array(offset) = elem
		len += 1
		this
	}

	override def ++=:(xs: TraversableOnce[E]): this.type =
		if (hasFastSize(xs)) {
			val added = xs.size
			reserveFront(added)
			offset -= added; len += added
			xs.copyToArray(array, offset)
			this
		} else {
			xs.foreach(_ +=: this)
			this
		}


	override def insertAll(n: Int, elems: Traversable[E]): Unit =
		if (n<0 || n>len)
			throw new IndexOutOfBoundsException(stringPrefix+"("+n+")")
		else if (hasFastSize(elems)) {
			val added = elems.size
			if (added>0) {
				if (array.length-end>=added) {
					reserve(added)
					val pos = offset+n
					Array.copy(array, pos, array, pos+added, len - n)
					elems.copyToArray(array, pos)
					len += added
				}else {
					val tail = dropSuffix(n, len + added)
					elems.copyToArray(array, end)
					tail.copyToArray(array, end+added)
					len += tail.size + added
				}
			}
		} else if (elems.nonEmpty) {
			val tail = dropSuffix(n, len+1)
			elems.foreach(this += _)
			this ++= tail
		}

	protected[this] def dropSuffix(splitIndex :Int, requiredCapacity :Int=len) :SharedArray[E]


	override def -=(x: E): this.type = {
		val idx = indexOf(x)
		if (idx>0) remove(idx)
		this
	}

	override def -=(elem1: E, elem2: E, elems: E*): this.type =
		minus(Seq(elem1, elem2), elems)

	override def --=(xs: TraversableOnce[E]): this.type = minus(Seq(), xs)


	protected[this] def minus(elems1 :Traversable[E], elems2 :TraversableOnce[E]) :this.type = {
		val removedIndices = indicesOf(elems1, elems2).toSeq.sorted.view
		if (removedIndices.nonEmpty) {
			copyWithout(removedIndices, array, offset)
			len -= removedIndices.length
		}
		this
	}

	protected[this] def copyWithout(removedIndices :Seq[Int], @unspecialized to :Array[E], from :Int) :Unit = {
		(removedIndices zip removedIndices.tail).zipWithIndex foreach { case ((start, end), shift) =>
			Array.copy(array, offset + start + 1, to, from + start - shift, end - start - 1)
		}
		val last = removedIndices.last
		Array.copy(array, last+1, to, from + last - removedIndices.size, len-last-1)
	}



	override def remove(n: Int): E = {
		val removed = apply(n)
		if (n == 0) {
			offset += 1
			len -=1
		} else if (n == len - 1) {
			len -= 1
		} else {
			shiftLeft(n, 1)
		}
		removed
	}


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

	protected[this] def shiftLeft(downto :Int, by :Int) = {
		val from = downto+by
		Array.copy(array, offset+from, array, offset+downto, len-from)
		len -= by
	}



	override def clear(): Unit = {
		len=0
	}

	/** Lazybones variant of `clear()` returning this instance. */
	def empty() :this.type = {
		clear(); this
	}

	protected def reserve(capacity :Int=1) :Unit

	protected def reserveFront(capacity :Int=1) :Unit


	override protected[this] def typeStringPrefix = "SharedBuffer"
}


/** Full implementation of a buffer backed by a growing array.
  * Defined as a trait to allow variation of behaviour in subclasses (specialized class can't extend other specialized version of a class).
 *
  * @tparam E element type before erasure (and specialization)
  */
trait AbstractResizableArrayBuffer[@specialized(Reified) E]
	extends SharedArrayBuffer[E]
{

	protected final var unmodifiable :Boolean = false



	@inline override protected[this] def set(idx: Int, elem: E): Unit = {
		reserve(0)
		array(offset+idx) = elem
	}

	override protected def reserve(required: Int=1): Unit = {
		var capacity = array.length-offset
		if (required > capacity - len || unmodifiable) {
			capacity += 1
			do { capacity *= 2 } while (capacity < len + required)
			val extended = new Array[E](offset + capacity)
			Array.copy(array, offset, extended, offset, len)
			array = extended
		}
	}

	override protected def reserveFront(required: Int=1): Unit =
		if (required > offset || unmodifiable) {
			var capacity = offset+len+1
			do { capacity *= 2 } while (capacity<len+required)
			val extended = new Array[E](capacity + (array.length-end))
			Array.copy(array, offset, extended, capacity-len, len)
			array = extended
			offset = capacity-len
		}


	protected[this] def dropSuffix(splitIndex :Int, capacity :Int=len) :SharedArray[E] =
		if (unmodifiable) {
			val tail = new ResizableArrayBuffer[E](array, offset+splitIndex, len-splitIndex, true)
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
						Array.copy(array, offset + splitIndex, copy, 0, remainder)
						SharedArray[E](copy)
					}else
						new ResizableArrayBuffer[E](array, len, 0, true)
				} else {
					//copy prefix until splitIndex into a new array and leave the suffix in current array
					val old = array
					array = new Array[E](math.max(offset+capacity, array.length))
					Array.copy(old, offset, array, offset, splitIndex)
					new ResizableArrayBuffer[E](old, offset+splitIndex, len-splitIndex, true)
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

	override protected def subseq(from: Int, until: Int): SharedArrayBuffer[E] =
		new ResizableArrayBuffer[E](array, offset + from, until - from, unmodifiable)


	override def toSpecBuffer[U >: E : Specialized]: SharedArrayBuffer[U] =
		if (storageClass isAssignableFrom Specialized[U].runType)
			new ResizableArrayBuffer[E](array, offset, length).asInstanceOf[SharedArrayBuffer[U]]
		else new ResizableArrayBuffer[U]() ++= this


	override protected[this] def factory: SharedArrayFactory[SharedArrayBuffer] = SharedArrayBuffer

}


/** A specialized buffer implementation which will swap the underlying array for a larger instance once its capacity
  * is exceeded.
 *
  * @param buffer initial underlying buffer, for erased type `E` may be any of its super types.
  * @param start offset at which actual data starts in the array
  * @param size initial size of the buffer
  * @param readOnly can this buffer modify the given array, or is it shared in read-only mode.
  * @tparam E element type before erasure.
  */
class ResizableArrayBuffer[@specialized(Reified) E]
//		(protected[this] final var array :Array[E], start :Int, size :Int, readOnly :Boolean=false)
		(buffer :Array[E], start :Int, size :Int, readOnly :Boolean=false)
	extends AbstractResizableArrayBuffer[E]
{
	def this(array :Array[E]) = this(array, 0, array.length)

	def this(storageType :Class[E]) = this(Array.empty[E](ClassTag(storageType)))

	def this() = this(Specialized.erasedArray[E])

	offset = start
	len = size
	unmodifiable = readOnly
	array = buffer
}




object SharedArrayBuffer extends SharedArrayFactory[SharedArrayBuffer] {

	def emptyOf[E :ClassTag](sizeHint :Int) :SharedArrayBuffer[E] =
		shared(new ArraySection(new Array[E](sizeHint), 0, 0))


	def upon[E](array :Array[E]) :SharedArrayBuffer[E] =
		shared(new ArraySection(array, 0, 0))

	def upon[E](array :Array[E], offset :Int) :SharedArrayBuffer[E] =
		shared(ArraySection.share(array, offset, 0))


	override protected def using[@specialized(Reified) E](array: Array[E], offset: Int, length: Int): SharedArrayBuffer[E] =
		new ResizableArrayBuffer[E](array, offset, length)

	final val DefaultSize = 16
}

