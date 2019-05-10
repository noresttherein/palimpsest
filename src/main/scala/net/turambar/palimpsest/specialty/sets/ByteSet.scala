package net.turambar.palimpsest.specialty.sets

import java.lang.Long.highestOneBit

import scala.collection.generic.CanBuildFrom
import scala.collection.{GenIterable, GenSet, GenTraversableOnce}
import net.turambar.palimpsest.specialty.FitIterator.BaseIterator
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.sets.ByteSet.{ByteSetBitmap, ByteSetBuilder, ByteSetIterator}
import net.turambar.palimpsest.specialty.{?, Blank, FitBuilder, FitIterator, FitTraversableOnce, RuntimeType, Sure}
import net.turambar.palimpsest.specialty.RuntimeType.{Fun1, Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.sets.OrderedSet.Mutable

import scala.annotation.tailrec
import scala.math.Ordering.ByteOrdering

/** Base class for mutable and immutable sets of `Byte` elements implemented as a bitmap.
  * As with most implementations specialized for a concrete element type, it is not part
  * of the public API but intended to be used via generic (but specialized) interfaces
  * of [[ValSet]], [[MutableSet]], [[StableSet]].
  *
  * Note that while this is an [[OrderedSet]], it follows unsigned ordering of values as if promoted to `Int`
  * without copying the sign bit: `x & 0xff`. In terms of natural promotion of `Byte` to `Int` range of `-128 .. 127`
  * this translates to positioning all negative values after positive values. As bytes are more often used in terms
  * of codes from the `0..255` range rather than 'tiny integer', this will usually be more convenient, even if it
  * stands against the principle of least confusion.
  *
  * @see [[net.turambar.palimpsest.specialty.sets.ByteSet.MutableByteSet]]
  * @see [[net.turambar.palimpsest.specialty.sets.ByteSet.StableByteSet]]
  * @author Marcin Mościcki
  */
private[sets] sealed abstract class ByteSet[This <: OrderedSetTemplate[Byte, This] with OrderedSet[Byte]] private[ByteSet](bytes :ByteSetBitmap)
	extends OrderedSet[Byte] with OrderedSetTemplate[Byte, This] /*with SetSpecialization[Byte, This]*/ with OfKnownSize
{
	@inline final private[ByteSet] def bitmap :ByteSetBitmap = bytes

	override final protected[this] def mySpecialization  :RuntimeType[Byte] = RuntimeType.OfByte

	override implicit def ordering: ValOrdering[Byte] = ByteSet.UnsignedOrdering


	override def empty :This = newByteSet(ByteSet.EmptyBitmap())

	/** Factory method to be implemented by subclasses
	  * @param bits bitmap carrying elements of the new set
	  * @return a mutable or immutable instance depending on the actual type of `this`.
	  */
	protected[this] def newByteSet(bits :ByteSetBitmap) :This



	override final def size: Int = bytes.size
//	override def hasFastSize = true
//	override def hasDefiniteSize = true
	
	override final def nonEmpty :Boolean = bytes.nonEmpty

	override final def isEmpty :Boolean = bytes.isEmpty



	override final def head :Byte = bytes.head

	override final def head_? : ?[Byte] = bytes.head_?

	override final def headOption :Option[Byte] = bytes.headOption
	
	override final def last :Byte = bytes.last

	override final def last_? : ?[Byte] = bytes.last_?

	override final def lastOption :Option[Byte] = bytes.lastOption

	override final def tail :This = newByteSet(bytes.tail)

	override final def init :This = newByteSet(bytes.init)


	override final def apply(elem: Byte): Boolean = bytes.contains(elem)
	
	override final def contains(elem: Byte): Boolean = bytes.contains(elem)


	override def ^(elem :Byte) :This = newByteSet(bytes ^ elem)

	override final def filterNot(p: Byte => Boolean): This = newByteSet(bytes.filterNot(p))

	override final def filter(p: Byte => Boolean): This = newByteSet(bytes.filter(p))

	override def filter(p: Byte => Boolean, ourTruth: Boolean): This = newByteSet(bytes.filter(p, ourTruth))


	override final def foreach[@specialized(Unit) U](f: Byte => U): Unit = bytes.foreach(f)
	override protected def reverseForeach(f: Byte => Unit): Unit = bytes.reverseForeach(f)




	override final def map[@specialized(Fun1Vals) B, That](f: Byte => B)(implicit bf: CanBuildFrom[This, B, That]): That = {
		val bu = bf(this.asInstanceOf[This])
		if (nonEmpty) {
			bu.sizeHint(size)
			var cell = 0
			do {
				val base = cell*64; var i = 0; var word = bitmap.bitmap(cell)
				while (word != 0L) {
					if ((word & 1L) != 0) bu += f((base+i).toByte)
					i += 1; word >>>= 1
				}
				cell += 1
			}while(cell < 4)
		}
		bu.result()
	}
	
	
	override final def flatMap[B, That](f: Byte => GenTraversableOnce[B])(implicit bf: CanBuildFrom[This, B, That]): That = {
		val bu = bf(this.asInstanceOf[This]); val s = size
		if (nonEmpty) {
			var cell = 0
			do {
				val base = cell*64; var i = 0; var word = bitmap.bitmap(cell)
				while (word != 0L) {
					if ((word & 1L) != 0) bu ++= f((base+i).toByte).seq
					i += 1; word >>>= 1
				}
				cell += 1
			}while(cell < 4)
		}
		bu.result()
	}


	override def find_?(p :Byte => Boolean): ?[Byte] = bytes.find_?(p, true)

	override def find_?(p :Byte => Boolean, where :Boolean): ?[Byte] = bytes.find_?(p, where)


	override def ++(elems: FitTraversableOnce[Byte]) :This = elems match {
		case other :ByteSet[_] => newByteSet(bytes ++ other.bitmap)
		case _ => super.++(elems)
	}

	override def --(elems: FitTraversableOnce[Byte]) :This = elems match {
		case other :ByteSet[_] => newByteSet(bytes -- other.bitmap)
		case _ => super.--(elems)
	}


	override def ^(that :ValSet[Byte]) :This = that match {
		case other :ByteSet[_] => newByteSet(bytes ^ other.bitmap)
		case _ =>
			val copy = bitmap.copy
			that foreach { byte :Byte => if (copy contains byte) copy -= byte else copy += byte }
			newByteSet(copy)
	}

	override def &(that :ValSet[Byte]) :This = that match {
		case other :ByteSet[_] => newByteSet(bytes & other.bitmap)
		case _ => filter(that)
	}

	override def intersect(that: GenSet[Byte]) :This = that match {
		case other :ByteSet[_] => newByteSet(bytes & other.bitmap)
		case _ => filter(that)
	}


	override def diff(that :GenSet[Byte]) :This = that match {
		case other :ByteSet[_] => newByteSet(bytes -- other.bitmap)
		case _ =>
			val copy = bitmap.copy
			bitmap foreachInt { i => if (that(i.toByte)) copy -= i.toByte }
			newByteSet(copy)
	}



	override def subsetOf(that :GenSet[Byte]) :Boolean = that match {
		case other :ByteSet[_] => bytes subsetOf other.bitmap
		case _ => super.subsetOf(that)
	}


	override protected def uncheckedCopyTo(xs: Array[Byte], start: Int, total: Int): Int = {
		var copied = 0; var i = 0
		var cell = 0
		while(cell < 4 && copied < total) {
			val base = cell*64; var i = 0; var word = bitmap.bitmap(cell)
			while (word != 0L) {
				if ((word & 1L) != 0) {
					xs(start + copied) = (base + i).toByte
					copied += 1
				}
				i += 1; word >>>= 1
			}
			cell += 1
		}
		copied
	}


	//	override final def toIterator: FitIterator[Byte] = new ByteSetIterator(bytes.copy)
	override final def iterator :FitIterator[Byte] = new ByteSetIterator(bytes.copy)


	override def keyAt(idx :Int) :Byte = bytes.keyAt(idx)


	override def keysIteratorFrom(start: Byte): FitIterator[Byte] = {
		val bits = bytes.copy
		bits.clearUntil(start & 0xff)
		new ByteSetIterator(bits)
	}


	override def from(from: Byte): This = {
		val bits = bytes.copy
		bits.clearUntil(from & 0xff)
		newByteSet(bits)
	}

	override def until(until: Byte): This = {
		val bits = bytes.copy
		bits.clearFrom(until & 0xff)
		newByteSet(bits)
	}

	override def range(from: Byte, until: Byte): This = {
		val bits = bytes.copy
		bits.clearUntil(from & 0xff)
		bits.clearFrom(until & 0xff)
		newByteSet(bits)
	}

	override def to(to: Byte): This = {
		val bits = bytes.copy
		bits.clearFrom((to & 0xff) + 1)
		newByteSet(bits)
	}

	override def rangeImpl(from: ?[Byte], until: ?[Byte]): This =
		if (until.isDefined) range(from getOrElse 0.toByte, until.get)
		else if (from.isDefined) this.from(from.get)
		else repr



	override def equals(that: Any): Boolean = that match {
		case bytes :ByteSet[_] => bitmap sameElements bytes.bitmap
		case _ => super.equals(that)
	}
	
	override def sameElements[B >: Byte](that: GenIterable[B]): Boolean = that match {
		case bytes :ByteSet[_] => bitmap sameElements bytes.bitmap
		case _ => super.sameElements(that)
	}


	override def mkString(start: String, sep: String, end: String): String = {
		val res = new StringBuilder(start)
		bitmap.foreachInt { byte => res ++= byte.toHexString ++= sep}
		val len = res.length
		if (len > start.length)
			res.delete(len - sep.length, len)
		res ++= end
		res.toString
	}

	override def stringPrefix = "Set[Byte]"

	override def typeStringPrefix = "ByteSet"
}




/** Companion and factory of immutable and mutable sets of bytes, their builders and iterators. */
private[sets] object ByteSet {
	import java.lang.Long.{bitCount, numberOfTrailingZeros, numberOfLeadingZeros}
	
	final val Empty = new StableByteSet(EmptyBitmap())

	@inline final def newBuilder :FitBuilder[Byte, StableOrderedSet[Byte]] = new ByteSetBuilder

	def empty :StableOrderedSet[Byte] = Empty

	def apply(bytes :Byte*) :StableOrderedSet[Byte] = (newBuilder ++= bytes).result()

	def mutable :MutableOrderedSet[Byte] = new MutableByteSet(EmptyBitmap())

	private final val GarbageBitmap = EmptyBitmap()
	@inline private def EmptyBitmap() = ByteSetBitmap(0L, 0L, 0L, 0L)


	/** Order on bytes when treated as unsigned numbers in the `0..255` range.*/
	object UnsignedOrdering extends ValOrdering[Byte] {
		override def compare(x: Byte, y: Byte): Int = (x & 0xff) - (y & 0xff)
	}



	final class StableByteSet(bits :ByteSetBitmap)
		extends ByteSet[StableOrderedSet[Byte]](bits) with StableOrderedSet[Byte]
	{
		override protected[this] def newByteSet(bits: ByteSetBitmap) = new StableByteSet(bits)
		override def empty :StableByteSet = Empty

		override def +(elem: Byte) = new StableByteSet(bitmap + elem)

		override def +(elem1 :Byte, elem2 :Byte, elems :Byte*) :StableByteSet = {
			val copy = bitmap.copy
			copy += elem1; copy += elem2
			elems match {
				case list :List[Byte] =>
					@tailrec def rec(l :List[Byte]) :Unit = l match {
						case hd::tail => copy += hd; rec(tail)
						case _ => ()
					}
					rec(list)
				case _ => elems.foreach { copy += _ }
			}
			new StableByteSet(copy)
		}


		override def -(elem: Byte) = new StableByteSet(bitmap - elem)

		override def -(elem1: Byte, elem2: Byte, elems: Byte*) :StableByteSet = {
			val copy = bitmap.copy
			copy -= elem1; copy -= elem2
			elems match {
				case list :List[Byte] =>
					@tailrec def rec(l :List[Byte]) :Unit = l match {
						case hd::tail => copy -= hd; rec(tail)
						case _ => ()
					}
					rec(list)
				case _ => elems.foreach { copy -= _ }
			}
			new StableByteSet(copy)
		}


		override def ^(elem :Byte) :StableByteSet = new StableByteSet(bitmap ^ elem)

		override def newBuilder = new ByteSetBuilder

		override def mutable: Mutable[Byte] = new MutableByteSet(bitmap.copy)
	}



	final class MutableByteSet(bits :ByteSetBitmap)
		extends ByteSet[MutableOrderedSet[Byte]](bits) with MutableOrderedSet[Byte]
	{
		override def empty = new MutableByteSet(EmptyBitmap())

		override protected[this] def newByteSet(bits: ByteSetBitmap): MutableByteSet = new MutableByteSet(bits)

		override def clear() :Unit = bitmap.clear()

		override def +=(elem: Byte): this.type = { bitmap += elem; this }

		override def -=(elem: Byte): this.type = { bitmap -= elem; this }

		override def ^=(elem :Byte): this.type = { bitmap ^= elem; this }

		override def --=(xs: FitTraversableOnce[Byte]) :this.type = xs match {
			case other :ByteSet[_] => bitmap --= other.bitmap; this
			case _ => super.--=(xs)
		}

		override def ++=(xs: FitTraversableOnce[Byte]) :this.type = xs match {
			case other :ByteSet[_] => bitmap ++= other.bitmap; this
			case _ => super.++=(xs)
		}

		override def &=(xs :ValSet[Byte]) :this.type = xs match {
			case other :ByteSet[_] =>
				bitmap &= other.bitmap; this
			case _ if xs.isEmpty =>
				bitmap.clear(); this
			case _ =>
				val res = EmptyBitmap()
				val it = xs.iterator
				do {
					val e = it.next()
					if (contains(e))
						bitmap += e
				} while (it.hasNext)
				bitmap := res
				this
		}

		override def ^=(xs :ValSet[Byte]) :this.type = xs match {
			case other :ByteSet[_] => bitmap ^= other.bitmap; this
			case _ => super.^=(xs)
		}



		override def stable = new StableByteSet(bitmap.copy)

		override def clone() = new MutableByteSet(bitmap.copy)

		override def count: Int = size
	}


	/** Bitmap set operations shared by set and iterator implementations.
	  * @param bitmap an array of exactly four elements where byte `n`
	  *               is present in the set if `bitmap(n/64)` has its `n % 64` (lowest) bit set.
	  */
	private[ByteSet] class ByteSetBitmap(val bitmap :Array[Long]) extends AnyVal {


		@inline def copy = new ByteSetBitmap({
			val array = new Array[Long](4)
			array(0) = bitmap(0); array(1) = bitmap(1); array(2) = bitmap(2); array(3) = bitmap(3)
			array
		})



		@inline def size: Int =
			bitCount(bitmap(0)) + bitCount(bitmap(1)) + bitCount(bitmap(2)) + bitCount(bitmap(3))
		
		@inline def nonEmpty :Boolean =
			(bitmap(3) | bitmap(2) | bitmap(1) | bitmap(0)) != 0L
		
		@inline def isEmpty :Boolean =
			(bitmap(3) | bitmap(2) | bitmap(1) | bitmap(0)) == 0L


		@inline def head :Byte = {
			val first = bitmap(0); val second = bitmap(1)
			if ((first | second) != 0L)
				if (first!=0L)
					return numberOfTrailingZeros(first).toByte
				else
	                return (64 + numberOfTrailingZeros(second)).toByte
			val third = bitmap(2)
			if (third != 0L)
				return (128 + numberOfTrailingZeros(third)).toByte
			val fourth = bitmap(3)
			if (fourth != 0L)
				(192 + numberOfTrailingZeros(fourth)).toByte
			else
	            throw new NoSuchElementException("Set[Byte]().head")
		}


		/** Returns the index of the first set bit in this bitmap. This represents the first byte in this set with
		  * the caveat that all negative values `&lt;-128..-1&gt;` preceed all positive values `&lt;0..127&gt;`
		  * @return lowest byte in this set (when all elements are interpreted as unsigned values) or -1 if empty
		  */
		@inline def headInt :Int = {
			val first = bitmap(0); val second = bitmap(1)
			if ((first | second) != 0L)
				if (first!=0L)
					return numberOfTrailingZeros(first)
				else
	                return 64 + numberOfTrailingZeros(second)
			val third = bitmap(2)
			if (third != 0)
				return 128 + numberOfTrailingZeros(third)
			val fourth = bitmap(3)
			if (fourth != 0)
				192 + numberOfTrailingZeros(fourth)
			else
	            -1
		}

		@inline def head_? : ?[Byte] = headInt match {
			case -1 => Blank
			case n => Sure(n.toByte)
		}

		@inline def headOption :Option[Byte] = headInt match {
			case -1 => None
			case n => Some(n.toByte)
		}
		

		@inline def lastInt :Int = {
			val third = bitmap(2); val fourth = bitmap(4)
			if ((third | fourth) != 0L)
				if (fourth != 0L)
					return 255 - numberOfLeadingZeros(fourth)
				else
                    return 191 - numberOfLeadingZeros(third)
			val second = bitmap(1)
			if (second != 0L)
				return 127 - numberOfLeadingZeros(second)
			val first = bitmap(0)
			if (first != 0L)
				63 - numberOfLeadingZeros(first)
			else
				-1
		}

		@inline def last :Byte = lastInt match {
			case -1 => throw new NoSuchElementException("Set[Byte]().last")
			case x => x.toByte
		}

		@inline def last_? : ?[Byte] = lastInt match {
			case -1 => Blank
			case x => Sure(x.toByte)
		}

		@inline def lastOption :Option[Byte] = lastInt match {
			case -1 => None
			case x => Some(x.toByte)
		}


		def tail :ByteSetBitmap = {
			val first = bitmap(0); val second = bitmap(1)
			if ((first | second) != 0L)
				if (first != 0L) ByteSetBitmap(first & (first-1), second, bitmap(2), bitmap(3))
				else ByteSetBitmap(0L, second & (second-1), bitmap(2), bitmap(3))
			else {
				val third = bitmap(2)
				if (third != 0L) ByteSetBitmap(0L, 0L, third & (third-1), bitmap(3))
				else {
					val fourth=bitmap(3)
					if (fourth == 0L)
						throw new UnsupportedOperationException(s"ByteSet().tail")
					ByteSetBitmap(0L, 0L, 0L, fourth & (fourth-1))
				}
			}
		}

		def init :ByteSetBitmap = {
			val third = bitmap(2); val fourth = bitmap(3)
			if ((third | fourth) != 0L)
				if (fourth != 0L) ByteSetBitmap(bitmap(0), bitmap(1), third, fourth ^ highestOneBit(fourth))
				else ByteSetBitmap(bitmap(0), bitmap(1), third ^ highestOneBit(third), 0L)
			else {
				val second = bitmap(1)
				if (second != 0L) ByteSetBitmap(bitmap(0), second ^ highestOneBit(second), 0L, 0L)
				else {
					val first = bitmap(0)
					if (first == 0L)
						throw new UnsupportedOperationException("ByteSet().init")
					ByteSetBitmap(first ^ highestOneBit(first), 0L, 0L, 0L)
				}
			}
		}
		

		def find_?(p :Byte => Boolean, where :Boolean): ?[Byte] = {
			var cell = 0
			do {
				val base = cell*64; var i = 0; var word = bitmap(cell)
				while (word != 0L) {
					if ((word & 1L) != 0) {
						val e =(base+i).toByte
						if (p(e) == where)
							return Sure(e)
					}
					i += 1; word >>>= 1
				}
				cell += 1
			} while(cell < 4)
			Blank
		}


		@inline def contains(elem: Byte): Boolean = {
			val i = elem & 0xff
			((bitmap(i / 64) >> i) & 1L) != 0
		}

		def keyAt(idx :Int) :Byte =
			if (idx < 0)
				throw new IndexOutOfBoundsException(s"ByteSet.keyAt($idx)")
			else {
				var cell = 0
				var left = idx
				while(cell < 4) {
					var chunk = bitmap(cell)
					while (chunk != 0L) {
						val bit = chunk & -chunk //lowest one bit
						if (left == 0) {
							var lo = 0; var hi = 63 //use bin search to find the index of the lowest one bit
							do {
								val mid = (lo + hi + 1) / 2 //if lo+1 == hi then mid = hi
								if ((bit >>> mid) == 0L)
									hi = mid - 1
								else
									lo = mid
							} while (lo < hi)

							return (cell * 64 + lo).toByte //caution: function return
						}
						chunk ^= bit //clear lowest bit and continue
						left -= 1
					}
					cell += 1
				}
				throw new IndexOutOfBoundsException(s"ByteSet.keyAt($idx)")
			}
		
		@inline def +(elem: Byte): ByteSetBitmap = {
			val i = elem & 0xff
			val cell = i / 64
			val mask = 1L << i
			if ((bitmap(cell) & mask) != 0) this
			else {
				val bits = copy//(bitmap(0), bitmap(1), bitmap(2), bitmap(3))
				bits.bitmap(cell) = bitmap(cell) | mask
				bits
			}
		}
		
		@inline def -(elem: Byte): ByteSetBitmap = {
			val i = elem & 0xff
			val cell = i/64
			val mask = 1L << i
			if ((bitmap(cell) & mask)==0) this
			else {
				val bits = copy
				bits.bitmap(cell) = bitmap(cell) ^ mask
				bits
			}
		}

		@inline def ^(elem :Byte) :ByteSetBitmap = {
			val i = elem & 0xff
			val cell = i/64
			val mask = 1L << i
			val bits = copy
			bits.bitmap(cell) = bitmap(cell) ^ mask
			bits
		}



		@inline def ++(bytes :ByteSetBitmap) :ByteSetBitmap =
			ByteSetBitmap(
				bitmap(0) | bytes.bitmap(0),
				bitmap(1) | bytes.bitmap(1),
				bitmap(2) | bytes.bitmap(2),
				bitmap(3) | bytes.bitmap(3)
			)

		@inline def --(bytes :ByteSetBitmap) :ByteSetBitmap =
			ByteSetBitmap(
				bitmap(0) & ~bytes.bitmap(0),
				bitmap(1) & ~bytes.bitmap(1),
				bitmap(2) & ~bytes.bitmap(2),
				bitmap(3) & ~bytes.bitmap(3)
			)

		@inline def &(bytes :ByteSetBitmap) :ByteSetBitmap =
			ByteSetBitmap(
				bitmap(0) & bytes.bitmap(0),
				bitmap(1) & bytes.bitmap(1),
				bitmap(2) & bytes.bitmap(2),
				bitmap(3) & bytes.bitmap(3)
			)



		@inline def ^(bytes :ByteSetBitmap) :ByteSetBitmap =
			ByteSetBitmap(
				bitmap(0) ^ bytes.bitmap(0),
				bitmap(1) ^ bytes.bitmap(1),
				bitmap(2) ^ bytes.bitmap(2),
				bitmap(3) ^ bytes.bitmap(3)
            )

		@inline def subsetOf(bytes :ByteSetBitmap) :Boolean =
			(bitmap(0) & bytes.bitmap(0)) == bitmap(0) &&
			(bitmap(1) & bytes.bitmap(1)) == bitmap(1) &&
			(bitmap(2) & bytes.bitmap(2)) == bitmap(2) &&
			(bitmap(3) & bytes.bitmap(3)) == bitmap(3)


		@inline def removeFirst() :Unit = {
			val first = bitmap(0); val second = bitmap(1)
			if ((first | second) != 0)
				if (first!=0) bitmap(0) = first & (first-1)
				else bitmap(1) = second & (second-1)
			else {
				val third = bitmap(2)
				if (third!=0) bitmap(2) = third & (third-1)
				else {
					val fourth = bitmap(3)
					bitmap(3) = fourth & (fourth-1)
				}
			}
		}

		@inline def clear() :Unit = {
			bitmap(0) = 0L; bitmap(1) = 0L; bitmap(2) = 0L; bitmap(3) = 0L
		}

		@inline def clearUntil(first :Int) :Unit = {
			var clearedWords = first >> 6
			bitmap(clearedWords) &= (0xffffffffffffffffL >>> -first)
			while(clearedWords > 0) {
				bitmap(clearedWords) = 0L; clearedWords -= 1
			}
		}

		@inline def clearFrom(until :Int) :Unit = {
			var clearedWord = until >> 6
			val clearedBits = until & 0x3f
			if (clearedBits > 0) {
				bitmap(clearedWord) &= ~(0x8000000000000000L >> clearedBits)
				clearedWord += 1
			}
			while (clearedWord < 3) {
				bitmap(clearedWord) = 0L
				clearedWord += 1
			}
		}



		@inline def +=(elem :Byte) :Unit = {
			val i = elem & 0xff
			val cell = i /64
			val mask = 1L << i
			bitmap(cell) = bitmap(cell) | mask
		}

		@inline def -=(elem :Byte) :Unit = {
			val i = elem & 0xff
			val cell = i/64
			val mask = 1L << i
			bitmap(cell) = bitmap(cell) ^ mask
		}

		@inline def ^=(elem :Byte) :Unit = {
			val i = elem & 0xff
			val cell = i/64
			val mask = 1L << i
			bitmap(cell) ^= mask
		}


		@inline def ++=(bytes :ByteSetBitmap) :Unit = {
			bitmap(0) |= bytes.bitmap(0)
			bitmap(1) |= bytes.bitmap(1)
			bitmap(2) |= bytes.bitmap(2)
			bitmap(3) |= bytes.bitmap(3)
		}
		
		@inline def --=(bytes :ByteSetBitmap) :Unit = {
			bitmap(0) &= ~bytes.bitmap(0)
			bitmap(1) &= ~bytes.bitmap(1)
			bitmap(2) &= ~bytes.bitmap(2)
			bitmap(3) &= ~bytes.bitmap(3)
		}

		@inline def ^=(bytes :ByteSetBitmap) :Unit = {
			bitmap(0) ^= bytes.bitmap(0)
			bitmap(1) ^= bytes.bitmap(1)
			bitmap(2) ^= bytes.bitmap(2)
			bitmap(3) ^= bytes.bitmap(3)
		}

		@inline def &=(bytes :ByteSetBitmap) :Unit = {
			bitmap(0) = bitmap(0) & bytes.bitmap(0)
			bitmap(1) = bitmap(1) & bytes.bitmap(1)
			bitmap(2) = bitmap(2) & bytes.bitmap(2)
			bitmap(3) = bitmap(3) & bytes.bitmap(3)
		}

		@inline def :=(bytes :ByteSetBitmap) :Unit = {
			bitmap(0) = bytes.bitmap(0)
			bitmap(1) = bytes.bitmap(1)
			bitmap(2) = bytes.bitmap(2)
			bitmap(3) = bytes.bitmap(3)
		}


		@inline def foreachInt[@specialized(Unit) U](f: Int => U): Unit = {
			var cell = 0
			do {
				val base = cell*64; var i = 0; var word = bitmap(cell)
				while (word != 0L) {
					if ((word & 1L) != 0) f((base+i) & 0xff)
					i += 1; word >>>= 1
				}
				cell += 1
			}while(cell < 4)
		}

		@inline def foreach[@specialized(Unit) U](f: Byte => U): Unit = {
			var cell = 0
			do {
				val base = cell*64; var i = 0; var word = bitmap(cell)
				while (word != 0L) {
					if ((word & 1L) != 0) f((base+i).toByte)
					i += 1; word >>>= 1
				}
				cell += 1
			}while(cell < 4)
		}

		@inline def reverseForeach(f :Byte=>Unit) :Unit = {
			var cell = 4; val mask = Long.MaxValue
			do {
				val base = cell*64
				cell -= 1
				var i = 1; var word = bitmap(cell)
				while (word != 0L) {
					if ((word & mask) != 0) f((base-i).toByte)
					i += 1; word <<= 1
				}
			}while(cell > 0)
		}

		@inline final def filterNot(p :Byte=>Boolean) :ByteSetBitmap = filter(p, ourTruth = false)
		@inline final def filter(p :Byte=>Boolean) :ByteSetBitmap = filter(p, ourTruth = true)

		def filter(p :Byte=>Boolean, ourTruth :Boolean) :ByteSetBitmap = {
			val copy = this.copy
			var cell = 0
			do {
				val base = cell*64; var i = 0; var word = bitmap(cell)
				while (word != 0L) {
					if ((word & 1L) != 0 && p((base+i).toByte)!=ourTruth)
						copy.bitmap(cell) ^ (1L << i)
					i += 1; word >>>= 1
				}
				cell += 1
			}while(cell < 4)
			copy
		}

		
		def iterator: FitIterator[Byte] =
			new ByteSetIterator(copy)
		
		def sameElements(other :ByteSetBitmap) :Boolean = bitmap sameElements other.bitmap
	}


	@inline private def ByteSetBitmap(first :Long, second :Long, third :Long, fourth :Long) :ByteSetBitmap = {
		val array = new Array[Long](4)
		array(0) = first; array(1) = second; array(2) = third; array(3) = fourth
		new ByteSetBitmap(array)
	}


	
	
	class ByteSetIterator private[ByteSet](bitmap :ByteSetBitmap)
		extends BaseIterator[Byte] with FitIterator[Byte]
	{
		
		private[this] var hd: Int = bitmap.headInt
		
		override def head :Byte = hd.toByte
		
		override def hasNext :Boolean = hd>=0
		
		override def skip() :Unit = { bitmap -= hd.toByte; hd = bitmap.head }
		
		override def next() :Byte = { val res = hd.toByte; bitmap -= res; hd = bitmap.headInt; res }
	}
	
	
	
	class ByteSetBuilder private[ByteSet] extends FitBuilder[Byte, StableByteSet] {
		protected[this] final var bits :ByteSetBitmap = EmptyBitmap()
		
		override def addOne :Byte => Unit = b => bits += b

		override def ++=(xs: FitTraversableOnce[Byte]) :ByteSetBuilder.this.type = xs match {
			case _ if xs.isEmpty => this
			case bytes :ByteSet[_] => bits ++= bytes.bitmap; this
			case _ =>
				val it = xs.toIterator
				while(it.hasNext) this += it.next()
				this
		}

		override def +=(elem: Byte): this.type = { bits += elem; this }
		
		override def result(): StableByteSet = { val res = new StableByteSet(bits); bits = GarbageBitmap; res }
		
		override def clear(): Unit = { bits = EmptyBitmap() }
		
		def count :Int = bits.size
		
	}
}
	
	