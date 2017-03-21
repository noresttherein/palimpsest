package net.turambar.palimpsest.specialty.sets

import scala.collection.generic.CanBuildFrom
import scala.collection.{GenIterable, GenSet, GenTraversableOnce}
import net.turambar.palimpsest.specialty.FitIterator.BaseIterator
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.sets.ByteSet.{ByteSetBitmap, ByteSetBuilder, ByteSetIterator}
import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator, FitTraversableOnce, Specialized}
import net.turambar.palimpsest.specialty.Specialized.{Fun1, Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.sets.ValSet.Mutable

/**
  * @author Marcin Mościcki
  */
private[sets] abstract class ByteSet[This<:ByteSet[This]] private[ByteSet](bytes :ByteSetBitmap)
	extends ValSet[Byte] with SetSpecialization[Byte, This] with OfKnownSize
{
	@inline final private[ByteSet] def bitmap :ByteSetBitmap = bytes

	override protected[this] def mySpecialization = Specialized.SpecializedByte

	override def empty :This = newByteSet(ByteSet.EmptyBitmap())

	protected[this] def newByteSet(bits :ByteSetBitmap) :This

	override final def size: Int = bytes.size
	override def hasFastSize = true
	override def hasDefiniteSize = true
	
	override final def nonEmpty = bytes.nonEmpty
	
	override final def isEmpty = bytes.isEmpty
	
	override final def head = bytes.head
	
	override final def headOption = bytes.headOption
	

	
	override final def apply(elem: Byte): Boolean = bytes.contains(elem)
	
	override final def contains(elem: Byte): Boolean = bytes.contains(elem)

	override final def tail :This = newByteSet(bytes.tail)


	override def ++(elems: FitTraversableOnce[Byte]) = elems match {
		case other :ByteSet[_] => newByteSet(bytes ++ other.bitmap)
		case _ => super.++(elems)
	}

	override def --(elems: FitTraversableOnce[Byte]) = elems match {
		case other :ByteSet[_] => newByteSet(bytes -- other.bitmap)
		case _ => super.--(elems)
	}

	override def intersect(that: GenSet[Byte]) :This = that match {
		case other :ByteSet[_] => newByteSet(bytes & other.bitmap)
		case _ => super.intersect(that)
	}

	override def subsetOf(that :GenSet[Byte]) :Boolean = that match {
		case other :ByteSet[_] => bytes subsetOf other.bitmap
		case _ => super.subsetOf(that)
	}

	override final def foreach[@specialized(Unit) U](f: (Byte) => U): Unit = bytes.foreach(f)
	override protected def reverseForeach(f: (Byte) => Unit): Unit = bytes.reverseForeach(f)



	override final def filterNot(p: (Byte) => Boolean): This = newByteSet(bytes.filterNot(p))
	
	override final def filter(p: (Byte) => Boolean): This = newByteSet(bytes.filter(p))
	override protected[this] def filter(p: (Byte) => Boolean, ourTruth: Boolean): This = newByteSet(bytes.filter(p, ourTruth))

	override final def map[@specialized(Fun1Vals) B, That](f: (Byte) => B)(implicit bf: CanBuildFrom[This, B, That]): That = {
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
	
	
	override final def flatMap[B, That](f: (Byte) => GenTraversableOnce[B])(implicit bf: CanBuildFrom[This, B, That]): That = {
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


	override protected def verifiedCopyTo(xs: Array[Byte], start: Int, total: Int): Int = {
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


	//	override final def fitIterator: FitIterator[Byte] = new ByteSetIterator(bytes.copy)
	override final def iterator :FitIterator[Byte] = new ByteSetIterator(bytes.copy)
	
//	override def newBuilder: FitBuilder[Byte, ByteSet[This]] = new ByteSetBuilder
	
	override def equals(that: Any): Boolean = that match {
		case bytes :ByteSet[_] => bitmap sameElements bytes.bitmap
		case _ => super.equals(that)
	}
	
	override def sameElements[B >: Byte](that: GenIterable[B]): Boolean = that match {
		case bytes :ByteSet[_] => bitmap sameElements bytes.bitmap
		case _ => super.sameElements(that)
	}



	override def stringPrefix = "Set[Byte]"

	override def typeStringPrefix = "ByteSet"
}





private[sets] object ByteSet {
	import java.lang.Long.{bitCount, numberOfTrailingZeros}
	
	final val Empty = new StableByteSet(EmptyBitmap())

	@inline final def newBuilder :FitBuilder[Byte, StableSet[Byte]] = new ByteSetBuilder

	def empty :StableSet[Byte] = Empty

	def apply(bytes :Byte*) :StableSet[Byte] = (newBuilder ++= bytes).result()

	def mutable :MutableSet[Byte] = new MutableByteSet(EmptyBitmap())

	private final val GarbageBitmap = EmptyBitmap()
	@inline private def EmptyBitmap() = new ByteSetBitmap(Array[Long](0L, 0L, 0L, 0L))
	

	final class StableByteSet(bits :ByteSetBitmap) extends ByteSet[StableByteSet](bits) with StableSet[Byte] {
		override protected[this] def newByteSet(bits: ByteSetBitmap) = new StableByteSet(bits)
		override def empty = Empty

		override def +(elem: Byte) = new StableByteSet(bitmap + elem)

		override def -(elem: Byte) = new StableByteSet(bitmap - elem)

		override def newBuilder = new ByteSetBuilder

		override def mutable: Mutable[Byte] = new MutableByteSet(bits.copy)
	}

	final class MutableByteSet(bits :ByteSetBitmap) extends ByteSet[MutableByteSet](bits) with MutableSet[Byte] with MutableSetLike[Byte, MutableByteSet] {
		override def empty = new MutableByteSet(EmptyBitmap())

		override protected[this] def newByteSet(bits: ByteSetBitmap): MutableByteSet = new MutableByteSet(bits)

		override def +=(elem: Byte): this.type = { bitmap += elem; this }

		override def -=(elem: Byte): this.type = { bitmap -= elem; this }

		override def --=(xs: FitTraversableOnce[Byte]) = xs match {
			case other :ByteSet[_] => bitmap --= other.bitmap; this
			case _ => super.--=(xs)
		}

		override def ++=(xs: FitTraversableOnce[Byte]) = xs match {
			case other :ByteSet[_] => bitmap ++= other.bitmap; this
			case _ => super.++=(xs)
		}

		override def stable = new StableByteSet(bitmap.copy)

		override def clone() = new MutableByteSet(bitmap.copy)

		override def count: Int = size
	}


	
	private[ByteSet] class ByteSetBitmap(val bitmap :Array[Long]) extends AnyVal {
		
		@inline def copy = new ByteSetBitmap(Array[Long](bitmap(0), bitmap(1), bitmap(2), bitmap(3)))
		
		@inline def size: Int =
			bitCount(bitmap(0)) + bitCount(bitmap(1)) + bitCount(bitmap(2)) + bitCount(bitmap(3))
		
		@inline def nonEmpty =
			(bitmap(3) | bitmap(2) | bitmap(1) | bitmap(0)) > 0
		
		@inline def isEmpty =
			(bitmap(3) | bitmap(2) | bitmap(1) | bitmap(0)) == 0
		
		@inline def head = {
			val first = bitmap(0); val second = bitmap(1)
			if ((first | second) != 0L)
				if (first!=0L) numberOfTrailingZeros(first).toByte
				else (64 + numberOfTrailingZeros(second)).toByte
			else if (bitmap(2) != 0) (128 + numberOfTrailingZeros(bitmap(2))).toByte
			else if (bitmap(3) != 0) (172 + numberOfTrailingZeros(bitmap(3))).toByte
			else throw new NoSuchElementException("Set[Byte]().head")
		}
		
		@inline def headInt = {
			val first = bitmap(0); val second = bitmap(1)
			if ((first | second) != 0L)
				if (first!=0L) numberOfTrailingZeros(first)
				else 64 + numberOfTrailingZeros(second)
			else if (bitmap(2) != 0) 128 + numberOfTrailingZeros(bitmap(2))
			else if (bitmap(3) != 0) 172 + numberOfTrailingZeros(bitmap(3))
			else -1
		}

		
		@inline def headOption = headInt match {
			case -1 => None
			case n => Some(n.toByte)
		}
		
		
		@inline def tail = new ByteSetBitmap({
			val first = bitmap(0); val second = bitmap(1)
			if ((first | second) != 0)
				if (first != 0) Array[Long](first & (first-1), second, bitmap(2), bitmap(3))
				else Array[Long](0L, second & (second-1), bitmap(2), bitmap(3))
			else {
				val third = bitmap(2)
				if (third!=0) Array[Long](0L, 0L, third & (third-1), bitmap(3))
				else {
					val fourth=bitmap(3)
					if (fourth==0)
						throw new UnsupportedOperationException(s"ByteSet().tail")
					Array[Long](0L, 0L, 0L, fourth & (fourth-1))
				}
			}
		})
		
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
//					if (fourth==0)
//						throw new UnsupportedOperationException(s"ByteSet().tail")
					bitmap(3) = fourth & (fourth-1)
				}
			}
		}
		
		@inline def contains(elem: Byte): Boolean = {
			val i = elem & 0xff
			((bitmap(i / 64) >> i) & 1L) != 0
		}
		
		@inline def +(elem: Byte): ByteSetBitmap = {
			val i = elem & 0xff
			val cell = i / 64
			val mask = 1L << i
			if ((bitmap(cell) & mask) != 0) this
			else {
				val bits = Array[Long](bitmap(0), bitmap(1), bitmap(2), bitmap(3))
				bits(cell) = bitmap(cell) | mask
				new ByteSetBitmap(bits)
			}
		}
		
		@inline def -(elem: Byte): ByteSetBitmap = {
			val i = elem & 0xff
			val cell = i/64
			val mask = 1L << i
			if ((bitmap(cell) & mask)==0) this
			else {
				val bits = Array[Long](bitmap(0), bitmap(1), bitmap(2), bitmap(3))
				bits(cell) = bitmap(cell) ^ mask
				new ByteSetBitmap(bits)
			}
		}

		@inline def ++(bytes :ByteSetBitmap) :ByteSetBitmap = {
			new ByteSetBitmap(Array[Long](
				bitmap(0) | bytes.bitmap(0),
				bitmap(1) | bytes.bitmap(1),
				bitmap(2) | bytes.bitmap(2),
				bitmap(3) | bytes.bitmap(3)
			))
		}

		@inline def --(bytes :ByteSetBitmap) :ByteSetBitmap =
			new ByteSetBitmap(Array[Long](
				bitmap(0) & ~bytes.bitmap(0),
				bitmap(1) & ~bytes.bitmap(1),
				bitmap(2) & ~bytes.bitmap(2),
				bitmap(3) & ~bytes.bitmap(3)
			))

		@inline def &(bytes :ByteSetBitmap) :ByteSetBitmap =
			new ByteSetBitmap(Array[Long](
				bitmap(0) & bytes.bitmap(0),
				bitmap(1) & bytes.bitmap(1),
				bitmap(2) & bytes.bitmap(2),
				bitmap(3) & bytes.bitmap(3)
			))

		@inline def subsetOf(bytes :ByteSetBitmap) :Boolean =
			(bitmap(0) & bytes.bitmap(0)) == bitmap(0) &&
			(bitmap(1) & bytes.bitmap(1)) == bitmap(1) &&
			(bitmap(2) & bytes.bitmap(2)) == bitmap(2) &&
			(bitmap(3) & bytes.bitmap(3)) == bitmap(3)


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


		@inline def foreach[@specialized(Unit) U](f: (Byte) => U): Unit = {
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
			val copy = Array[Long](bitmap(0), bitmap(1), bitmap(2), bitmap(3))
			var cell = 0
			do {
				val base = cell*64; var i = 0; var word = bitmap(cell)
				while (word != 0L) {
					if ((word & 1L) != 0 && p((base+i).toByte)!=ourTruth)
						copy(cell) ^ (1L << i)
					i += 1; word >>>= 1
				}
				cell += 1
			}while(cell < 4)
			new ByteSetBitmap(copy)
		}

		
		def iterator: FitIterator[Byte] =
			new ByteSetIterator(copy)
		
		def sameElements(other :ByteSetBitmap) :Boolean = bitmap sameElements other.bitmap
	}
	
	
	
	
	
	class ByteSetIterator private[ByteSet](bitmap :ByteSetBitmap)
		extends BaseIterator[Byte] with FitIterator[Byte]
	{
		
		private[this] var hd: Int = bitmap.headInt
		
		override def head = hd.toByte
		
		override def hasNext = hd>=0
		
		override def skip() = { bitmap -= hd.toByte; hd = bitmap.head }
		
		override def next() = { val res = hd.toByte; bitmap -= res; hd = bitmap.headInt; res }
	}
	
	
	
	class ByteSetBuilder private[ByteSet] extends FitBuilder[Byte, StableByteSet] {
		protected[this] final var bits = EmptyBitmap()
		
		override def addOne :Byte => Unit = b => bits += b

		override def ++=(xs: FitTraversableOnce[Byte]) = xs match {
			case _ if xs.isEmpty => this
			case bytes :ByteSet[_] => bits ++= bytes.bitmap; this
			case _ =>
				val it = xs.fitIterator
				while(it.hasNext) this += it.next()
				this
		}

		override def +=(elem: Byte): this.type = { bits += elem; this }
		
		override def result(): StableByteSet = { val res = new StableByteSet(bits); bits = GarbageBitmap; res }
		
		override def clear(): Unit = { bits = EmptyBitmap() }
		
		def count = bits.size
		
	}
}
	
	