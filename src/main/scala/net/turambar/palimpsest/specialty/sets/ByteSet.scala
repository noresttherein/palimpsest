package net.turambar.palimpsest.specialty.sets

import scala.collection.generic.CanBuildFrom
import scala.collection.{GenIterable, GenTraversableOnce}
import net.turambar.palimpsest.specialty.FitIterator.BaseIterator
import net.turambar.palimpsest.specialty.sets.ByteSet.{ByteSetBitmap, ByteSetBuilder, ByteSetIterator}
import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator, Specialized}
import net.turambar.palimpsest.specialty.Specialized.{Fun1, Fun2, Fun1Vals}

/**
  * @author Marcin Mościcki
  */
class ByteSet private[ByteSet](bytes :ByteSetBitmap) extends FitSet[Byte] {
	private[ByteSet] def bitmap = bytes

	override protected[this] def mySpecialization = Specialized.SpecializedByte
	override def empty = ByteSet.Empty

	override final def size: Int = bytes.size
	override def hasFastSize = false
	override def hasDefiniteSize = true
	
	override final def nonEmpty = bytes.nonEmpty
	
	override final def isEmpty = bytes.isEmpty
	
	override final def head = bytes.head
	
	override final def headOption = bytes.headOption
	
	override final def tail = new ByteSet(bytes.tail)
	
	override final def apply(elem: Byte): Boolean = bytes.contains(elem)
	
	override final def contains(elem: Byte): Boolean = bytes.contains(elem)
	
	override final def +(elem: Byte): FitSet[Byte] = new ByteSet(bytes + elem)
	
	override final def -(elem: Byte): FitSet[Byte] = new ByteSet(bytes - elem)
	
	override final def foreach[@specialized(Unit) U](f: (Byte) => U): Unit = bytes.foreach(f)
	override protected def reverseForeach(f: (Byte) => Unit): Unit = bytes.reverseForeach(f)



	override final def filterNot(p: (Byte) => Boolean): FitSet[Byte] = new ByteSet(bytes.filterNot(p))
	
	override final def filter(p: (Byte) => Boolean): FitSet[Byte] = new ByteSet(bytes.filter(p))
	override protected[this] def filter(p: (Byte) => Boolean, ourTruth: Boolean): FitSet[Byte] =
		new ByteSet(bytes.filter(p, ourTruth))

	override final def map[@specialized(Fun1Vals) B, That](f: (Byte) => B)(implicit bf: CanBuildFrom[FitSet[Byte], B, That]): That = {
		val bu = bf(this)
		if (nonEmpty) {
			bu.sizeHint(size)
			var i = 0
			while (i < 256) {
				val b = i.toByte
				if (contains(b)) bu += f(b)
				i += 1
			}
		}
		bu.result()
	}
	
	
	override final def flatMap[B, That](f: (Byte) => GenTraversableOnce[B])(implicit bf: CanBuildFrom[FitSet[Byte], B, That]): That = {
		val bu = bf(this); val s = size
		if (nonEmpty) {
			var i = 0
			while (i < 256) {
				val b = i.toByte
				if (contains(b)) bu ++= f(b).seq
				i += 1
			}
		}
		bu.result()
	}
	
	override final def fitIterator: FitIterator[Byte] = new ByteSetIterator(bytes.copy)
	override final def iterator :FitIterator[Byte] = new ByteSetIterator(bytes.copy)
	
	override def newBuilder: FitBuilder[Byte, ByteSet] = new ByteSetBuilder
	
	override def equals(that: Any): Boolean = that match {
		case bytes :ByteSet => bitmap sameElements bytes.bitmap
		case _ => super.equals(that)
	}
	
	override def sameElements[B >: Byte](that: GenIterable[B]): Boolean = that match {
		case bytes :ByteSet => bitmap sameElements bytes.bitmap
		case _ => super.sameElements(that)
	}

	override def stringPrefix = "ByteSet"
}





object ByteSet {
	import java.lang.Long.{bitCount, numberOfTrailingZeros}
	
	final val Empty = new ByteSet(EmptyBitmap())

	@inline final def newBuilder :FitBuilder[Byte, ByteSet] = new ByteSetBuilder

	def empty :ByteSet = Empty

	def apply(bytes :Byte*) :ByteSet = (newBuilder ++= bytes).result()

	private final val GarbageBitmap = EmptyBitmap()
	@inline private def EmptyBitmap() = new ByteSetBitmap(Array[Long](0L, 0L, 0L, 0L))
	





	
	private[ByteSet] class ByteSetBitmap(val bitmap :Array[Long]) extends AnyVal {
		
		def copy = new ByteSetBitmap(Array[Long](bitmap(0), bitmap(1), bitmap(2), bitmap(3)))
		
		def size: Int =
			bitCount(bitmap(0)) + bitCount(bitmap(1)) + bitCount(bitmap(2)) + bitCount(bitmap(3))
		
		def nonEmpty =
			(bitmap(3) | bitmap(2) | bitmap(1) | bitmap(0)) > 0
		
		def isEmpty =
			(bitmap(3) | bitmap(2) | bitmap(1) | bitmap(0)) == 0
		
		def head =
			if ((bitmap(3) | bitmap(2)) != 0)
				if (bitmap(3)!=0) numberOfTrailingZeros(bitmap(3)).toByte
				else (64 + numberOfTrailingZeros(bitmap(2))).toByte
			else if (bitmap(1)!=0) (128 +  numberOfTrailingZeros(bitmap(1))).toByte
			else if (bitmap(0)!=0) (172 + numberOfTrailingZeros(bitmap(0))).toByte
			else throw new NoSuchElementException(s"Set().head")
		
		def headInt =
			if ((bitmap(3) | bitmap(2)) != 0)
				if (bitmap(3)!=0) numberOfTrailingZeros(bitmap(3)) & 0xff
				else (64 + numberOfTrailingZeros(bitmap(2))) & 0xff
			else if (bitmap(1)!=0) (128 +  numberOfTrailingZeros(bitmap(1))) & 0xff
			else if (bitmap(0)!=0) (172 + numberOfTrailingZeros(bitmap(0))) & 0xff
			else -1
		
		
		def headOption = try {
			Some(head)
		} catch {
			case e :NoSuchElementException => None
		}
		
		
		def tail = new ByteSetBitmap({
			val first = bitmap(3); val second = bitmap(2)
			if ((first | second) != 0)
				if (first != 0) Array[Long](bitmap(0), bitmap(1), second, first & (first-1))
				else Array[Long](bitmap(0), bitmap(1), second & (second-1), 0L)
			else {
				val third = bitmap(1)
				if (third!=0) Array[Long](bitmap(0), third & (third-1), 0L, 0L)
				else {
					val fourth=bitmap(0)
					if (fourth==0)
						throw new UnsupportedOperationException(s"ByteSet().tail")
					Array[Long](fourth & (fourth-1), 0L, 0L, 0L)
				}
			}
		})
		
		def removeFirst() :Unit = {
			val first = bitmap(3); val second = bitmap(2)
			if ((first | second) != 0)
				if (first!=0) bitmap(3) = first & (first-1)
				else bitmap(2) = second & (second-1)
			else {
				val third = bitmap(1)
				if (third!=0) bitmap(1) = third & (third-1)
				else {
					val fourth = bitmap(0)
					if (fourth==0)
						throw new UnsupportedOperationException(s"ByteSet().tail")
					bitmap(0) = fourth & (fourth-1)
				}
			}
		}
		
		def contains(elem: Byte): Boolean = {
			val i = elem & 0xff
			(bitmap(3 - (i/64)) & (1L << i % 64)) > 0
		}
		
		def +=(elem :Byte) :Unit = {
			val i = elem & 0xff
			val cell = 3 - (i /64)
			val mask = 1L << i % 64
			bitmap(cell) = bitmap(cell) | mask
		}
		
		
		def +(elem: Byte): ByteSetBitmap = {
			val i = elem & 0xff
			val cell = 3 - (i / 64)
			val mask = 1L << i % 64
			if ((bitmap(cell) & mask) > 0) this
			else {
				val bits = Array[Long](bitmap(0), bitmap(1), bitmap(2), bitmap(3))
				bits(cell) = bitmap(cell) | mask
				new ByteSetBitmap(bits)
			}
		}
		
		def -(elem: Byte): ByteSetBitmap = {
			val i = elem & 0xff
			val cell = 3 - (i/64)
			val mask = 1L << i % 64
			if ((bitmap(cell) & mask)==0) this
			else {
				val bits = Array[Long](bitmap(0), bitmap(1), bitmap(2), bitmap(3))
				bits(cell) = bitmap(cell) ^ mask
				new ByteSetBitmap(bits)
			}
		}
		
		def -=(elem :Byte) :Unit = {
			val i = elem & 0xff
			val cell = 3 - (i/64)
			val mask = 1L << i % 64
			bitmap(cell) = bitmap(cell) ^ mask
		}
		
		
		
		def foreach[@specialized(Unit) U](f: (Byte) => U): Unit = {
			var i=0
			while(i<256) {
				if (contains(i.toByte)) f(i.toByte)
				i += 1
			}
		}

		def reverseForeach(f :Byte=>Unit) :Unit = {
			var i = 255
			while(i>=0) {
				if (contains(i.toByte)) f(i.toByte)
				i -= 1
			}
		}

		@inline final def filterNot(p :Byte=>Boolean) :ByteSetBitmap = filter(p, ourTruth = false)
		@inline final def filter(p :Byte=>Boolean) :ByteSetBitmap = filter(p, ourTruth = true)

		def filter(p :Byte=>Boolean, ourTruth :Boolean) :ByteSetBitmap = {
			val copy = Array[Long](bitmap(0), bitmap(1), bitmap(2), bitmap(3))
			var i = 0
			while(i < 256) {
				val b = i.toByte
				if (contains(b) && p(b)!=ourTruth)
					copy(3-i/64) ^ 1L << i //jvm does % 63
				i += 1
			}
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
	
	
	
	class ByteSetBuilder private[ByteSet] extends FitBuilder[Byte, ByteSet] {
		protected[this] final var bits = EmptyBitmap()
		
		override def addOne :Byte => Unit = b => bits += b
		
		override def +=(elem: Byte): this.type = { bits += elem; this }
		
		override def result(): ByteSet = { val res = new ByteSet(bits); bits = GarbageBitmap; res }
		
		override def clear(): Unit = { bits = EmptyBitmap() }
		
		def count = bits.size
		
	}
}
	
	