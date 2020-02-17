package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.Vals.OfKnownSize
import net.turambar.palimpsest.specialty.{?, Blank, Vals, Sure}
import net.turambar.palimpsest.specialty.iterables.AptIterable
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.sets.IntBitSet.BitmapSize

import scala.compat.Platform.ConcurrentModificationException

/** A mutable bit set backed by an array covering the whole `Int` value range.
  * Used in particular by other sets when their size reaches a limit specific to their implementation.
  * @author Marcin Mościcki
  */
class IntBitSet private (bitmap :Array[Int], private[this] var bitcount :Int, protected[this] var version :Long = 0)
	extends MutableOrderedSet[Int] with MutableSetSpecialization[Int, IntBitSet] with OrderedSetTemplate[Int, IntBitSet]
	   with OfKnownSize
{ outer =>

	override def empty :IntBitSet = new IntBitSet(Array.emptyIntArray, 0)

	protected def membership :Array[Int] = bitmap

	override def size :Int = bitcount
	protected[this] def size_=(bitcount :Int) :Unit = this.bitcount = bitcount

	override def +=(elem :Int) :this.type = {
		val neg = elem ^ 0x80000000
		val cell = neg >>> 5
		val bit = 1 << (neg & 0x1f)
		val neighbourhood = bitmap(cell)
		if ((neighbourhood & bit) == 0) {
			bitmap(cell) = neighbourhood | bit
			bitcount += 1
			version += 1
		}
		this
	}

	override def -=(elem :Int) :this.type = {
		val neg = elem ^ 0x80000000
		val cell = neg >>> 5
		val bit = 1 << (neg & 0x1f)
		val neighbourhood = bitmap(cell)
		if ((neighbourhood & bit) != 0) {
			bitmap(cell) = neighbourhood & ~bit
			bitcount -= 1
			version += 1
		}
		this
	}

	override def flip(elem :Int) :Boolean = {
		val neg = elem ^ 0x80000000
		val cell = neg >>> 5
		val bit = 1 << (neg & 0x1f)
		val neighbourhood = bitmap(cell)
		version += 1
		if ((neighbourhood & bit) == 0) {
			bitmap(cell) = neighbourhood | bit
			bitcount += 1
			true
		} else {
			bitmap(cell) = neighbourhood & ~bit
			bitcount -= 1
			false
		}
	}

	override def contains(elem :Int) :Boolean = {
		val neg = elem ^ 0x80000000
		val cell = neg >>> 5
		val bit = 1 << (neg & 0x1f)
		(bitmap(cell) & bit) != 0
	}

	override def foreach[@specialized(Unit) U](f :Int => U) :Unit = {
		var cell = 0
		while (cell < BitmapSize) {
			var bits = bitmap(cell)
			var rem = 0
			while (bits != 0) {
				if ((bits & 1) != 0)
					f(((cell << 5) | rem) ^ 0x80000000)
				bits >>>= 1; rem += 1
			}
			cell += 1
		}
	}

	override protected def reverseForeach(f :Int => Unit) :Unit = {
		var cell = BitmapSize - 1
		while (cell >= 0) {
			var bits = bitmap(cell)
			var rem = 0x1f
			while (bits != 0) {
				if ((bits & 0x80000000) != 0)
					f(((cell << 5) | rem) ^ 0x80000000)
				bits <<= 1; rem -= 1
			}
			cell -= 1
		}
	}

	override def iterator :AptIterator[Int] = new IntBitSetIterator(bitmap)

	override def keysIteratorFrom(start :Int) :AptIterator[Int] = {
		val neg = start ^ 0x80000000
		val cell = neg >>> 5
		val rem = neg & 0x1f
		if (rem == 0)
			new IntBitSetIterator(bitmap, cell - 1, 31)
		else
			new IntBitSetIterator(bitmap, cell, rem - 1)
	}

	override def keyAt(n :Int) :Int = iterator.drop(n).next()

	override def rangeImpl(fromKey : ?[Int], untilKey : ?[Int]) :IntBitSet =
		if (untilKey.isDefined && untilKey.get == Int.MinValue)
			rangeImpl(Sure(Int.MaxValue), Sure(Int.MinValue + 1))
		else
			new IntBitSet(bitmap, -1, -1) {
				private[this] val start = fromKey getOrElse Int.MinValue
				private[this] val end = untilKey map (_ - 1) getOrElse Int.MaxValue

				override def size :Int =
					if (version == outer.version)
						super.size
					else if (end < start) {
						version = outer.version
						size = 0
						0
					} else {
						version = outer.version
						var bitcount = 0
						foreach { _ => bitcount += 1 }
						size = bitcount
						bitcount
					}

				override def +=(elem :Int) = {
					if (version == outer.version) {
						outer += elem
						size = outer.size
						version = outer.version
					} else {
						outer += elem
					}
					this
				}


				override def -=(elem :Int) = {
					if (version == outer.version) {
						outer -= elem
						size = outer.size
						version = outer.version
					} else {
						outer -= elem
					}
					this
				}

				override def flip(elem :Int) :Boolean = {
					if (version == outer.version) {
						val res = outer flip elem
						size = outer.size
						version = outer.version
						res
					} else {
						outer flip elem
					}
				}

				override def contains(elem :Int) =
					elem >= start && elem <= end && super.contains(elem)


				override def foreach[@specialized(Unit) U](f :Int => U) :Unit =
					if (start <= end) {
						val bitmap = membership
						val lastCell = (end ^ 0x80000000) >>> 5
						val lastBits = end & 0x1f
						var cell = (start ^ 0x80000000) >>> 5
						var rem = start & 0x1f
						var bits = bitmap(cell) >>> rem
						while (cell < lastCell) {
							while (bits != 0) {
								if ((bits & 1) != 0)
									f((cell << 5) | rem)
								rem += 1
								bits >>>= 1
							}
							rem = 0
							cell += 1
							bits = bitmap(cell)
						}
						while (bits != 0 && rem <= lastBits) {
							if ((bits & 1) != 0)
								f((cell << 5) | rem)
							rem += 1
							bits >>>= 1
						}
					}

				override protected def reverseForeach(f :Int => Unit) :Unit =
					if (start <= end) {
						val bitmap = membership
						val firstCell = (start ^ 0x80000000) >>> 5
						val firstBits = start & 0x1f
						var cell = (end ^ 0x80000000) >>> 5
						var rem = end & 0x1f
						var bits = bitmap(cell) & (0xffffffff >>> (31 - rem))
						var bit = 1 << rem
						while (cell > firstCell) {
							while (bits != 0 && bit != 0) {
								if ((bits & bit) != 0)
									f((cell << 5) | rem)
								bits &= ~bit
								rem -= 1
								bit >>>= 1
							}
							cell -= 1
							rem = 31
							bits = bitmap(cell)
							bit = 0x80000000
						}
						while (bits != 0 && rem >= firstBits) {
							if ((bits & bit) != 0)
								f((cell << 5) | rem)
							rem -= 1
							bit >>>= 1
						}
					}

				override def iterator =
					outer.iteratorFrom(start).takeWhile(_ <= end)

				override def keysIteratorFrom(start :Int) =
					if (start < this.start) iterator
					else outer.keysIteratorFrom(start) takeWhile { _ <= end }

				override def rangeImpl(fromKey : ?[Int], untilKey : ?[Int]) =
					if (untilKey.isDefined && untilKey.get == Int.MinValue)
						outer.rangeImpl(Sure(Int.MaxValue), Sure(Int.MinValue - 1))
					else {
						val maxFrom = fromKey getOrElse start max start
						val minUntil =
							if (end == Int.MaxValue) untilKey
							else untilKey map { _ min end + 1 } orElse Sure(end + 1)
						outer.rangeImpl(Sure(maxFrom), minUntil)
					}

			}

	override implicit def ordering :ValOrdering[Int] = ValOrdering.IntOrdering

	override def stringPrefix = "IntBitSet"



	private class IntBitSetIterator(bitmap :Array[Int], private[this] var cell :Int = -1, private[this] var rem :Int = 31, version :Long = outer.version)
		extends AptIterator[Int]
	{
		private[this] var last :Int = _
		skip()

		override def hasNext :Boolean = cell < BitmapSize

		override def head :Int = last

		override def next() :Int = { val res = last; skip(); res }

		override def skip() :Unit = {
			if (version != outer.version)
				throw new ConcurrentModificationException
			do {
				rem += 1
				if (rem == 32) {
					cell += 1
					rem = 0
				}
			} while(cell < BitmapSize && (bitmap(cell) & (1 << rem)) == 0)
			last = ((cell << 5) | rem) ^ 0x80000000
		}

	}


}



object IntBitSet {

	def empty :IntBitSet = new IntBitSet(new Array[Int](BitmapSize), 0)

	def apply(values :Int*) :IntBitSet = apply(AptIterator.adapt(values.iterator))

	def apply(elements :Vals[Int]) :IntBitSet = {
		val res = new IntBitSet(new Array[Int](BitmapSize), 0)
		elements foreach res.+= //{ i :Int => res += i }
		res
	}

	private final val BitmapSize = 0x08000000


}
