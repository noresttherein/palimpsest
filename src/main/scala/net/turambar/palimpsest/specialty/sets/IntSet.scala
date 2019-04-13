package net.turambar.palimpsest.specialty.sets
/*
import java.util

import net.turambar.palimpsest.specialty.FitIterable.IterableMapping
import net.turambar.palimpsest.specialty.FitIterator.{BaseIterator, FastSizeIterator, MappedIterator}
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.sets.IntSet.IntSetIterator
import net.turambar.palimpsest.specialty.{?, FitBuilder, FitIterator, Specialized}
import net.turambar.palimpsest.specialty.Specialized.{Fun1, Fun1Res, Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.iterables.EmptyIterable
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.ordered.ValOrdering.IntOrdering

import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable, AbstractIterator, BitSet, BitSetLike, GenTraversableOnce, Set, SortedSet, SortedSetLike}



/**
  * @author Marcin Mościcki
  */
@deprecated("")
class IntSet private[IntSet](negative :BitSet, positive :BitSet, override val size :Int) extends StableSet[Int] with OfKnownSize {

	private[IntSet] def this(negative :BitSet, positive :BitSet) = this(negative, positive, negative.size + positive.size)
	private[IntSet] def this(positive :BitSet) = this(BitSet.empty, positive, positive.size)

	override def hasFastSize = true
	override def hasDefiniteSize = true

	override protected[this] def mySpecialization: Specialized[Int] = Specialized.SpecializedInt
	override def empty = IntSet.Empty

	override def foreach[@specialized(Unit) U](f: (Int) => U) = {
		negative foreach { i => f(-i) }
		positive foreach f
	}
	override protected def reverseForeach(f: (Int) => Unit): Unit = {
		positive.foldRight(()) { (i, _) => f(i) }
		negative.foldRight(()) { (i, _) => f(-i) }
	}

	override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, Int) => O) = {
		val half = negative.foldLeft(z){ (acc, i) => op(acc, -i) }
		positive.foldLeft(half)(op)
	}

	override def foldRight[@specialized(Fun2) O](z :O)(op :(Int, O) => O) = {
		val half = positive.foldRight(z)(op)
		negative.foldRight(half){ (i, acc) => op(-i, acc) }
	}

	override def contains(elem: Int): Boolean =
		elem>=0 && positive.contains(elem) || negative.contains(-elem)



	override protected[this] def filter(p: (Int) => Boolean, ourTruth: Boolean): StableSet[Int] =
		if (size==0) this
		else
			new IntSet(negative.filter{ i => p(-i)==ourTruth }, positive.filter { i => p(i)==ourTruth })

	override def filter(p: (Int) => Boolean) =
		if (size==0) this
		else new IntSet(negative.filter{ i => p(-i) }, positive.filter(p))

	override def filterNot(p: (Int) => Boolean) =
		if (size==0) this
		else new IntSet(negative.filter{i => !p(-i)}, positive.filterNot(p))


	override def map[@specialized(Fun1Vals) O, That](f: (Int) => O)(implicit bf: CanBuildFrom[StableSet[Int], O, That]) = {
		val b = bf(this)
		negative foreach { i => b += f(-i) }
		positive foreach { i => b += f(i) }
		b.result()
	}

	override def flatMap[U, That](f: (Int) => GenTraversableOnce[U])(implicit bf: CanBuildFrom[StableSet[Int], U, That]) :That = {
		val b = bf(this)
		negative foreach { i => b ++= f(-i).seq }
		positive foreach { i => b ++= f(i).seq }
		b.result()
	}


	override def +(elem: Int): IntSet =
		if (elem>=0)
			if (positive.contains(elem)) this
			else new IntSet(negative, positive + elem, size+1)
		else
			if (negative.contains(-elem)) this
			else new IntSet(negative + (-elem), positive, size+1)
	
	override def -(elem: Int): IntSet =
		if (elem>=0)
			if(positive.contains(elem)) new IntSet(negative, positive-elem, size - 1)
			else this
		else
			if (negative.contains(elem)) new IntSet(negative - (-elem), positive, size-1)
			else this
	
	override def iterator: FitIterator[Int] = new IntSetIterator(negative, positive, size)
//	override def toIterator :FitIterator[Int] = new IntSetIterator(negative, positive, size)

	override def mutable: MutableSet[Int] = MutableSet.from(this)

	override def stringPrefix = "IntSet"
}


object IntSet {
	import scala.collection.mutable.{BitSet => MutableBitSet}
	final val Empty :StableSet[Int] = new IntSet(BitSet.empty, BitSet.empty, 0)
	@inline final def empty = Empty
	
	def newBuilder :FitBuilder[Int, StableSet[Int]] = new IntSetBuilder(MutableBitSet.empty, MutableBitSet.empty, 0)

	def singleton(value :Int) :StableSet[Int] = Empty + value
	
	def mutable :MutableSet[Int] = Mutable.empty

	object Sorted {
		final val Empty :StableOrderedSet[Int] = new EmptyIterable[Int, StableOrderedSet[Int]] with StableOrderedSet[Int] with EmptySetSpecialization[Int, StableOrderedSet[Int]]{
			override implicit val ordering: ValOrdering[Int] = IntOrdering
			override def keysIteratorFrom(start: Int): FitIterator[Int] = FitIterator.empty[Int]
			override def rangeImpl(from: ?[Int], until: ?[Int]): StableOrderedSet[Int] = this

			override def contains(elem: Int): Boolean = false

			override def +(elem: Int): StableOrderedSet[Int] = singleton(elem)
			override def -(elem: Int): StableOrderedSet[Int] = this
		}

		def singleton(value :Int) :StableOrderedSet[Int] = ???

		def newBuilder :FitBuilder[Int, StableOrderedSet[Int]] = ???
		
		def mutable :MutableOrderedSet[Int] = ???
	}

	
	object Mutable {
		def empty :MutableSet[Int] = MutableSet.from(IntSet.Empty)
	}



	trait MappedIntSet[+S<:ValSet[Int] with SetSpecialization[Int, S], @specialized(Byte, Short, Char, Float) Y, +Repr <: ValSet[Y] with SetSpecialization[Y, Repr]]
		extends IterableMapping[Int, S, Y, Repr] with ValSet[Y] with SetSpecialization[Y, Repr]
	{ //this :IterableMapping[Int, FitSet[Int], Y, Repr] => //this :Repr =>
		@inline override protected def forSource[@specialized(Fun1Res) O](f :Y=>O) = { x :Int => f(my(x)) }
//		@inline override final protected def my(x: Int): Y = from(x)
		override protected[this] def fromSource(col: S): Repr
		override protected[this] val source :S

		protected def my(x :Int) :Y
		protected def toInt(y :Y) :Int
		protected val to :Y=>Int = toInt
//		@inline override final protected def from = From

		override def empty :Repr = fromSource((source :SetSpecialization[Int, S]).empty)
		override def mutable :MutableSet[Y] = new MutableViewAs(from, to)(source.mutable)
//		override def stable =

		override def head :Y = from(source.head)
		override def last :Y = from(source.last)

		override def contains(elem: Y): Boolean = source.contains(toInt(elem))


		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, Y) => O) =
			source.foldLeft(z)( (o :O, x :Int) => op(o, my(x)))

		override def foldRight[@specialized(Fun2) O](z :O)(op :(Y, O)=>O) =
			source.foldRight(z)( (x :Int, o :O) => op(my(x), o) )


		override def +(elem: Y): Repr = fromSource(source + toInt(elem))

		override def -(elem: Y): Repr = fromSource(source - toInt(elem))

		override def iterator: FitIterator[Y] = new MappedIterator(from)(source.iterator)

		override def newBuilder =
			source.newBuilder.mapInput(to).mapResult(fromSource)

		override protected def uncheckedCopyTo(xs: Array[Y], start: Int, total: Int): Int =
			if (total >= source.size) {
				var idx = start
				source.foreach { i: Int => xs(idx) = my(i); idx += 1 }
				source.size
			} else {
				iterator.copyToArray(xs, start, total)
				total
			}

	}



	trait MappedMutableIntSet[
			+S<:MutableSet[Int] with SetSpecialization[Int, S],
			@specialized(Byte, Short, Char, Float) Y,
			+Repr<:MutableSet[Y] with MutableSetSpecialization[Y, Repr] with SetSpecialization[Y, Repr]
		] extends IterableMapping[Int, S, Y, Repr] with MutableSet[Y] with MutableSetSpecialization[Y, Repr]
				  with MappedIntSet[S, Y, Repr] //with MutableSet[Y]
	{ //this :IterableMapping[Int, MutableSet[Int], Y, Repr]  => //this :Repr =>
//		override protected[this] val source :MutableSet[Int]
		override def stable :ValSet.Stable[Y] = new ViewAs(from, to)(source.stable)

		override def add(elem: Y) = source.add(toInt(elem))
		override def remove(elem: Y) = source.remove(toInt(elem))

		override def +=(elem: Y): this.type = { source -= toInt(elem); this }
		override def -=(elem: Y): this.type = { source -= toInt(elem); this }

		override def +=(elem1: Y, elem2: Y, elems: Y*) :this.type = {
			source += (toInt(elem1), toInt(elem2), elems.map(toInt):_*); this
		}
		override def -=(elem1: Y, elem2: Y, elems: Y*) = {
			source.-=(toInt(elem1), toInt(elem2), elems.map(toInt):_*); this
		}

		override def sizeHint(expect: Int) = source.sizeHint(expect)

		override def count = size

		override def newBuilder = (source :SetSpecialization[Int, S]).newBuilder.mapInput(to).mapResult(fromSource)
	}



	class ViewAs[@specialized(Byte, Short, Char, Float) Y]
			(override final val from :Int=>Y, override final val to :Y=>Int)(protected val source :StableSet[Int])
		extends MappedIntSet[StableSet[Int], Y, StableSet[Y]] with StableSet[Y]
	{
		override protected def toInt(y: Y): Int = to(y)
		override protected def my(x: Int): Y = from(x)
		override protected[this] def fromSource(col: StableSet[Int]): StableSet[Y] = new ViewAs(from, to)(col)
	}

	class MutableViewAs[@specialized(Byte, Short, Char, Float) Y]
			(override final val from :Int=>Y, override final val to :Y=>Int)(protected val source :MutableSet[Int])
		extends MappedMutableIntSet[MutableSet[Int], Y, MutableSet[Y]]
	{
		override protected def toInt(y: Y): Int = to(y)
		override protected def my(x: Int): Y = from(x)
		override protected[this] def fromSource(col: MutableSet[Int]): MutableSet[Y] = new MutableViewAs(from, to)(col)
	}


	class SortedViewAs[@specialized(Byte, Short, Char, Float) Y]
			(override final val from :Int=>Y, override final val to :Y=>Int)(override val source :OrderedSet.Stable[Int])
		extends IterableMapping[Int, StableOrderedSet[Int], Y, OrderedSet.Stable[Y]] with OrderedSet.Stable[Y]
				with MappedIntSet[StableOrderedSet[Int], Y, OrderedSet.Stable[Y]]
	{
		implicit override def ordering = source.ordering.on(to)

		override protected def toInt(y: Y): Int = to(y)
		override protected def my(x: Int): Y = from(x)

		override def mutable = new MutableSortedViewAs(from, to)(source.mutable)

		override protected[this] def fromSource(col: StableOrderedSet[Int]): StableOrderedSet[Y] =
			new SortedViewAs(from, to)(col)

		override def rangeImpl(from: Option[Y], until: Option[Y]): StableOrderedSet[Y] =
			(from, until) match {
				case (Some(f), Some(t)) => fromSource(source.range(toInt(f), toInt(t)))
				case (Some(f), _) => fromSource(source.from(toInt(f)))
				case (_, Some(t)) => fromSource(source.until(toInt(t)))
				case _ => this
			}
		

		override def keysIteratorFrom(start: Y): FitIterator[Y] =
			new MappedIterator(from)(source.keysIteratorFrom(toInt(start)))
		
//		override def iterator :FitIterator[Y] = new MappedIterator(from)(source.iterator)
	}


	class MutableSortedViewAs[@specialized(Byte, Short, Char, Float) Y]
			(override final val from :Int=>Y, override final val to :Y=>Int)(override val source :MutableSet.Ordered[Int])
		extends IterableMapping[Int, MutableSet.Ordered[Int], Y, MutableSet.Ordered[Y]] with MutableSet.Ordered[Y]
				with MappedMutableIntSet[MutableSet.Ordered[Int], Y, MutableSet.Ordered[Y]]
	{
		implicit override def ordering = source.ordering.on(to)

		override protected def toInt(y: Y): Int = to(y)
		override protected def my(x: Int): Y = from(x)

		override def mutable :MutableOrderedSet[Y] = this
		override def stable = new SortedViewAs(from, to)(source.stable)

		override protected[this] def fromSource(col: MutableSet.Ordered[Int]): MutableSet.Ordered[Y] =
			new MutableSortedViewAs(from, to)(col)
//			fromSorted(col.asInstanceOf[MutableSet.Ordered[Int]])

		@inline final private[this] def fromSorted(col :MutableSet.Ordered[Int]) :MutableSet.Ordered[Y] =
			new MutableSortedViewAs(from, to)(col)

		override def rangeImpl(from: Option[Y], until: Option[Y]): MutableSet.Ordered[Y] = fromSorted(
			(from, until) match {
				case (Some(f), Some(t)) => source.rangeImpl(Some(toInt(f)), Some(toInt(t)))
				case (Some(f), _) => source.rangeImpl(Some(toInt(f)), None)
				case (_, Some(t)) => source.rangeImpl(None, Some(toInt(t)))
				case _ => source.rangeImpl(None, None)
			}
		)

		override def keysIteratorFrom(start: Y): FitIterator[Y] =
			new MappedIterator(from)(source.keysIteratorFrom(toInt(start)))
		
//		override def iterator :FitIterator[Y] = new MappedIterator(from)(source.iterator)
	}






	private class IntSetIterator(negative :BitSet, positive :BitSet, private[this] var left :Int) extends FastSizeIterator[Int] with FitIterator[Int] {
		override def size = left
		override def hasNext = left>0
		override def ofAtLeast(n :Int) = left >= n
		
		var head :Int =
			if (left<=0) 0
			else if (negative.nonEmpty) -negative.last
			else positive.head
		
		
		override def next(): Int = {
			val res = head
			left -= 1
			if (left>0) {
				while (head < 0) {
					head += 1
					if (negative.contains(-head)) return res
				}
				while (!positive.contains(head)) head += 1
			}
			res
		}
		

		override def skip(): Unit = {
			left -= 1
			if (left>0) {
				do head += 1 while (head < 0 && !negative.contains(-head))
				if (head >= 0)
					while (!positive.contains(head)) head += 1
			}
		}
	}






	private class IntSetBuilder(negative :MutableBitSet, positive :MutableBitSet, private[this] var size :Int)
		extends FitBuilder[Int, IntSet]
	{
		override def +=(elem: Int): this.type = {
			if (elem >= 0)
				if (!positive.contains(elem)){
					positive += elem
					size += 1
				}
			else if (!negative.contains(-elem)) {
				negative += -elem
				size += 1
			}
			this
		}
		
		override def result(): IntSet = new IntSet(negative, positive, size)
		
		override def count: Int = size
		
		override def clear(): Unit = {
			negative.clear()
			positive.clear()
			size=0
		}
	}






/*
	private class ContinuousBitSet(private[this] var words :Array[Long], precalculatedSize :Int = -1)
		extends BitSet with BitSetLike[ContinuousBitSet] //todo: base  class
				with FitSet.Sorted[Int] with SetSpecialization[Int, ContinuousBitSet]
	{

		private[IntSet] def bitmap = words
		private[IntSet] def bitmap_=(array :Array[Long]) :Unit = words = array

		@volatile private[this] var bitCount = precalculatedSize

		override def size = {
			if (bitCount<0)
				bitCount = super.size
			bitCount
		}
		protected def size_=(n :Int) :Unit = bitCount = n
		override def hasFastSize = bitCount >= 0
//		override def hasDefiniteSize = true

		@inline final protected def inc :Int = if (bitCount>=0) bitCount+1 else -1
		@inline final protected def dec :Int = if (bitCount>0) bitCount-1 else -1
		@inline final protected def size_++() :Unit = if (bitCount>=0) bitCount += 1
		@inline final protected def size_--() :Unit = if (bitCount>0) bitCount -= 1

		override def seq: FitSet[Int] = this
		override def empty: ContinuousBitSet = new ContinuousBitSet(Array.emptyLongArray, 0)

		override protected def nwords: Int = words.length
		override protected def word(idx: Int): Long = words(idx)

		override protected def fromBitMaskNoCopy(elems: Array[Long]): ContinuousBitSet =
			fromBitMaskNoCopy(elems, -1)

		protected def fromBitMaskNoCopy(elems :Array[Long], bitCount :Int) :ContinuousBitSet =
			new ContinuousBitSet(elems, bitCount)

		override protected def reverseForeach(f: (Int) => Unit): Unit = {
			var idx = words.length
			while(idx > 0) {
				var n = idx << 6; idx -= 1
				var w = words(idx)
				while(w != 0L) {
					n -= 1
					if ((w & 0x8000000000000000L)!=0) f(n)
					w <<= 1
				}
			}
		}

		override protected[this] def filter(p: (Int) => Boolean, where: Boolean): ContinuousBitSet = {
			val copy = util.Arrays.copyOf(words, words.length)
			var idx = 0; val max = words.length; var count = 0
			while(idx < max) {
				var w = words(idx)
				var n = idx << 6
				while(w != 0L) {
					if ((w & 1L) == 1L)
						if(p(n)!=where)
							copy(idx) ^= 1L << n
						else
							count += 1
					w >>= 1
					n += 1
				}
				idx += 1
			}
			if (count==0) empty
			else {
				idx = copy.length-1
				if (copy(idx)==0L) {
					do { idx-=1 } while(copy(idx)==0L)
					fromBitMaskNoCopy(util.Arrays.copyOf(copy, idx+1), count)
				} else
					fromBitMaskNoCopy(copy, count)
			}
		}


		override def +(elem: Int): ContinuousBitSet = {
			val idx = elem >>> 6
			if (idx < words.length) {
				if ((words(idx) >> elem & 1L) == 1L) this
				else {
					val cpy = new Array[Long](words.length)
					System.arraycopy(words, 0, cpy, 0, words.length)
					cpy(idx) |= 1L << elem
					fromBitMaskNoCopy(cpy, if (bitCount>=0) bitCount+1 else -1)
				}
			} else {
				val cpy = new Array[Long](idx+1)
				System.arraycopy(words, 0, cpy, 0, words.length)
				cpy(idx) |= 1L << elem
				fromBitMaskNoCopy(words, if (bitCount>=0) bitCount+1 else -1)
			}
		}

		override def -(elem: Int): ContinuousBitSet = {
			var idx = elem >>> 6
			if (idx < words.length) {
				var w = words(idx)
				if ((w >> elem & 1L) == 0) this
				else {
					w ^= 1L << elem
					val copy =
						if (idx==words.length-1 && w==0) {
							if (bitCount == 1) return empty
							else if (bitCount > 1) {
								do {idx -= 1} while (words(idx) == 0L)
							} else {
								do {idx -= 1} while (idx >= 0 && words(idx) == 0L)
							}
							util.Arrays.copyOf(words, idx+1)
						} else {
							val c = util.Arrays.copyOf(words, words.length)
							c(idx) = w
							c
						}
					fromBitMaskNoCopy(copy, if (bitCount>0) bitCount-1 else -1)
				}
			} else this
		}

		override def toIterator: FitIterator[Int] = new BitSetIterator(words)

		override def keysIteratorFrom(start: Int) :FitIterator[Int] with AbstractIterator[Int] =
			new BitSetIterator(words, start)

//		override def newBuilder
	}


	private class MutableContinuousBitSet(bmap :Array[Long], precalculatedSize :Int = -1)
		extends ContinuousBitSet(bmap, precalculatedSize) with BitSetLike[MutableContinuousBitSet]
				with MutableSet.Ordered[Int] with mutable.SetLike[Int, MutableContinuousBitSet]
				with SetSpecialization[Int, MutableContinuousBitSet]
	{
		override def count = size

		override def empty = new MutableContinuousBitSet(Array.emptyLongArray, 0)

		override protected def fromBitMaskNoCopy(elems: Array[Long], size :Int) =
			new MutableContinuousBitSet(elems, size)

		override def add(elem: Int): Boolean = {
			val words = bitmap; val length = words.length
			val idx = elem >>> 6
			if (idx < length) {
				((words(idx) >> elem & 1L) == 0L) && {
					words(idx) |= 1L << elem
					size_++()
					true
				}
			} else {
				var capacity = length
				if (idx < 0x01ffffff) //any larger and we are over Int.MaxValue
					do { capacity <<= 1 } while (capacity < idx)
				else
					capacity = 0x01ffffff
				val cpy = new Array[Long](capacity)
				System.arraycopy(words, 0, cpy, 0, length)
				cpy(idx) |= 1L << elem
				bitmap = cpy
				size_++()
				true
			}
		}

		override def +=(elem: Int): this.type = { add(elem); this }


		override def remove(elem: Int): Boolean =  {
			val words = bitmap; val length = bitmap.length
			val idx = elem >>> 6
			(idx < length) && {
				var w = words(idx)
				((w >> elem) & 1L) == 1L && {
					w ^= 1L << elem
					words(idx) = w
					size_--()
					true
				}
			}
		}

		override def -=(elem: Int): this.type = { remove(elem); this }


	}
*/



	private[sets] class BitSetIterator(words :Array[Long], start :Int=0) extends BaseIterator[Int] with FitIterator[Int] {
		override def hasDefiniteSize = true
		override def hasFastSize = false
		private[this] var n=start
		private[this] val max = words.length << 6
		if (n < max && ((words(n >>> 6) >> n) & 1L)==0) skip()

		override def head = n
		override def hasNext = n < max
		override def next() = { val res = n; skip(); res }

		override def skip() :Unit = {
			var idx = n >>> 6
			var w = words(idx) >>> n
			do {
				while(w != 0L) {
					if ((w & 1L)==1L)
						return
					w >>= 1
					n += 1
				}
				idx += 1; n = idx << 6
				if (n>=max)
					return
				w = words(idx)
			} while(true)
		}

	}
}
*/