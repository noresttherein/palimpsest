package net.noresttherein.palimpsest.sets

import java.io.{ObjectInputStream, ObjectOutputStream}

import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.ItemTypes
import net.noresttherein.palimpsest.sets.MutableHashSet.{FillFactorDenominator, MaxFillFactor, MinCapacity, MinFillFactor, MinSlots, Neighbourhood, Bitmap, FullBitmap, SerializedHashSet}
import net.noresttherein.palimpsest.{AptBuilder, RuntimeType}
import net.noresttherein.palimpsest.iterables.{AptCompanion, SpecializableIterableFactory}
import net.noresttherein.palimpsest.iterables.AptCompanion.CanFitFrom
import net.noresttherein.palimpsest.iterables.AptIterable.{IterableSerializer}

import scala.collection.generic.CanBuildFrom
import java.lang.Integer.{numberOfTrailingZeros, reverseBytes}

import net.noresttherein.palimpsest.Vals.OfKnownSize

/** A specialized hash set backed by an array using open addressing. The collisions are resolved using the hopscotch
  * algorithm with a neighbourhood size of 16, leading to good locality and overall pessimistic performance even
  * with high fill factors. This class is not thread safe.
  * @author Marcin Mościcki marcin@moscicki.net
  */
class MutableHashSet[@specialized(LargeSetElements) E] private[sets] (
		/** The array of length n + 15 where n is the number of buckets in this hash table. */
		private[this] var slots :Array[E],
		/** The array with bitmaps marking which slots are used by elements hashed to the given bucket.
		  * The n-th bucket can store elements at slots &lt;n,n+15&gt;. An element's bucket is calculated modulo
		  * the length of this array rather than `slots`.
		  */
		private[this] var buckets :Array[Bitmap],
		/** The current number of elements in this set. */
		private[this] var keys :Int)
	extends MutableSet[E] with MutableSetSpecialization[E, MutableHashSet[E]]
	   with SpecializableSet[E, MutableHashSet] with OfKnownSize with Serializable
{

//	def this() = this(RuntimeType.arrayOf[E](MinSlots), new Array[Bitmap](MinCapacity), 0)



	override def size :Int = keys

	@inline
	private[this] final def hash(item :E) :Int = {
		var hc = item.## * 0x9e3775cd
		hc = reverseBytes(hc)
		hc * 0x9e3775cd
	}

	private[this] final def rehash(capacity :Int = buckets.length * 2) :Unit = {
		val i = new HopscotchHashSetIterator[E](slots, buckets, keys)
		slots = RuntimeType.arrayOf[E](capacity + Neighbourhood - 1)(specialization)
		buckets = new Array[Bitmap](capacity)
		keys = 0
		while (i.hasNext)
			this add i.next()
	}



	override def sizeHint(expect :Int) :Unit =
		if (expect > keys) {
			val capacity = java.lang.Integer.highestOneBit((MaxFillFactor * expect / FillFactorDenominator).toInt)
			if (capacity > buckets.length)
				rehash(capacity)
		}


	override def contains(elem :E) :Boolean = {
		val capacity = buckets.length
		//caution: capacity must be a power of two: (then capacity - 1) masks log capacity - 1 lower bits.
		val bucket = hash(elem) & (capacity - 1)
		var hits = buckets(bucket) & FullBitmap
		while (hits != 0) {
			if (slots(bucket + numberOfTrailingZeros(hits)) == elem)
				return true //caution: early method return: we found the element
			hits &= ~(hits & -hits) //clear the lowest bit
		}
		false
	}


	override def -=(elem :E) :this.type = { remove(elem); this }

	override def remove(elem :E) :Boolean = {
		val capacity = buckets.length
		//caution: capacity must be a power of two: (then capacity - 1) masks log capacity - 1 lower bits.
		val bucket = hash(elem) & (capacity - 1)
		val collisions = buckets(bucket) & FullBitmap
		var hits = collisions
		while (hits != 0) {
			if (slots(bucket + numberOfTrailingZeros(hits)) == elem) {
				keys -= 1
				buckets(bucket) = Bitmap(collisions & ~(collisions & -collisions))
				if (keys * FillFactorDenominator <= MinFillFactor * capacity && capacity > MinCapacity)
					rehash(capacity >> 1)
				return true //caution: early method return: we found the element
			}
			hits &= ~(hits & -hits) //clear the lowest bit
		}
		false
	}


	override def +=(elem :E) :this.type = { add(elem); this }


	override def add(elem :E) :Boolean = {
		val capacity = buckets.length
		//caution: we assume here capacity is a power of two and mask log capacity - 1 lower bits instead of costly modulo
		val bucket = hash(elem) & (capacity - 1)
		var slot = bucket
		val collisions = buckets(slot) & FullBitmap
		if (collisions == FullBitmap) { //the bucket is full
			rehash(capacity << 1)
			return this add elem //caution: early method return - the bucket is full
		}

		var hits = collisions
		while (hits != 0) {
			if (slots(bucket + numberOfTrailingZeros(hits)) == elem)
				return false //caution: early method return - elem already present
			hits &= ~(hits & -hits)
		}
		if ((keys + 1) * FillFactorDenominator >= MaxFillFactor * capacity) {
			rehash(capacity << 1)
			return this add elem //caution: early method return - needed to rehash based on the fill factor
		}

		slot = bucket - Neighbourhood + 1
		if (slot < 0)
			slot = 0
		//to find a following free slot we need first to go back and compose the occupation mask
		hits = buckets(slot) & FullBitmap
		while (slot < bucket) {
			slot += 1
			hits >>>= 1
			hits |= buckets(slot) & FullBitmap
		}
		while ((hits & 1) != 0) { //find a free slot at bucket or higher
			slot += 1
			hits >>>= 1
			if (slot < capacity) //this check could be avoided by splitting the loop into two
				hits |= buckets(slot) & FullBitmap
		}
		if (slot >= capacity + Neighbourhood - 1) {
			rehash(capacity << 1)
			return this add elem
		}

		var free = slot //the first free slot above the bucket
		while (free - bucket >= Neighbourhood) { //move elements around until the free slot is in the hash bucket neighbourhood
			slot = free - Neighbourhood + 1
			//find the earliest element belonging to one of the buckets <slot, free) to displace to free
			var improveMask = FullBitmap //mask for the following offsets which are better than what we found already
			var displacedBucket = -1 //index of the bucket with the first element which can be displaced
			var displacedBit = 0 //mask for the lowest bit (i.e offset of the earliest element) from displacedBucket
			while (slot < free && improveMask != 0) {
				hits = buckets(slot) & improveMask
				if  (hits != 0) {
					displacedBucket = slot
					displacedBit = hits & -hits //lowest set bit in hits
					improveMask = displacedBit - 1 //mask for all bits lower than the displacedBit
				}
				improveMask >>>= 1
				slot += 1
			}
			if (displacedBucket < 0) { //no element in <free-7, free) can be moved to free
				rehash(capacity << 1)
				return this add elem //caution: early method return
			}
			slot = displacedBucket + numberOfTrailingZeros(displacedBit)
			slots(free) = slots(slot)
			free = slot
		}

		//finally, store the elem in the free slot and update the usage mask
		slots(free) = elem
		keys += 1
		buckets(bucket) = Bitmap(collisions | (1 << (free - bucket)))
		true
	}


	override def flip(elem :E) :Boolean =
		if (contains(elem)) !remove(elem)
		else !add(elem)


	override def clear() :Unit = {
		keys = 0
		val capacity = buckets.length
		if (capacity == MinCapacity) {
			var i = 0
			while (i < capacity) {
				buckets(i) = 0
				i += 1
			}
		} else {
			slots = RuntimeType.arrayOf[E](MinSlots)(specialization)
			buckets = new Array[Bitmap](MinCapacity)
		}
	}



	override def foreach[@specialized(Unit) U](f :E=>U) :Unit = {
		var remaining = keys
		var slot = 0
		var mask = 0
		while (remaining > 0) {
			mask >>>= 1
			mask |= buckets(slot) & FullBitmap
			if ((mask & 1) != 0) {
				f(slots(slot))
				remaining -= 1
			}
			slot += 1
		}
	}

	override protected def reverseForeach(f :E => Unit) :Unit = {
		var remaining = keys
		var slot = buckets.length - 1
		var mask = 0
		while (remaining > 0) {
			mask <<= 1
			mask |= buckets(slot) & FullBitmap
			if ((mask & 0x80) != 0) {
				f(slots(slot))
				remaining -= 1
			}
			slot -= 1
		}
	}

	override def iterator :AptIterator[E] =
		if (keys == 0) AptIterator.Empty
		else new HopscotchHashSetIterator[E](slots, buckets, keys)

	override def empty :MutableHashSet[E] =
		new MutableHashSet[E](RuntimeType.arrayOf[E](MinSlots)(specialization), new Array[Bitmap](MinCapacity), 0)

	override def companion :AptCompanion[MutableHashSet] = MutableHashSet


	private def writeReplace :AnyRef = new SerializedHashSet(this)

	private def readObject(is :ObjectInputStream) :Nothing =
		throw new UnsupportedOperationException("MutableHashSet can't be deserialized directly.")

}






object MutableHashSet extends SpecializableIterableFactory[MutableHashSet] {


	override def empty[@specialized(ItemTypes) E] :MutableHashSet[E] = //new MutableHashSet[E]
		new MutableHashSet[E](RuntimeType.arrayOf[E](MinSlots), new Array[Bitmap](MinCapacity), 0)

	/** Creates an empty mutable hash set preallocating memory required to store `sizeHint` elements.
	  * The set will function as normal and can grow beyond that size; this is simply to avoid multiple resizing
	  * before reaching that limit.
	  */
	def empty[@specialized(ItemTypes) E](sizeHint :Int) :MutableHashSet[E] = {
		//set the capacity to the largest power of two less than or equal to theoretical maximal capacity.
		//this is because hash set requires the capacity to be a power of two; as MaxFillFactor > 2*MinFillFactor,
		//this won't drop the capacity below the minimal fill factor.
		var capacity = java.lang.Integer.highestOneBit((MaxFillFactor * sizeHint / FillFactorDenominator).toInt)
		if (capacity < MinCapacity)
			capacity = MinCapacity
		new MutableHashSet[E](RuntimeType.arrayOf[E](capacity + Neighbourhood - 1), new Array[Bitmap](capacity), 0)
	}

	@inline override def newBuilder[@specialized(ItemTypes) E] :AptBuilder[E, MutableHashSet[E]] = empty[E]


	override implicit def canBuildFrom[E](implicit fit :CanFitFrom[MutableHashSet[_], E, MutableHashSet[E]])
			:CanBuildFrom[MutableHashSet[_], E, MutableHashSet[E]] = fit.cbf


	/** `MinFillFactor / FillFactorDenominator` defines the minimal percentage of full buckets in a hash set. */
	private final val MinFillFactor = 33L
	/** `MaxFillFactor / FillFactorDenominator` defines the maximal percentage of full buckets in a hash set.
	  * It must be strictly larger than `MinFillFactor * 2` and the ratio must be strictly less than `1.0`. */
	private final val MaxFillFactor = 80L
	private final val FillFactorDenominator = 100L
	/** Minimal number of buckets in a hash set. */
	private final val MinCapacity = 8
	/** Size of the bucket neighbourhood, i.e. the number of buckets following the preferred bucket which can contain
	  * an element with a given hash code. */
	private final val Neighbourhood = 16
	/** Minimal number of slots in a hash set, including the neighbourhood for the last bucket. */
	private final val MinSlots = MinCapacity + Neighbourhood - 1
	
	private[sets] type Bitmap = Short

	@inline private[sets] final val FullBitmap = 0xffff

	@inline private[sets] def Bitmap(word :Int) :Bitmap = word.toShort
	
	//todo: Mutable Set should serialize itself.
	private[MutableHashSet] class SerializedHashSet[@specialized(LargeSetElements) E]
	                                               (@transient protected[this] override var self :MutableHashSet[E])
		extends IterableSerializer[E, MutableHashSet[E]]
	{
		protected[this] override def builder :AptBuilder[E, MutableHashSet[E]] = empty[E]
	}
}






private[sets] class HopscotchHashSetIterator[@specialized(LargeSetElements) E](
		slots :Array[E], usage :Array[Bitmap], private[this] var remaining :Int
	) extends AptIterator[E]
{
	private[this] var hd :E = _  //head element
	private[this] var slot = -1  //bucket index of the head element
	private[this] var mask = 0   //mask with '1' bits marking which of the following slots are full
	next()

	override def size :Int = remaining

	override def head :E = hd

	override def hasNext :Boolean = remaining > 0

	override def next() :E = {
		val res = hd
		do {
			slot += 1
			mask >>>= 1
			mask |= usage(slot) & FullBitmap
		} while ((mask & 1) == 0)
		hd = slots(slot)
		remaining -= 1
		res
	}

	override def skip() :Unit = next()

}
