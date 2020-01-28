package net.turambar.palimpsest.specialty.sets

import java.io.{ObjectInputStream, ObjectOutputStream}

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.ItemTypes
import net.turambar.palimpsest.specialty.sets.MutableHashSet.{FillFactorDenominator, MaxFillFactor, MinCapacity, MinFillFactor, MinSlots, SerializedHashSet}
import net.turambar.palimpsest.specialty.{FitBuilder, RuntimeType}
import net.turambar.palimpsest.specialty.iterables.{FitCompanion, SpecializableIterable, SpecializableIterableFactory}
import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.FitIterable.{ElementDeserializer, ElementSerializer, IterableSerializer}

import scala.collection.generic.CanBuildFrom
import java.lang.Integer.{numberOfTrailingZeros, reverseBytes}

import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize

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
		private[this] var buckets :Array[Short],
		/** The current number of elements in this set. */
		private[this] var elems :Int)
	extends MutableSet[E] with MutableSetSpecialization[E, MutableHashSet[E]]
	   with SpecializableSet[E, MutableHashSet] with OfKnownSize with Serializable
{

	override def size :Int = elems

	@inline
	private[this] final def hash(item :E) :Int = {
		var hc = item.## * 0x9e3775cd
		hc = reverseBytes(hc)
		hc * 0x9e3775cd
	}

	private[this] final def rehash(capacity :Int = buckets.length * 2) :Unit = {
		val i = new HopscotchHashSetIterator[E](slots, buckets, elems)
		slots = RuntimeType.arrayOf[E](capacity + 15)(specialization)
		buckets = new Array[Short](capacity)
		elems = 0
		while (i.hasNext)
			this add i.next()
	}



	override def contains(elem :E) :Boolean = {
		val capacity = buckets.length
		//caution: capacity must be a power of two: (then capacity - 1) masks log capacity - 1 lower bits.
		val bucket = hash(elem) & (capacity - 1)
		var hits = buckets(bucket) & 0xffff
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
		val collisions = buckets(bucket) & 0xffff
		var hits = collisions
		while (hits != 0) {
			if (slots(bucket + numberOfTrailingZeros(hits)) == elem) {
				elems -= 1
				buckets(bucket) = (collisions & ~(collisions & -collisions)).toShort
				if (elems * FillFactorDenominator <= MinFillFactor * capacity && capacity > MinCapacity)
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
		val collisions = buckets(slot) & 0xffff
		if (collisions == 0xffff) { //the bucket is full
			rehash(capacity << 1)
			return this add elem //caution: early method return - the bucket is full
		}

		var hits = collisions
		while (hits != 0) {
			if (slots(bucket + numberOfTrailingZeros(hits)) == elem)
				return false //caution: early method return - elem already present
			hits &= ~(hits & -hits)
		}
		if ((elems + 1) * FillFactorDenominator >= MaxFillFactor * capacity) {
			rehash(capacity << 1)
			return this add elem //caution: early method return - needed to rehash based on the fill factor
		}

		slot = bucket - 15
		if (slot < 0)
			slot = 0
		//to find a following free slot we need first to go back and compose the occupation mask
		hits = buckets(slot) & 0xffff
		while (slot < bucket) {
			slot += 1
			hits >>>= 1
			hits |= buckets(slot) & 0xffff
		}
		while ((hits & 1) != 0) { //find a free slot at bucket or higher
			slot += 1
			hits >>>= 1
			if (slot < capacity)
				hits |= buckets(slot) & 0xffff
		}
		if (slot >= capacity + 15) {
			rehash(capacity << 1)
			return this add elem
		}

		var free = slot //the first free slot above the bucket
		while (free - bucket >= 16) { //move elements around until the free slot is in the hash bucket neighbourhood
			slot = free - 15
			//find the earliest element belonging to one of the buckets <slot, free) to displace to free
			var improveMask = 0xffff //mask for the following offsets which are better than what we found already
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
		elems += 1
		buckets(bucket) = (collisions | (1 << (free - bucket))).toShort
		true
	}


	override def flip(elem :E) :Boolean =
		if (contains(elem)) !remove(elem)
		else !add(elem)


	override def clear() :Unit = {
		elems = 0
		val capacity = buckets.length
		if (capacity == MinCapacity) {
			var i = 0
			while (i < capacity) {
				buckets(i) = 0
				i += 1
			}
		} else {
			slots = RuntimeType.arrayOf[E](MinSlots)(specialization)
			buckets = new Array[Short](MinCapacity)
		}
	}



	override def foreach[@specialized(Unit) U](f :E=>U) :Unit = {
		var remaining = elems
		var slot = 0
		var mask = 0
		while (remaining > 0) {
			mask >>>= 1
			mask |= buckets(slot) & 0xffff
			if ((mask & 1) != 0) {
				f(slots(slot))
				remaining -= 1
			}
			slot += 1
		}
	}

	override protected def reverseForeach(f :E => Unit) :Unit = {
		var remaining = elems
		var slot = buckets.length - 1
		var mask = 0
		while (remaining > 0) {
			mask <<= 1
			mask |= buckets(slot) & 0xffff
			if ((mask & 0x80) != 0) {
				f(slots(slot))
				remaining -= 1
			}
			slot -= 1
		}
	}

	override def iterator :FitIterator[E] =
		if (elems == 0) FitIterator.Empty
		else new HopscotchHashSetIterator[E](slots, buckets, elems)

	override def empty :MutableHashSet[E] =
		new MutableHashSet[E](RuntimeType.arrayOf[E](MinSlots)(specialization), new Array[Short](MinCapacity), 0)

	override def companion :FitCompanion[MutableHashSet] = MutableHashSet


	private def writeReplace :AnyRef = new SerializedHashSet(this)

	private def readObject(is :ObjectInputStream) :Nothing =
		throw new UnsupportedOperationException("MutableHashSet can't be deserialized directly.")

}






object MutableHashSet extends SpecializableIterableFactory[MutableHashSet] {


	override def empty[@specialized(ItemTypes) E] :MutableHashSet[E] =
		new MutableHashSet[E](RuntimeType.arrayOf[E](MinSlots), new Array[Short](MinCapacity), 0)


	override def newBuilder[@specialized(ItemTypes) E] :FitBuilder[E, MutableHashSet[E]] = empty[E]


	override implicit def canBuildFrom[E](implicit fit :CanFitFrom[MutableHashSet[_], E, MutableHashSet[E]])
			:CanBuildFrom[MutableHashSet[_], E, MutableHashSet[E]] = fit.cbf


	private final val MinFillFactor = 33L
	private final val MaxFillFactor = 80L
	private final val FillFactorDenominator = 100L
	private final val MinCapacity = 8
	private final val Neighbourhood = 16
	private final val MinSlots = MinCapacity + Neighbourhood - 1

	//todo: Mutable Set should serialize itself.
	private[MutableHashSet] class SerializedHashSet[@specialized(LargeSetElements) E]
	                                               (@transient protected[this] override var self :MutableHashSet[E])
		extends IterableSerializer[E, MutableHashSet[E]]
	{
		protected[this] override def builder :FitBuilder[E, MutableHashSet[E]] = empty[E]
	}
}






private[sets] class HopscotchHashSetIterator[@specialized(LargeSetElements) E](
		slots :Array[E], usage :Array[Short], private[this] var remaining :Int
	) extends FitIterator[E]
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
			mask |= usage(slot) & 0xffff
		} while ((mask & 1) == 0)
		hd = slots(slot)
		remaining -= 1
		res
	}

	override def skip() :Unit = next()

}
