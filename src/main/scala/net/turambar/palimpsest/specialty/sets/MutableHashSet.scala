package net.turambar.palimpsest.specialty.sets

import java.io.{ObjectInputStream, ObjectOutputStream}

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.Elements
import net.turambar.palimpsest.specialty.sets.MutableHashSet.{MinFillFactor, SerializedHashSet}
import net.turambar.palimpsest.specialty.{FitBuilder, RuntimeType}
import net.turambar.palimpsest.specialty.iterables.{FitCompanion, SpecializableIterable, SpecializableIterableFactory}
import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.FitIterable.{ElementDeserializer, ElementSerializer}

import scala.collection.generic.CanBuildFrom

/** A specialized hash set backed by an array using open addressing. The collisions are resolved using the hopscotch
  * algorithm leading to good locality and overall pessimistic performance even with high fill factors.
  * This class is not thread safe.
  * @author Marcin Mościcki marcin@moscicki.net
  */
class MutableHashSet[@specialized(LargeSetElements) E] private[sets] (
		/** The array of length n * 15 where n is the number of buckets in this hash table. */
		private[this] var slots :Array[E],
		/** The array with bitmaps marking which slots are used by elements hashed to the given bucket.
		  * The n-th bucket can store elements at slots &lt;n,n+15&gt;. An element's bucket is calculated modulo
		  * the length of this array rather than `slots`.
		  */
		private[this] var buckets :Array[Short],
		/** The current number of elements in this set. */
		private[this] var elems :Int)
	extends MutableSet[E] with MutableSetSpecialization[E, MutableHashSet[E]]
	   with SpecializableSet[E, MutableHashSet] with Serializable
{


	private[this] final def rehash(capacity :Int = buckets.length * 2) :Unit = {
		val i = new HopscotchHashSetIterator[E](slots, buckets, elems)
		slots = RuntimeType.arrayFor[E](capacity + 15)(specialization)
		buckets = new Array[Short](capacity)
		elems = 0
		while (i.hasNext)
			this += i.next()
	}



	override def contains(elem :E) :Boolean = {
		val capacity = buckets.length
		var slot = elem.hashCode % capacity
		var hits = buckets(slot) & 0xffff
		while (hits != 0) {
			if ((hits & 1) != 0 && slots(slot) == elem)
				return true //caution: early method return: we found the element
			hits >>= 1
			slot += 1
		}
		false
	}


	override def -=(elem :E) :this.type = { remove(elem); this }

	override def remove(elem :E) :Boolean = {
		val capacity = buckets.length
		val bucket = elem.hashCode % capacity
		val collisions = buckets(bucket) & 0xffff
		var mask = collisions
		var slot = bucket
		while (mask != 0) {
			if ((mask & 1) != 0 && slots(slot) == elem) {
				elems -= 1
				buckets(bucket) = (collisions & ~(1 << (slot - bucket))).toShort
				if (elems / capacity <= MinFillFactor && capacity > 8)
					rehash(capacity / 2)
				return true //caution: early method return - we found the element
			}
			mask >>= 1
			slot += 1
		}
		false
	}


	override def +=(elem :E) :this.type = { add(elem); this }


	override def add(elem :E) :Boolean = {
		val capacity = buckets.length
		val bucket = elem.hashCode % capacity
		var slot = bucket
		val collisions = buckets(slot) & 0xffff
		var hits = collisions
		while (hits != 0) {
			if ((hits & 1) != 0 && slots(slot) == elem)
				return false //caution: early method return - elem already present
			hits >>= 1
			slot += 1
		}
		if (collisions == 0xffff) { //the bucket is full
			rehash(capacity * 2)
			return this add elem //caution: early method return - the bucket is full
		}
		slot = bucket - 15
		if (slot < 0)
			slot = 0
		//find the first free slot at bucket or higher; we need first to go back and compose the occupation mask
		hits = buckets(slot) & 0xffff
		while (slot < bucket) {
			slot += 1
			hits >>= 1
			hits |= buckets(slot) & 0xffff
		}
		while ((hits & 1) != 0) { //find the free slot
			slot += 1
			hits >>= 1
			if (slot < capacity)
				hits |= buckets(slot) & 0xffff
		}
		if (slot >= capacity + 15) {
			rehash(capacity * 2)
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
				improveMask >>= 1
				slot += 1
			}
			if (displacedBucket < 0) { //no element in <free-7, free) can be moved to free
				rehash(capacity * 2)
				return this add elem //caution: early method return
			}
			slot = displacedBucket + java.lang.Integer.numberOfTrailingZeros(displacedBit)
			slots(free) = slots(slot)
			free = slot
		}

		//finally, store the elem in the free slot and update the usage mask
		slots(free) = elem
		elems += 1
		buckets(bucket) = (buckets(bucket) | (1 << (free - bucket))).toShort
		true
	}


	override def flip(elem :E) :Boolean =
		if (contains(elem)) !remove(elem)
		else !add(elem)


	override def foreach[@specialized(Unit) U](f :E=>U) :Unit = {
		var remaining = elems
		var slot = 0
		var mask = 0
		while (remaining > 0) {
			mask >>= 1
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
		new MutableHashSet[E](RuntimeType.arrayFor[E](24)(specialization), new Array[Short](8), 0)

	override def companion :FitCompanion[MutableHashSet] = MutableHashSet


	private def writeReplace :AnyRef = new SerializedHashSet(this)

	private def readObject(is :ObjectInputStream) :Nothing =
		throw new UnsupportedOperationException("MutableHashSet can't be deserialized directly.")

}






object MutableHashSet extends SpecializableIterableFactory[MutableHashSet] {


	override def empty[@specialized(Elements) E] :MutableHashSet[E] =
		new MutableHashSet[E](RuntimeType.arrayFor[E](24), new Array[Short](16), 0)


	override def newBuilder[@specialized(Elements) E] :FitBuilder[E, MutableHashSet[E]] = empty[E]


	override implicit def canBuildFrom[E](implicit fit :CanFitFrom[MutableHashSet[_], E, MutableHashSet[E]])
			:CanBuildFrom[MutableHashSet[_], E, MutableHashSet[E]] = fit.cbf


	private final val MinFillFactor = 0.33
	private final val Neighbourhood = 16


	private[MutableHashSet] class SerializedHashSet[@specialized(LargeSetElements) E](@transient private var set :MutableHashSet[E])
		extends Serializable
	{
		private def writeObject(os :ObjectOutputStream) :Unit = writeSet(os, set)

		private def writeSet(os :ObjectOutputStream, set :MutableHashSet[E]) :Unit = {
			os.defaultWriteObject()
			val writer = ElementSerializer[E]()
			os writeInt set.size
			val it = set.iterator
			while (it.hasNext)
				writer(os, it.next())
		}

		private def readObject(is :ObjectInputStream) :Unit = set = readSet(is)

		private def readSet(is :ObjectInputStream) :MutableHashSet[E] = {
			is.defaultReadObject()
			val reader = ElementDeserializer[E]()
			var size = is.readInt
			val res = MutableHashSet.empty[E]
			while (size > 0) {
				res add reader(is)
				size -= 1
			}
			res
		}

		private def readResolve :AnyRef = set
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
			mask >>= 1
			mask |= usage(slot) & 0xffff
		} while ((mask & 1) == 0)
		hd = slots(slot)
		remaining -= 1
		res
	}

	override def skip() :Unit = next()

}
