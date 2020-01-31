package net.turambar.palimpsest.specialty.ordered

import scala.annotation.unspecialized

import net.turambar.palimpsest.specialty.sets.{OrderedSet, ValSet}
import net.turambar.palimpsest.specialty.{?, some_?, Blank, Sure}
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.MultiValue
import net.turambar.palimpsest.specialty.iterators.FitIterator
import scala.collection.generic.Sorted


trait OrderedBy[+T <: OrderedBy[T, K], @specialized(MultiValue) K] extends Sorted[K, T] {
	implicit def ordering :ValOrdering[K]

	override def compare(e1 :K, e2 :K) :Int = ordering.compare(e1, e2)
	protected def lte(x :K, y :K) :Boolean = compare(x, y) <= 0
	protected def lt(x :K, y :K) :Boolean = compare(x, y) < 0
	protected def equiv(x :K, y :K) :Boolean = compare(x, y) == 0


	override def firstKey :K
	override def lastKey :K

	/** N-th key in this collection. */
	def keyAt(n :Int) :K = {
		val it = keysIterator.drop(n)
		if (it.hasNext) it.next()
		else throw new IndexOutOfBoundsException(n)
	}

	def contains(key :K) :Boolean

	override def from(from: K): T = rangeImpl(Sure(from), Blank)

	override def until(until: K): T = rangeImpl(Blank, Sure(until))

	override def range(from: K, until: K): T = rangeImpl(Sure(from), Sure(until))

	override def rangeImpl(from :Option[K], until :Option[K]) :T = rangeImpl(some_?(from), some_?(until))

	def rangeImpl(from: ?[K], until: ?[K]): T

	override def to(to: K): T = {
		val rest = keysIteratorFrom(to)
		while (rest.hasNext) {
			val next = rest.next()
			if (compare(next, to) > 0)
				return until(next)
		}
		repr
	}

//	def reverseKeyIterator :FitIterator[K]
	override def keysIteratorFrom(start :K) :FitIterator[K]

	def keysIterator :FitIterator[K]

	def empty :T

	override def keySet :OrderedSet[K]

//	def stringPrefix :String
}






object OrderedBy {

	trait OrderedEmpty[@specialized(MultiValue) K, +T <: T OrderedBy K] extends OrderedBy[T, K] {

		override def empty :T = this.asInstanceOf[T]

		override def firstKey: K = throw new NoSuchElementException(s"empty $this.firstKey")

		override def lastKey: K = throw new NoSuchElementException(s"empty $this.lastKey")

		override def contains(key :K) :Boolean = false

		override def keyAt(n: Int): K = throw new IndexOutOfBoundsException(s"empty $this.keyAt($n)")

//		override def reverseKeyIterator: FitIterator[K] = FitIterator.empty
		override def keysIteratorFrom(start: K): FitIterator[K] = FitIterator.empty

		override def from(from: K) :T = this.asInstanceOf[T]
		override def until(until: K) :T = this.asInstanceOf[T]
		override def range(from: K, until: K) :T = this.asInstanceOf[T]
		override def to(to: K) :T = this.asInstanceOf[T]
		override def rangeImpl(from: ?[K], until: ?[K]): T = this.asInstanceOf[T]

		override def keySet: OrderedSet[K] = OrderedSet.empty
	}



	trait OrderedSingleton[@specialized(MultiValue) K, +T <: T OrderedBy K] extends OrderedBy[T, K] {
//		protected def key :K
//		def empty :T

//		override def firstKey :K = key
		override def lastKey :K = firstKey

		override def keyAt(n :Int) :K = if (n==0) firstKey else throw new IndexOutOfBoundsException(s"$this.keyAt($n)")

		override def contains(key :K) :Boolean = key == firstKey


		override def from(from: K): T = if (lte(from, firstKey)) this.asInstanceOf[T] else empty
		override def until(until: K): T = if (lt(firstKey, until)) this.asInstanceOf[T] else empty
		override def to(to :K) :T = if (lte(firstKey, to)) this.asInstanceOf[T] else empty

		override def range(from: K, until: K): T = {
			val k = firstKey
			if (lte(from, k) && lt(k, until)) this.asInstanceOf[T]
			else empty
		}

		override def rangeImpl(from: ?[K], until: ?[K]): T = {
			val k = firstKey
			if (from.isDefined && lt(k, from.get) || until.isDefined && lte(until.get, k)) empty
			else this.asInstanceOf[T]

		}

//		override def reverseKeyIterator: FitIterator[K] = FitIterator(key)

		override def keysIteratorFrom(key :K) :FitIterator[K] =
			if (lte(key, firstKey)) FitIterator.one(key)
			else FitIterator.empty

		override def keySet :OrderedSet[K] = OrderedSet.one(firstKey)
	}



	trait OrderedProxy[+T <: OrderedBy[T, K], @specialized(MultiValue) K] extends OrderedBy[T, K] {
		protected[this] def source :OrderedBy[_, K]

		@unspecialized
		override implicit def ordering :ValOrdering[K] = source.ordering

		override def firstKey :K = source.firstKey

		override def lastKey :K = source.lastKey

		override def keyAt(n :Int) :K = source.keyAt(n)

//		override def contains(key :K) :Boolean = source.contains(key)

		override def keysIteratorFrom(start :K) :FitIterator[K] = source.keysIteratorFrom(start)

		@unspecialized
		override def keySet :OrderedSet[K] = source.keySet
	}
}
