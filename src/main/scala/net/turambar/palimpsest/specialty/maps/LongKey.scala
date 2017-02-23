package net.turambar.palimpsest.specialty.maps

import java.lang.Long.highestOneBit

import net.turambar.palimpsest.specialty.FitIterable.{IterableFoundation, IterableMapping}
import net.turambar.palimpsest.specialty.FitIterator.{BaseIterator, MappedIterator}
import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator, Specialized}
import net.turambar.palimpsest.specialty.Specialized.{Fun1Res, Fun2}
import net.turambar.palimpsest.specialty.Elements
import net.turambar.palimpsest.specialty.iterables.{EmptyIterable, SingletonFoundation, SingletonSpecialization}
import net.turambar.palimpsest.specialty.sets.FitSet.ImmutableSetBuilder
//import net.turambar.palimpsest.specialty.maps.LongKey.{LongMapIterator, Singleton}
import net.turambar.palimpsest.specialty.sets.{FitSet, MutableSet, SetSpecialization, SortedFitSet}

import scala.annotation.tailrec

/**
  * @author Marcin Mościcki
  */
/*
sealed trait LongKeys[+V] {
	def isEmpty :Boolean
	private[sets] def path :Long

	private[sets] def mask :Long

	def containsKey(key :Long) :Boolean

	def keysIterator :FitIterator[Long]
	def elements :Iterator[LongKey[V]]

	def filterKeys(p :Long=>Boolean, ourTruth :Boolean) :LongKeys[V]

	def +[U>:V](key :LongKey[U]) :LongKeys[U]
	def -(key :Long) :LongKeys[V]
}

sealed trait LongKey[@specialized(Elements) +V] extends LongKeys[V] {
	def key :Long
	def value :V
}


object LongKey {
	trait LongSet extends LongKeys[Nothing] with FitSet[Long] with SetSpecialization[Long, LongSet] {
		override protected[this] def mySpecialization = Specialized.SpecializedLong
		override def hasFastSize = true

		override def empty :LongSet = Empty

		override def filterKeys(p :Long=>Boolean, ourTruth :Boolean) :LongSet

		override def filter(p :Long=>Boolean, ourTruth :Boolean) :LongSet = filterKeys(p, ourTruth)

		override def contains(key :Long) = containsKey(key)

		override def fitIterator :FitIterator[Long] = keysIterator

		override def stringPrefix = "LongSet"
	}

	trait SetKey extends LongKey[Nothing] with LongSet {
		override def value = throw new NoSuchElementException(s"SetKeys($key).value")
	}

//	@inline final def empty :LongKey[Nothing] = Empty

//	def apply(values :Long*) :LongKey = (empty /: values)(_ + _)

//	def newBuilder :FitBuilder[Long, LongKey] = new ImmutableSetBuilder[Long, LongKey](Empty) //new LongMapBuilder

	@inline final private[LongKeys] def apply[V](s1 :LongKey[V], s2 :LongKey[V]) :LongKey[V] =
		if (s1.isEmpty) s2
		else if (s2.isEmpty) s1
		else Branch(s1, s2)


	case object Empty extends EmptyIterable[Long, LongSet] with LongSet {
		override def filterKeys(p: (Long) => Boolean, ourTruth: Boolean) = this

		override def containsKey(elem: Long): Boolean = false
		override def +[U>:Nothing](key :LongKey[U]) :LongKey[U] = key
		override def +(elem :Long) :LongSet = new Singleton(elem)
		override def -(elem: Long): LongSet = this


		override def path = 0L
		override def mask = 0L

		override def toString = "LongMap()"
	}


	final case class Singleton(override val head :Long)
		extends SingletonFoundation[Long, LongSet] with SingletonSpecialization[Long, LongSet] with LongSet
	{
		override def empty :LongSet = LongKey.Empty

		override def path = head
		override def mask = 0xffffffffffffffffL

		override def contains(elem: Long): Boolean = elem==head

		override def +(elem: Long): LongSet =
			if (elem==head) this
			else Branch(this, new Singleton(elem))

		override def -(elem: Long): LongKey =
			if (elem==head) Empty else this

		override def toString = s"LongMap($head)"
	}

	private object Branch {
		private[LongKeys] def apply(s1 :LongKey, s2 :LongKey) :Branch = {
			val suffix1 = s1.path
			val diff = suffix1 ^ s2.path
			val diffBit = diff & -diff
			val sharedSuffix = (diffBit-1) & suffix1
			if ((suffix1 & diffBit) == 0L)
				new Branch(sharedSuffix, diffBit, s1, s2)
			else
				new Branch(sharedSuffix, diffBit, s2, s1)
		}

		private[LongKeys] def prefixJoin(s1 :LongKey, s2 :LongKey) :Branch = {
			val prefix1 = s1.path
			val diff = prefix1 ^ s2.path
			val diffBit = highestOneBit(diff)
			val mask = -diffBit ^ diffBit
			val sharedPrefix = mask & prefix1
			if ((prefix1 & diffBit) == 0L)
				new Branch(sharedPrefix, diffBit, s1, s2)
			else
				new Branch(sharedPrefix, diffBit, s2, s1)
		}
	}



	private case class Branch(path :Long, switchBit :Long, left :LongKey, right :LongKey)
		extends IterableFoundation[Long, LongKey] with LongKey
	{
		override val size = left.size + right.size
		override def isEmpty = false
		@inline final def mask = switchBit - 1 //clears the single `1` bit in the mask and sets all lower bits.

		override def contains(elem: Long): Boolean =
			if ((elem & mask) != path) false
			else if ((elem & switchBit) == 0) left.contains(elem)
			else right.contains(elem)

		override def foreach[@specialized(Unit) U](f: (Long) => U) = {
			left foreach f
			right foreach f
		}

		override def reverseForeach(f: (Long) => Unit): Unit = {
			right reverseTraverse f
			left reverseTraverse f
		}


		override def forall(p: (Long) => Boolean) = left.forall(p) && right.forall(p)

		override def exists(p: (Long) => Boolean) = left.exists(p) || right.exists(p)

		override def find(p: (Long) => Boolean) = left.find(p) orElse right.find(p)

		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, Long) => O) =
			right.foldLeft(left.foldLeft(z)(op))(op)

		override def foldRight[@specialized(Fun2) O](z: O)(op: (Long, O) => O) =
			left.foldRight(right.foldRight(z)(op))(op)

		override protected[this] def dropTake(from: Int, until: Int) :LongKey =
			if (until<=from || from >= size)
				Empty
			else {
				val lsize = left.size
				if (from >= lsize)
					right.slice(from-lsize, until-lsize)
				else if (until <= lsize)
					left.slice(from, until)
				else new Branch(path, switchBit, left.drop(from), right.take(until-lsize))
			}

		override def filter(p: (Long) => Boolean, ourTruth: Boolean): LongKey =
			join(left.filter(p, ourTruth), right.filter(p, ourTruth))
		//			LongMap(left.filter(p, ourTruth), right.filter(p, ourTruth))


		override def +(elem: Long): LongKey =
			if ((elem & mask) != path) LongKey(this, Singleton(elem))
			else if ((elem & switchBit) == 0) new Branch(path, switchBit, left+elem, right)
			else new Branch(path, switchBit, left, right+elem)

		override def -(elem: Long): LongKey =
			if ((elem & mask) != path) this
			else if ((elem & switchBit) == 0) join(left-elem, right) //LongMap(left - elem, right)
			else join(left, right-elem) //LongMap(left, right-elem)


		override def toString = {
			val s = new StringBuilder(stringPrefix) append '('
			left.addString(s, ", ")
			s append ", "
			right.addString(s, ", ")
			s append ')'
			s.result()
		}

		@inline private[LongKeys$] final def join(l :LongKey, r :LongKey) =
			if (l.isEmpty) r
			else if (r.isEmpty) l
			else new Branch(path, switchBit, l, r)

	}



	private[sets] class SortedLongMap private[LongKeys$](protected[this] val source :LongKey)
		extends IterableMapping[Long, LongKey, Long, SortedFitSet[Long]] with SortedFitSet[Long]
	{
		import java.lang.Long.MIN_VALUE
		override implicit def ordering: Ordering[Long] = Ordering.Long

		override protected def forSource[@specialized(Fun1Res) O](f: (Long) => O) = { x :Long => f(x + MIN_VALUE) }
		override protected[this] def fromSource(col: LongKey): SortedLongMap = new SortedLongMap(col)
		override protected[this] def my(x: Long): Long = x + MIN_VALUE
		override protected[this] def from = Sorted.SignedToUnsigned

		override def empty = Sorted.Empty

		@inline final private def mask(set :Branch) :Long = {
			val diffBit = set.switchBit
			-diffBit ^ diffBit
		}

		override def contains(elem: Long): Boolean = {
			@tailrec def descend(node :LongKey=source, value :Long = elem - MIN_VALUE) :Boolean = node match {
				case Empty => false
				case s :Singleton => s.head == value
				case b :Branch =>
					if ((mask(b) & value) != b.path) false
					else if ((b.switchBit & value)==0L) descend(b.left, value)
					else descend(b.right, value)
			}
			descend()
		}



		override def +(elem: Long): SortedFitSet[Long] = { //new SortedLongMap(source + (elem + MIN_VALUE))
		def add(set :LongKey = source, value :Long = elem - MIN_VALUE) :LongKey = set match {
			case Empty => new Singleton(value)
			case s :Singleton if s.head==value => s
			case s :Singleton => Branch.prefixJoin(s, new Singleton(value))
			case b :Branch if (mask(b) & value) != b.path =>
				Branch.prefixJoin(b, new Singleton(value))
			case b :Branch if (b.switchBit & value)==0L =>
				new Branch(b.path, b.switchBit, add(b.left, value), b.right)
			case b :Branch =>
				new Branch(b.path, b.switchBit, b.left, add(b.right, value))
		}
			new SortedLongMap(add())
		}

		override def -(elem: Long): SortedFitSet[Long] = { // new SortedLongMap(source - (elem + MIN_VALUE))
		def del(set :LongKey = source, value :Long = elem - MIN_VALUE) :LongKey = set match {
			case Empty => Empty
			case s :Singleton if s.head == value => Empty
			case s :Singleton => s
			case b :Branch if (mask(b) & value) != b.path => b
			case b :Branch if (b.switchBit & value) == 0L =>
				b.join(del(b.left, value), b.right)
			case b :Branch =>
				b.join(b.left, del(b.right, value))
		}
			new SortedLongMap(del())
		}

		override def fitIterator = new MappedIterator[Long, Long](_ + MIN_VALUE)(source.fitIterator)

		override def keysIteratorFrom(start: Long): FitIterator[Long] = {
			@tailrec def makeStack(node :LongKey = source, value :Long = start - MIN_VALUE, stack :List[LongKey]=Nil) :List[LongKey] =
				node match {
					case Empty => stack
					case s :Singleton if s.head + MIN_VALUE < start => stack
					case s :Singleton => s::stack
					case b :Branch if (mask(b) & value) != b.path => stack
					case b :Branch if (b.switchBit & value) == 0L => makeStack(b.left, value, b::stack)
					case b :Branch => makeStack(b.right, value, stack)
				}
			new LongMapIterator(makeStack()).map((_:Long) + MIN_VALUE)
		}

		override def rangeImpl(from: Option[Long], until: Option[Long]): SortedFitSet[Long] =
			if (until.isDefined) range(from getOrElse MIN_VALUE, until.get)
			else if (from.isDefined) this.from(from.get)
			else this

		override def until(until: Long) = range(MIN_VALUE, until)

		override def from(from: Long) =
			if (from==MIN_VALUE) this
			else {
				def drop(node :LongKey = source, lo :Long = from - MIN_VALUE) :LongKey = node match {
					case Empty => Empty
					case s :Singleton => if (s.head + MIN_VALUE < from) Empty else s
					case b :Branch if (mask(b) & lo) != b.path => Empty
					case b :Branch =>
						if ((b.switchBit & lo)==0L)
							b.join(drop(b.left, lo), b.right)
						else drop(b.right, lo)
				}
				new SortedLongMap(drop())
			}


		override def range(from: Long, until: Long) = {
			def dropTake(node :LongKey=source, lo :Long = from - MIN_VALUE, hi :Long = until - MIN_VALUE) :LongKey = node match {
				case Empty => Empty
				case s :Singleton =>
					if (from > s.head + MIN_VALUE || until <= s.head + MIN_VALUE) Empty
					else s
				case b :Branch =>
					if ((b.switchBit & lo) == 0L) //from in the left subtree
						if ((b.switchBit & hi)==0L) //until also in the left subtree
							dropTake(b.left, lo, hi)
						else //from..until spans both left and right subtree
							b.join(dropTake(b.left, lo, hi), dropTake(b.right, lo, hi))
					else dropTake(b.right, lo, hi)
			}
			if (until<=from) Sorted.Empty
			else new SortedLongMap(dropTake())
		}

	}

	type Sorted = SortedFitSet[Long]

	object Sorted {
		final val Empty :SortedFitSet[Long] = new SortedLongMap(LongKey.Empty)

		def newBuilder :FitBuilder[Long, SortedFitSet[Long]] = new ImmutableSetBuilder(Empty)

		def Singleton(value :Long) :SortedFitSet[Long] = new SortedLongMap(LongKey.Singleton(value - java.lang.Long.MIN_VALUE))

		private[LongKeys$] final val SignedToUnsigned :Long=>Long = { l :Long => l + java.lang.Long.MIN_VALUE }
		private[LongKeys$] final val UnsignedToSinged :Long=>Long = { l :Long => l - java.lang.Long.MIN_VALUE }
	}


	@tailrec
	private def traverseStack(s :LongKey, stack :List[LongKey]=Nil) :List[LongKey] = s match {
		case Empty => stack
		case s :Singleton => s::stack
		case b :Branch => traverseStack(b.left, b::stack)
	}



	private[LongKeys$] class LongMapIterator(private[this] var stack :List[LongKey])
		extends BaseIterator[Long] with FitIterator[Long]
	{
		def this(set :LongKey) = this(traverseStack(set))

		override def hasFastSize: Boolean = stack.isEmpty
		override def hasDefiniteSize = true

		override def hasNext: Boolean = stack.nonEmpty

		override def head: Long = stack.head.path

		override def next(): Long = {
			val res = stack.head.path
			stack = stack.tail
			stack match {
				case Nil => res
				case (b :Branch)::tail =>
					stack = traverseStack(b.right, tail)
					res
				case _ => throw new MatchError(s"Expected to find LongMap.Branch on top of the stack, but got: $stack")
			}
		}

		override def skip(): Unit = stack.tail match {
			case Nil => ()
			case (b :Branch)::suffix => stack = traverseStack(b.right, suffix)
			case _ => throw new MatchError(s"Expected to find LongMap.Branch on top of the stack, but got: $stack")
		}

	}


	private class LongMapBuilder extends FitBuilder[Long, LongKey] {
		private[this] var res :LongKey = Empty

		override def +=(elem: Long): this.type = {
			res += elem; this
		}

		override def clear(): Unit = res = Empty

		def result() = res

		def count = res.size
	}





	object Mutable {
		def empty :MutableSet[Long] = MutableSet.adapt(Empty)
		def newBuilder :FitBuilder[Long, MutableSet[Long]] = MutableSet.adapt(Empty)
		def Singleton(value :Long) :MutableSet[Long] = MutableSet.adapt(LongKey.Singleton(value))
	}

}
*/
