package net.turambar.palimpsest.specialty.sets

import java.lang

import net.turambar.palimpsest.specialty.iterables.{DoubletonFoundation, DoubletonSpecialization, EmptyIterableFoundation, EmptyIterableTemplate, SingletonFoundation, SingletonSpecialization}
import net.turambar.palimpsest.specialty.{?, Blank, FitBuilder, FitTraversableOnce, RuntimeType, Sure}
import net.turambar.palimpsest.specialty.iterators.{BaseIterator, FastSizeIterator}
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.sets.BooleanSet.{BooleanSetIterator, IsTrue}
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.{Fun1, Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.ordered.OrderedBy.OrderedEmpty
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.sets.ValSet.StableSetBuilder
import net.turambar.palimpsest.specialty.RuntimeType.Specialized

import scala.collection.{GenSet, GenTraversableOnce}
import scala.collection.generic.CanBuildFrom


/** A, mostly humorous, mutable set of boolean values (i.e. one of `{}, {false}, {true}, {false, true}`) represented
  * as a bitmap. The lowest bit (value 1) specifies whether 'false' belongs to this set,
  * and the second bit (value 2) specifies whether 'true' belongs to this set.
  *
  * @author Marcin Mościcki
  */
final class BooleanSet protected (private[this] var bitmap :Int) extends MutableOrderedSet[Boolean] with OfKnownSize {
	//todo: reduce the size of the class by at least half.

	@inline private def toBitset :Int = bitmap

	override def specialization: RuntimeType[Boolean] = RuntimeType.OfBoolean
	override implicit def ordering: ValOrdering[Boolean] = ValOrdering.BooleanOrdering

	override def empty = new BooleanSet(0)
	
	override def size: Int = bitmap - (bitmap >> 1)

	override def isEmpty :Boolean = bitmap == 0

	override def nonEmpty :Boolean = bitmap > 0

	override def hasFastSize = true
	override def hasDefiniteSize: Boolean =  true


	override def head :Boolean =
		if (bitmap==0) throw new NoSuchElementException(s"Set().head")
		else bitmap == 2 // case {true} => true; case _ => false

	override def last :Boolean =
		if (bitmap==0) throw new NoSuchElementException(s"Set().last")
		else bitmap != 1 // case {false} => false; case _ => true

	override def keyAt(n :Int) :Boolean =
		if (n < 0 || n >= bitmap - (bitmap >> 1)) throw new NoSuchElementException(s"$this.keyAt($n)")
		else (bitmap & 1) == n


	override def tail :BooleanSet =
		if (bitmap>0) new BooleanSet(bitmap ^ (bitmap & -bitmap)) //def lowestOneBit = i & -i
		else throw new UnsupportedOperationException("Set().tail")

	override def init :BooleanSet =
		if (bitmap>0) new BooleanSet((bitmap - 2) & 3)
		else throw new UnsupportedOperationException("Set().init")



	override def filter(p: Boolean => Boolean, ourTruth: Boolean): MutableOrderedSet[Boolean] =
		new BooleanSet(
		  (if ((bitmap & 1) > 0 && p(false) == ourTruth) 1 else 0) |
		      (if ((bitmap & 2) > 0 && p(true) == ourTruth) 2 else 0)
		)


	override def foreach[@specialized(Unit) U](f: Boolean => U) :Unit = bitmap match {
		case 1 => f(false)
		case 2 => f(true)
		case 3 => f(false); f(true);
		case _ => ()
	}

	override protected def reverseForeach(f: Boolean => Unit): Unit = bitmap match {
		case 1 => f(false)
		case 2 => f(true)
		case 3 => f(true); f(false)
		case _ => ()
	}


	override def map[@specialized(Fun1Vals) O, That](f: Boolean => O)(implicit bf: CanBuildFrom[MutableOrderedSet[Boolean], O, That]) :That = {
		val b = bf(this)
		if ((bitmap & 1) > 0) b += f(false)
		if ((bitmap & 2) > 0) b += f(true)
		b.result()
	}

	override def flatMap[U, That](f: Boolean => GenTraversableOnce[U])(implicit bf: CanBuildFrom[MutableOrderedSet[Boolean], U, That]) :That = {
		val b = bf(this)
		if ((bitmap & 1) > 0) b ++= f(false).seq
		if ((bitmap & 2) > 0) b ++= f(true).seq
		b.result()
	}


	override def find_?(p :Boolean => Boolean, where :Boolean): ?[Boolean] =
		if ((bitmap & 1) > 0 && p(false) == where)
			Sure(false)
		else if ((bitmap & 2) > 0 && p(true) == where)
			Sure(true)
		else
			Blank




	override def contains(elem: Boolean): Boolean =
		elem && (bitmap & 2) > 0 || !elem && (bitmap & 1) > 0


	override def +(elem: Boolean): MutableOrderedSet[Boolean] =
		new BooleanSet(bitmap | (if (elem) 2 else 1))
	
	override def -(elem: Boolean): MutableOrderedSet[Boolean] =
		new BooleanSet(bitmap & (if (elem) 1 else 2))

	override def ^(elem :Boolean) :MutableOrderedSet[Boolean] =
		new BooleanSet(bitmap ^ (if (elem) 1 else 2))


	override def ^(that :GenSet[Boolean]) :MutableOrderedSet[Boolean] = that match {
		case set :BooleanSet => new BooleanSet(bitmap ^ set.toBitset)
		case set :ValSet[Boolean] =>
			var bitset = bitmap
			if (set(true)) bitset ^= 1
			if (set(false)) bitset ^= 2
			new BooleanSet(bitset)
		case _ =>
			var bitset = bitmap
			if (that(true)) bitset ^= 1
			if (that(false)) bitset ^= 2
			new BooleanSet(bitset)
	}

	override def &(that :ValSet[Boolean]) :MutableOrderedSet[Boolean] = that match {
		case set :BooleanSet => new BooleanSet(bitmap & set.toBitset)
		case _ =>
			var bitset = bitmap
			if (!that.contains(false)) bitset &= ~1
			if (!that.contains(true)) bitset &= ~2
			new BooleanSet(bitset)
 	}

	override def +=(elem: Boolean): this.type = {
		bitmap |= (if (elem) 2 else 1)
		this
	}

	override def -=(elem: Boolean): this.type = {
		bitmap &= (if (elem) 1 else 2)
		this
	}

	override def flip(elem :Boolean): Boolean = {
		val bit = if (elem) 1 else 2
		bitmap ^= bit
		(bitmap & bit) != 0
	}

	override def +=(elem1: Boolean, elem2: Boolean, elems: Boolean*): this.type = {
		if (bitmap!=3)
			if (elem1 ^ elem2) //elem1 != elem2
				bitmap = 3
			else if (elem1) {
				bitmap |= 2
				if (bitmap!=3 && !elems.forall(IsTrue))
					bitmap = 3
			} else {
				bitmap |= 1
				if (bitmap!=3 && elems.exists(IsTrue))
					bitmap = 3
			}
		this
	}


	override def -=(elem1: Boolean, elem2: Boolean, elems: Boolean*): this.type = {
		if (bitmap > 0)
			if (elem1 ^ elem2) //elem1 != elem2
				bitmap = 0
			else if (elem1)
				if ((bitmap & 1) == 0 || !elems.forall(IsTrue))
					bitmap = 0
				else
					bitmap = 1
			else //!elem1 && !elem2
				if ((bitmap & 2) > 0 && !elems.exists(IsTrue))
					bitmap = 2
				else
					bitmap = 0
		this
	}


	override def ++=(xs: TraversableOnce[Boolean]): this.type = {
		xs match {
			case other :BooleanSet => bitmap |= other.toBitset
//			case other :FitTraversableOnce[Boolean] => this ++= other
			case _ if xs.nonEmpty => bitmap match {
				case 1 =>
					if (xs.exists(IsTrue))
						bitmap = 3
				case 2 =>
					if (!xs.forall(IsTrue))
						bitmap = 3
				case 0 =>
					if (xs.forall(IsTrue)) bitmap = 2
					else if (xs.exists(IsTrue)) bitmap = 3
					else bitmap = 1
				case _ =>
			}
			case _ =>
		}
		this
	}


	override def ++=(xs: FitTraversableOnce[Boolean]): this.type = xs match {
		case other :BooleanSet => bitmap |= other.toBitset; this
		case i :FitIterator[Boolean] =>
			while (bitmap != 3 && i.hasNext)
				if (i.next()) bitmap |= 2
				else bitmap |= 1
			this
		case _ if bitmap != 3 && xs.nonEmpty =>
			++=(xs.toIterator)
		case _ => this
	}





	override def --=(xs: TraversableOnce[Boolean]): this.type = xs match {
		case other :BooleanSet => bitmap &= ~other.toBitset; this
		case _ if bitmap == 0 => this
		case _ if xs.nonEmpty =>
			if ((bitmap & 2) == 0 || xs.forall(IsTrue))
				bitmap &= 1
			else if (xs.exists(IsTrue))
				bitmap = 0
			else
				bitmap &= 2
			this
		case _ => this
	}

	override def --=(xs: FitTraversableOnce[Boolean]): this.type = xs match {
		case other :BooleanSet => bitmap &= ~other.toBitset; this
		case i :FitIterator[Boolean] =>
			while (bitmap>0 && i.hasNext)
				if (i.next()) bitmap &= 1
				else bitmap &= 2
			this
		case _ if bitmap==0 || xs.isEmpty => this
		case _ => --=(xs.toIterator)
	}



	override def clear(): Unit = bitmap = 0



	override def iterator: FitIterator[Boolean] = new BooleanSetIterator(bitmap)


	//todo: projection carrying mutations to this
	override def keysIteratorFrom(start: Boolean): FitIterator[Boolean] =
		if (start) new BooleanSetIterator(bitmap & 2)
		else new BooleanSetIterator(bitmap)

	//todo: projection carrying mutations to this
	override def rangeImpl(from: ?[Boolean], until: ?[Boolean]): MutableOrderedSet[Boolean] =
		if (until.isDefined)
			if (until.get) new BooleanSet(bitmap & 1)
			else new BooleanSet(0)
		else if (from.isDefined && from.get) new BooleanSet(bitmap & 2)
		else new BooleanSet(bitmap)




	override def stable: StableOrderedSet[Boolean] = bitmap match {
		case 0 => BooleanSet.Empty
		case 1 => BooleanSet.False
		case 2 => BooleanSet.True
		case _ => BooleanSet.Both
	}

	override def result(): BooleanSet = this


	override def stringPrefix = "Set[Boolean]"

	override def debugPrefix = "BooleanSet"

	override def toString :String = bitmap match {
		case 0 => "Set[Boolean]()"
		case 1 => "Set[Boolean](false)"
		case 2 => "Set[Boolean](true)"
		case 3 => "Set[Boolean](false, true)"
	}

}


/** Companion and factory for mutable and immutable sets of `Boolean` values.
  * Mutable sets are represented by the class of the same name.
  * Immutable sets are represented by one of four constant values specific for the sets power (size).
  * Both set types are ordered using default `Boolean` ordering placing `false` before `true` in all traversals.
  * All classess used in these implementations are protected and intended to by used through standard specialized
  * interfaces such as [[StableSet]], [[MutableSet]], etc..
  */
private[sets] object BooleanSet {
	/** Returns the constant for empty, immutable boolean sets. */
	@inline def empty :StableOrderedSet[Boolean] = Empty

	/** Returns a new, empty mutable set. */
	@inline def apply() :MutableOrderedSet[Boolean] = new BooleanSet(0)

	/** Returns a singleton mutable set. */
	@inline final def apply(value :Boolean) :MutableOrderedSet[Boolean] =
		if (value) new BooleanSet(2) else new BooleanSet(1)
//		if(value) True else False

	/** Returns the constant immutable set representing the singleton for the given value. */
	@inline final def singleton(value :Boolean) :StableOrderedSet[Boolean] =
		if (value) True else False

	/** Builder for immutable boolean sets.
	  * Note that mutable sets are their own builders and need not an additional implementation.
	  * @return default set builder iteratively growing its immutable result set.
	  */
	@inline final def newBuilder :FitBuilder[Boolean, StableOrderedSet[Boolean]] = //new BooleanSetBuilder
		new StableSetBuilder[Boolean, StableOrderedSet[Boolean]](Empty)

	@inline private[BooleanSet] final val IsTrue = { x :Boolean => x }



	/** Empty, immutable boolean set. Adding elements will always return a constant representing
	  * one of four possible values of this set type: `{}, {false}, {true}, {false, true}`.
	  */
	case object Empty extends /*EmptyIterableFoundation[Boolean, StableOrderedSet[Boolean]] with */StableOrderedSet[Boolean] with EmptyIterableTemplate[Boolean, StableOrderedSet[Boolean]]/*with OrderedEmpty[Boolean, StableOrderedSet[Boolean]]*/ {
		//todo: empty set base class?
		override def ordering :ValOrdering[Boolean] = ValOrdering.BooleanOrdering
		override def specialization: RuntimeType[Boolean] = RuntimeType.OfBoolean

		override def inverse :StableOrderedSet[Boolean] = this
		override def empty :Empty.type = this
		override def contains(elem: Boolean): Boolean = false

		override def +(elem: Boolean): StableOrderedSet[Boolean] = if (elem) True else False

		override def -(elem: Boolean): StableOrderedSet[Boolean] = this

		override def ^(elem :Boolean) :StableOrderedSet[Boolean] = new Singleton(elem)

		override def intersect(that: GenSet[Boolean]): StableOrderedSet[Boolean] = this

		override def &(that :ValSet[Boolean]) :StableOrderedSet[Boolean] = this

		override def union(that: GenSet[Boolean]): StableOrderedSet[Boolean] = that match {
			case s :StableOrderedSet[Boolean] => s
			case s :BooleanSet => s.stable
			case _ if that(true) => if (that(false)) Both else True
			case _ if that(false) => False
			case _ => this
		}

		override def diff(that: GenSet[Boolean]): StableOrderedSet[Boolean] = this

		override def ^(that :GenSet[Boolean]) :StableOrderedSet[Boolean] = this union that

		override def ^(that :ValSet[Boolean]) :StableOrderedSet[Boolean] = this union that

		override def mutable: MutableOrderedSet[Boolean] = new BooleanSet(0)

		override def keyAt(n :Int) :Boolean = throw new NoSuchElementException(s"Set[Boolean]().keyAt($n)")

		override def keysIteratorFrom(start: Boolean): FitIterator[Boolean] = FitIterator.Empty

		override def rangeImpl(from: ?[Boolean], until: ?[Boolean]): StableOrderedSet[Boolean] = this

		protected override def trustedCopyTo(xs: Array[Boolean], start: Int, total: Int) = 0
	}


	/** Immutable boolean set containing exactly one element. */
	private final class Singleton(value :Boolean)
		extends SingletonFoundation[Boolean, StableOrderedSet[Boolean]] with StableOrderedSet[Boolean]
				with SingletonSpecialization[Boolean, StableOrderedSet[Boolean]]
	{
		override def ordering :ValOrdering[Boolean] = ValOrdering.BooleanOrdering
		override def specialization: RuntimeType[Boolean] = RuntimeType.OfBoolean

		override def inverse :StableOrderedSet[Boolean] = this
		override def empty :Empty.type = Empty
		@inline override def head :Boolean = value

		override def keyAt(n :Int) :Boolean =
			if (n==0) value else throw new NoSuchElementException(s"Set[Boolean]($value).keyAt($n)")

		override def contains(elem: Boolean): Boolean = elem==head

		override def +(elem: Boolean): StableOrderedSet[Boolean] = if (elem==head) this else Both

		override def -(elem: Boolean): StableOrderedSet[Boolean] = if (elem==value) Empty else this

		override def ^(elem :Boolean) :StableOrderedSet[Boolean] =
			if (elem!=value) Both
			else if (elem) False
			else True

		override def intersect(that :GenSet[Boolean]) :StableOrderedSet[Boolean] = that match {
			case fit :ValSet[Boolean] =>
				&(that)
			case _ if that.contains(value) => this
			case _ => Empty
		}

		override def &(that: ValSet[Boolean]): StableOrderedSet[Boolean] =
			if (that.isEmpty) Empty
			else if (that.contains(value)) this else Empty


		override def union(that: GenSet[Boolean]): StableOrderedSet[Boolean] = that match {
			case s :ValSet[Boolean] => if (s.contains(!value)) Both else this
			case _ if that.contains(!value) => Both
			case _ => this
		}

		override def diff(that: GenSet[Boolean]): StableOrderedSet[Boolean] = that match {
			case other :ValSet[Boolean] =>
				if (other.contains(value)) Empty else this
			case _ if that.contains(value) => Empty
			case _ => this
		}

		override def ^(that :GenSet[Boolean]): StableOrderedSet[Boolean] = that match {
			case _ if that.isEmpty => this
			case other :ValSet[Boolean] => this ^ other
			case _ =>
				if (that.contains(value))
					if (that.contains(!value))
						if (value) False else True
					else Empty
				else Both
		}

		override def ^(that :ValSet[Boolean]) :StableOrderedSet[Boolean] =
			if (that.contains(value))
				if (that.contains(!value))
					if (value) False else True
				else Empty
			else Both



		override def mutable: MutableOrderedSet[Boolean] = new BooleanSet(if (value) 2 else 1)

		override def keysIteratorFrom(start: Boolean): FitIterator[Boolean] =
			if (start && !value) FitIterator.empty else FitIterator.one(value)

		override def rangeImpl(from: ?[Boolean], until: ?[Boolean]): StableOrderedSet[Boolean] =
			if (until.isDefined)
				if (value || !until.get) Empty
				else if(from.isDefined && from.get) Empty //!value
				else this
			else if (!value && from.isDefined && from.get) Empty
			else this

	}


	/** Singleton immutable set for `{true}`. */
	final val True :StableOrderedSet[Boolean] = new Singleton(true)

	/** Singleton immutable set for `{false}`. */
	final val False :StableOrderedSet[Boolean] = new Singleton(false)

	/** Dedicated implementation for maximal immutable set containing both values: `{false, true}`. */
	case object Both extends DoubletonFoundation[Boolean, StableOrderedSet[Boolean]]
		           with StableOrderedSet[Boolean] with DoubletonSpecialization[Boolean, StableOrderedSet[Boolean]]
	{

		override implicit def ordering: ValOrdering[Boolean] = ValOrdering.BooleanOrdering
		override def specialization: RuntimeType[Boolean] = RuntimeType.OfBoolean

		override def inverse :StableOrderedSet[Boolean] =
			StableOrderedSet.of[Boolean](ValOrdering.ReverseBooleanOrdering) ++ this
		override def empty :StableOrderedSet[Boolean] = Empty

		override def head = false
		override def last = true

		override def init :StableOrderedSet[Boolean] = False
		override def tail :StableOrderedSet[Boolean] = True


		override def keyAt(n :Int) :Boolean =
			if (n < 0 | n > 1)
				throw new NoSuchElementException(s"Set[Boolean](false, true).keyAt($n)")
			else n==1

		override def contains(elem: Boolean): Boolean = true

		override def +(elem: Boolean): StableOrderedSet[Boolean] = this

		override def -(elem: Boolean): StableOrderedSet[Boolean] = if (elem) False else True

		override def ^(elem :Boolean): StableOrderedSet[Boolean] = if (elem) False else True

		override def &(that :ValSet[Boolean]) :StableOrderedSet[Boolean] =
			if (that.contains(true))
				if (that.contains(false)) this else True
			else
				if (that.contains(false)) False else Empty

		override def intersect(that: GenSet[Boolean]): StableOrderedSet[Boolean] = that match {
			case other :StableOrderedSet[Boolean] => other
			case s :BooleanSet => s.stable
			case _ =>
				if (that.contains(true))
					if (that.contains(false)) this else True
				else
					if (that.contains(false)) False else Empty
		}


		override def union(that: GenSet[Boolean]): StableOrderedSet[Boolean] = this

		override def diff(that: GenSet[Boolean]): StableOrderedSet[Boolean] = that match {
			case s :ValSet[Boolean] =>
				if (s.contains(true))
					if (s.contains(false)) Empty else False
				else
					if (s.contains(false)) True else this
			case _ => that.size match {
				case 0 => this
				case 2 => Empty
				case _ if that.contains(true) => False
				case _ => True
			}
		}

		override def ^(that :GenSet[Boolean]) :StableOrderedSet[Boolean] = diff(that)

		override def ^(that :ValSet[Boolean]) :StableOrderedSet[Boolean] = diff(that)

		override def mutable: MutableOrderedSet[Boolean] = new BooleanSet(3)


		override def keysIteratorFrom(start: Boolean): FitIterator[Boolean] =
			if (start) FitIterator.one(true)
			else FitIterator.two(false, true)

		override def rangeImpl(from: ?[Boolean], until: ?[Boolean]): StableOrderedSet[Boolean] =
			if (until.isDefined)
				if (!until.get) Empty
				else if (from.isDefined && from.get) Empty
				else False
			else if (from.isDefined && from.get) True
			else this

		protected override def trustedCopyTo(xs: Array[Boolean], start: Int, total: Int) :Int = {
			xs(start) = false
			if (total>1) {
				xs(start+1) = true
				2
			} else 1
		}
	}


	private class BooleanSetIterator (private[this] var bitmap :Int)
		extends FastSizeIterator[Boolean] with FitIterator[Boolean]
	{
		override def specialization :Specialized[Boolean] = RuntimeType.OfBoolean

		override def hasNext :Boolean = bitmap != 0
		override def size :Int = bitmap - (bitmap >> 1)
		
		def head :Boolean = bitmap == 2
		
		override def skip(): Unit = bitmap ^= (bitmap & -bitmap) //clear lowest one

		override def next() :Boolean = { //00 => Nil; 01 => false::Nil; 10 => true::Nil; 11 => false::true::Nil
			val res = bitmap == 2
			bitmap ^= bitmap & -bitmap
			res
		}
	}

}