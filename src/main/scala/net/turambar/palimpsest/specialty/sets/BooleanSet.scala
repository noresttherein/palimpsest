package net.turambar.palimpsest.specialty.sets

import java.lang

import net.turambar.palimpsest.specialty.iterables.{DoubletonFoundation, EmptyIterable, SingletonFoundation, SingletonSpecialization}
import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator, FitTraversableOnce, Specialized}
import net.turambar.palimpsest.specialty.FitIterator.{BaseIterator, FastSizeIterator}
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.sets.BooleanSet.{BooleanSetIterator, IsTrue}
import net.turambar.palimpsest.specialty.Specialized.{Fun1, Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.sets.ValSet.{ImmutableSetBuilder, Mutable, Stable}

import scala.collection.{GenSet, GenTraversableOnce}
import scala.collection.generic.CanBuildFrom


/** A set of boolean values (i.e. one of `{}, {false}, {true}, {false, true}`) represented
  * as a bitmap. The lowest bit (value 1) specifies whether 'false' belongs to this set,
  * and the second bit (value 2) specifies whether 'true' belongs to this set.
  * This is my attempt at humour, if anyone wondered.
  * @author Marcin Mościcki
  */
final class BooleanSet protected (private[this] var bitmap :Int) extends MutableOrderedSet[Boolean] with OfKnownSize {
	@inline private def toBitset = bitmap

	protected[this] override def mySpecialization: Specialized[Boolean] = Specialized.SpecializedBoolean
	override implicit def ordering: Ordering[Boolean] = Ordering.Boolean

	override def empty = new BooleanSet(0)
	
	override def size: Int = bitmap - (bitmap >> 1)
	override def count = bitmap - (bitmap >> 1)

	override def hasFastSize = true
	override def hasDefiniteSize: Boolean =  true
	
	override def head =
		if (bitmap==0) throw new NoSuchElementException(s"Set().head")
		else bitmap == 2 // case {true} => true; case _ => false

	override def last =
		if (bitmap==0) throw new NoSuchElementException(s"Set().last")
		else bitmap != 1 // case {false} => false; case _ => true
	
	override def tail =
		if (bitmap>0) new BooleanSet(bitmap ^ (bitmap & -bitmap)) //def lowestOneBit = i & -i
		else throw new UnsupportedOperationException("Set().tail")

	override def init =
		if (bitmap>0) new BooleanSet((bitmap - 2) & 3)
		else throw new UnsupportedOperationException("Set().init")

	override def isEmpty = bitmap==0

	override def nonEmpty = bitmap>0
	
	override def contains(elem: Boolean): Boolean =
		elem && (bitmap & 2) > 0 || !elem && (bitmap & 1) > 0

	//todo: projection carrying mutations to this
	override def keysIteratorFrom(start: Boolean): FitIterator[Boolean] =
		if (start) new BooleanSetIterator(bitmap & 2)
		else new BooleanSetIterator(bitmap)

	//todo: projection carrying mutations to this
	override def rangeImpl(from: Option[Boolean], until: Option[Boolean]): MutableOrderedSet[Boolean] =
		if (until.isDefined)
			if (until.get) new BooleanSet(bitmap & 1)
			else new BooleanSet(0)
		else if (from.isDefined && from.get) new BooleanSet(bitmap & 2)
		else new BooleanSet(bitmap)



	override def +(elem: Boolean): MutableOrderedSet[Boolean] =
		new BooleanSet(bitmap | (if (elem) 2 else 1))
	
	override def -(elem: Boolean): MutableOrderedSet[Boolean] =
		new BooleanSet(bitmap & (if (elem) 1 else 2))



	override def +=(elem: Boolean): this.type = {
		bitmap |= (if (elem) 2 else 1)
		this
	}

	override def -=(elem: Boolean): this.type = {
		bitmap &= (if (elem) 1 else 2)
		this
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
			++=(xs.fitIterator)
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
		case _ => --=(xs.fitIterator)
	}



	override def clear(): Unit = bitmap = 0


	override def foreach[@specialized(Unit) U](f: (Boolean) => U) :Unit = bitmap match {
		case 1 => f(false)
		case 2 => f(true)
		case 3 => f(false); f(true);
		case _ => ()
	}

	override protected def reverseForeach(f: (Boolean) => Unit): Unit = bitmap match {
		case 1 => f(false)
		case 2 => f(true)
		case 3 => f(true); f(false)
		case _ => ()
	}

	override protected[this] def filter(p: (Boolean) => Boolean, ourTruth: Boolean): MutableOrderedSet[Boolean] = new BooleanSet(
		(if ((bitmap & 1) > 0 && p(false)==ourTruth) 1 else 0) |
		(if ((bitmap & 2) > 0 && p(true)==ourTruth) 2 else 0)
	)

	override def map[@specialized(Fun1Vals) O, That](f: (Boolean) => O)(implicit bf: CanBuildFrom[MutableOrderedSet[Boolean], O, That]) = {
		val b = bf(this)
		if ((bitmap & 1) > 0) b += f(false)
		if ((bitmap & 2) > 0) b += f(true)
		b.result()
	}

	override def flatMap[U, That](f: (Boolean) => GenTraversableOnce[U])(implicit bf: CanBuildFrom[MutableOrderedSet[Boolean], U, That]) = {
		val b = bf(this)
		if ((bitmap & 1) > 0) b ++= f(false).seq
		if ((bitmap & 2) > 0) b ++= f(true).seq
		b.result()
	}

	override def iterator: FitIterator[Boolean] = new BooleanSetIterator(bitmap)

	override def stable: OrderedSet.Stable[Boolean] = bitmap match {
		case 0 => BooleanSet.Empty
		case 1 => BooleanSet.False
		case 2 => BooleanSet.True
		case _ => BooleanSet.Both
	}

	override def result(): BooleanSet = this


	override def stringPrefix = "BooleanSet"
	
	override def toString = bitmap match {
		case 0 => "BooleanSet()"
		case 1 => "BooleanSet(false)"
		case 2 => "BooleanSet(true)"
		case 3 => "BooleanSet(false, true)"
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
		new ImmutableSetBuilder[Boolean, StableOrderedSet[Boolean]](Empty)

	@inline private[BooleanSet] final val IsTrue = { x :Boolean => x }


	/** Empty, immutable boolean set. Adding elements will always return a constant representing
	  * one of four possible values of this set type: `{}, {false}, {true}, {false, true}`.
	  */
	object Empty extends EmptyIterable[Boolean, StableOrderedSet[Boolean]] with StableOrderedSet[Boolean] {
		override def ordering = Ordering.Boolean
		protected[this] override def mySpecialization: Specialized[Boolean] = Specialized.SpecializedBoolean


		override def empty = this
		override def contains(elem: Boolean): Boolean = false

		override def +(elem: Boolean): StableOrderedSet[Boolean] = if (elem) True else False

		override def -(elem: Boolean): StableOrderedSet[Boolean] = this


		override def intersect(that: GenSet[Boolean]): StableOrderedSet[Boolean] = this

		override def union(that: GenSet[Boolean]): StableOrderedSet[Boolean] = that match {
			case s :StableOrderedSet[Boolean] => s
			case s :BooleanSet => s.stable
			case _ => this ++ that
		}

		override def diff(that: GenSet[Boolean]): StableOrderedSet[Boolean] = this

		override def mutable: MutableOrderedSet[Boolean] = new BooleanSet(0)

		override def keysIteratorFrom(start: Boolean): FitIterator[Boolean] = FitIterator.Empty

		override def rangeImpl(from: Option[Boolean], until: Option[Boolean]): StableOrderedSet[Boolean] = this

		protected override def verifiedCopyTo(xs: Array[Boolean], start: Int, total: Int) = 0
	}

	/** Immutable boolean set containing exactly one element. */
	private final class Singleton(value :Boolean)
		extends SingletonFoundation[Boolean, StableOrderedSet[Boolean]] with StableOrderedSet[Boolean]
				with SingletonSpecialization[Boolean, StableOrderedSet[Boolean]]
	{
		override def ordering = Ordering.Boolean
		protected[this] override def mySpecialization: Specialized[Boolean] = Specialized.SpecializedBoolean


		override def empty = Empty
		@inline override def head = value

		override def contains(elem: Boolean): Boolean = elem==head

		override def +(elem: Boolean): StableOrderedSet[Boolean] = if (elem==head) this else Both

		override def -(elem: Boolean): StableOrderedSet[Boolean] = if (elem==value) Empty else this


		override def intersect(that: GenSet[Boolean]): StableOrderedSet[Boolean] = that match {
			case Empty => Empty
			case Both => this
			case _ =>
				if (that.contains(value)) this else Empty
		}

		override def union(that: GenSet[Boolean]): StableOrderedSet[Boolean] = that match {
			case Empty => this
			case Both => Both
			case s :Singleton => if (s.head==value) this else Both
			case s :BooleanSet => if (that.contains(!value)) Both else this
			case _ if that.contains(!value) => Both
			case _ => this
		}

		override def diff(that: GenSet[Boolean]): StableOrderedSet[Boolean] = that match {
			case Empty => this
			case other :ValSet[Boolean] =>
				if (other.contains(value)) Empty else this
			case _ if that.contains(value) => Empty
			case _ => this
		}

		override def mutable: MutableOrderedSet[Boolean] = new BooleanSet(if (value) 2 else 1)

		override def keysIteratorFrom(start: Boolean): FitIterator[Boolean] =
			if (start && !value) FitIterator.empty else FitIterator(value)

		override def rangeImpl(from: Option[Boolean], until: Option[Boolean]): StableOrderedSet[Boolean] =
			if (until.isDefined)
				if (value || !until.get) Empty
				else if(from.isDefined && from.get) Empty //!value
				else this
			else if (!value && from.isDefined && from.get) Empty
			else this

		protected override def verifiedCopyTo(xs: Array[Boolean], start: Int, total: Int) = {
			xs(start) = value; 1
		}
	}

	/** Singleton immutable set for `{true}`. */
	final val True :StableOrderedSet[Boolean] = new Singleton(true)

	/** Singleton immutable set for `{false}`. */
	final val False :StableOrderedSet[Boolean] = new Singleton(false)

	/** Dedicated implementation for maximal immutable set containing both values: `{false, true}`. */
	object Both extends DoubletonFoundation[Boolean, StableOrderedSet[Boolean]] with StableOrderedSet[Boolean] {
		override implicit def ordering: Ordering[Boolean] = Ordering.Boolean
		protected[this] override def mySpecialization: Specialized[Boolean] = Specialized.SpecializedBoolean


		override def empty = Empty
		override def head = false
		override def last = true
		override def init = False
		override def tail = True

		override def contains(elem: Boolean): Boolean = true

		override def +(elem: Boolean): StableOrderedSet[Boolean] = this

		override def -(elem: Boolean): StableOrderedSet[Boolean] = if (elem) False else True


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
			case Empty => this
			case Both => Empty
			case s :Singleton => if (that.head) False else True
			case _ =>
				if (that.contains(true))
					if (that.contains(false)) Empty else False
				else
					if (that.contains(false)) True else this
		}

		override def mutable: MutableOrderedSet[Boolean] = new BooleanSet(3)


		override def keysIteratorFrom(start: Boolean): FitIterator[Boolean] =
			if (start) FitIterator(true)
			else FitIterator(false, true)

		override def rangeImpl(from: Option[Boolean], until: Option[Boolean]): StableOrderedSet[Boolean] =
			if (until.isDefined)
				if (!until.get) Empty
				else if (from.isDefined && from.get) Empty
				else False
			else if (from.isDefined && from.get) True
			else this

		protected override def verifiedCopyTo(xs: Array[Boolean], start: Int, total: Int) = {
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
		override def hasNext = bitmap != 0
		override def size = bitmap - (bitmap >> 1)
		
		def head :Boolean = bitmap == 2
		
		override def skip(): Unit = bitmap ^= (bitmap & -bitmap) //clear lowest one

		override def next() :Boolean = { //00 => Nil; 01 => false::Nil; 10 => true::Nil; 11 => false::true::Nil
			val res = bitmap == 2
			bitmap ^= bitmap & -bitmap
			res
		}
	}

/*
	private class BooleanSetBuilder extends FitBuilder[Boolean, BooleanSet] {
		private[this] var bitmap = 0
		
		override def addOne :Boolean => Unit = { b => if (b) bitmap |= 1 else bitmap |= 2 }
		
		def count = bitmap - (bitmap >> 1)
		
		override def +=(elem1: Boolean, elem2: Boolean, elems: Boolean*): this.type = {
			if (bitmap!=3)
				if (elem1 ^ elem2) bitmap = 3
				else if (elem1 | elem2) {
					bitmap |= 1
					if (bitmap!=3 && !elems.forall(identity))
						bitmap = 3
				} else {
					bitmap |= 2
					if (bitmap!=3 && elems.exists(identity))
						bitmap = 3
				}
			this
		}
		
		override def ++=(xs: TraversableOnce[Boolean]): this.type = {
			if (xs.nonEmpty) bitmap match {
				case 1 =>
					if (!xs.forall(identity))
						bitmap = 3
				case 2 =>
					if (xs.exists(identity))
						bitmap = 3
				case 0 =>
					if (xs.forall(identity)) bitmap = 1
					else if (xs.exists(identity)) bitmap = 3
					else bitmap = 2
				case _ =>
			}
			this
		}
		
		override def +=(elem: Boolean): this.type =
			{ if (elem) bitmap |= 1 else bitmap |= 2; this }
		
		override def result(): BooleanSet = new BooleanSet(bitmap)
		
		override def clear(): Unit = bitmap = 0
	}
*/
}