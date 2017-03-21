package net.turambar.palimpsest.specialty.sets

import java.lang

import net.turambar.palimpsest.specialty.iterables.{DoubletonFoundation, EmptyIterable, IterableFoundation, SingletonFoundation, SingletonSpecialization}
import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator, Specialized}
import net.turambar.palimpsest.specialty.FitIterator.{BaseIterator, FastSizeIterator}
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.sets.BooleanSet.{BooleanSetBuilder, BooleanSetIterator}
import net.turambar.palimpsest.specialty.Specialized.{Fun1, Fun1Vals, Fun2}
import net.turambar.palimpsest.specialty.sets.ValSet.{ImmutableSetBuilder, Mutable, Stable}

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom


/** A set of boolean values (i.e. one of `{}, {false], {true}, {true, false}`) represented
  * as a bitmap. The first bit (value 1) specifies whether 'true' belongs to this set,
  * and the second bit (value 2) specifies whether 'false' belongs to this set.
  * This is my sense of humor, if anyone wondered.
  * @author Marcin Mościcki
  */
sealed class BooleanSet protected (private[this] var bitmap :Int) extends ValSet[Boolean] with OfKnownSize {
	import java.{lang=>j}

	protected[this] override def mySpecialization: Specialized[Boolean] = Specialized.SpecializedBoolean
	override def empty = BooleanSet.Empty
	
	override final def size: Int = bitmap - (bitmap >> 1)
	override def hasFastSize = true
	override def hasDefiniteSize: Boolean =  true
	
	override final def head =
		if (bitmap==0) throw new NoSuchElementException(s"Set().head")
		else (bitmap & 1) > 0
	
	override final def last =
		if (bitmap<3) throw new NoSuchElementException(s"Set($head).tail")
		else (bitmap & 2) == 0
	
	override final def tail =
		if (bitmap>0) new BooleanSet(bitmap ^ j.Integer.lowestOneBit(bitmap))
		else throw new UnsupportedOperationException(s"Set().tail")
	
	override final def isEmpty = bitmap==0
	override final def nonEmpty = bitmap>0
	
	override final def contains(elem: Boolean): Boolean =
		elem && (bitmap & 1) >0 || !elem && (bitmap & 2) > 0
	
	override final def +(elem: Boolean): ValSet[Boolean] =
		new BooleanSet(bitmap | (if (elem) 1 else 2))
	
	override def -(elem: Boolean): ValSet[Boolean] =
		new BooleanSet(bitmap & (if (elem) 2 else 1))

	override def foreach[@specialized(Unit) U](f: (Boolean) => U) :Unit = bitmap match {
		case 1 => f(true)
		case 2 => f(false)
		case 3 => f(true); f(false);
		case _ => ()
	}

	override protected def reverseForeach(f: (Boolean) => Unit): Unit = bitmap match {
		case 1 => f(true)
		case 2 => f(false)
		case 3 => f(false); f(true)
		case _ => ()
	}

	override protected[this] def filter(p: (Boolean) => Boolean, ourTruth: Boolean): ValSet[Boolean] = new BooleanSet(
		(if ((bitmap & 1) > 0 && p(true)==ourTruth) 1 else 0) |
		(if ((bitmap & 2) > 0 && p(false)==ourTruth) 2 else 0)
	)

	override def map[@specialized(Fun1Vals) O, That](f: (Boolean) => O)(implicit bf: CanBuildFrom[ValSet[Boolean], O, That]) = {
		val b = bf(this)
		if ((bitmap & 1) > 0) b += f(true)
		if ((bitmap & 2) > 0) b += f(false)
		b.result()
	}

	override def flatMap[U, That](f: (Boolean) => GenTraversableOnce[U])(implicit bf: CanBuildFrom[ValSet[Boolean], U, That]) = {
		val b = bf(this)
		if ((bitmap & 1) > 0) b ++= f(true).seq
		if ((bitmap & 2) > 0) b ++= f(false).seq
		b.result()
	}

//	override def fitIterator: FitIterator[Boolean] = new BooleanSetIterator(bitmap)
	override def iterator: FitIterator[Boolean] = new BooleanSetIterator(bitmap)

	override def mutable = MutableSet.from(this)
	override def stable: Stable[Boolean] = ???

	override def newBuilder: FitBuilder[Boolean, BooleanSet] = new BooleanSetBuilder
	
	override def stringPrefix = "BooleanSet"
	
	override def toString = bitmap match {
		case 0 => "BooleanSet()"
		case 1 => "BooleanSet(true)"
		case 2 => "BooleanSet(false)"
		case 3 => "BooleanSet(true, false)"
	}
}



private[sets] object BooleanSet {
	def empty :StableOrderedSet[Boolean] = Empty

	@inline final def apply(value :Boolean) :StableOrderedSet[Boolean] =
		if(value) True else False

	@inline final def singleton(value :Boolean) :StableOrderedSet[Boolean] =
		if (value) True else False

	//todo:
	@inline final def newBuilder :FitBuilder[Boolean, StableOrderedSet[Boolean]] = //new BooleanSetBuilder
		new ImmutableSetBuilder[Boolean, StableOrderedSet[Boolean]](Empty)

	object Empty extends EmptyIterable[Boolean, StableOrderedSet[Boolean]] with StableOrderedSet[Boolean] {
		override def ordering = Ordering.Boolean

		override def empty = this
		override def contains(elem: Boolean): Boolean = false

		override def +(elem: Boolean): StableOrderedSet[Boolean] = if (elem) True else False

		override def -(elem: Boolean): StableOrderedSet[Boolean] = this

		override def mutable: MutableOrderedSet[Boolean] = ???

		override def keysIteratorFrom(start: Boolean): FitIterator[Boolean] = FitIterator.Empty

		override def rangeImpl(from: Option[Boolean], until: Option[Boolean]): StableOrderedSet[Boolean] = this

		protected override def verifiedCopyTo(xs: Array[Boolean], start: Int, total: Int) = 0
	}
	private class Singleton(value :Boolean)
		extends SingletonFoundation[Boolean, StableOrderedSet[Boolean]] with StableOrderedSet[Boolean]
				with SingletonSpecialization[Boolean, StableOrderedSet[Boolean]]
	{
		override def ordering = Ordering.Boolean

		override def empty = Empty
		override def head = value

		override def contains(elem: Boolean): Boolean = elem==head

		override def +(elem: Boolean): StableOrderedSet[Boolean] = if (elem==head) this else Both

		override def -(elem: Boolean): StableOrderedSet[Boolean] = if (elem==value) Empty else this

		override def mutable: MutableOrderedSet[Boolean] = ???

		override def keysIteratorFrom(start: Boolean): FitIterator[Boolean] =
			if (start && !value) FitIterator.empty else FitIterator(value)

		override def rangeImpl(from: Option[Boolean], until: Option[Boolean]): StableOrderedSet[Boolean] =
			if (until.isDefined)
				if (value || !until.get) Empty
				else if(from.isDefined && from.get) Empty
				else this
			else if (!value && from.isDefined && from.get) Empty
			else this

		protected override def verifiedCopyTo(xs: Array[Boolean], start: Int, total: Int) = {
			xs(start) = value; 1
		}
	}

	final val True :StableOrderedSet[Boolean] = new Singleton(true)
	final val False :StableOrderedSet[Boolean] = new Singleton(false)

	object Both extends DoubletonFoundation[Boolean, StableOrderedSet[Boolean]] with StableOrderedSet[Boolean] {
		override implicit def ordering: Ordering[Boolean] = Ordering.Boolean

		override def empty = Empty
		override def head = true
		override def last = false
		override def init = True
		override def tail = False

		override def contains(elem: Boolean): Boolean = true

		override def +(elem: Boolean): StableOrderedSet[Boolean] = this

		override def -(elem: Boolean): StableOrderedSet[Boolean] = if (elem) False else True


		override def mutable: MutableOrderedSet[Boolean] = ???


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

//	final val Empty = new BooleanSet(0)
	
//	@inline final def newBuilder :FitBuilder[Boolean, BooleanSet] = new BooleanSetBuilder
	
	private class BooleanSetIterator (private[this] var bitmap :Int)
		extends FastSizeIterator[Boolean] with FitIterator[Boolean]
	{
		override def hasNext = bitmap != 0
		override def size = bitmap - ((bitmap & 2) >> 1)
		
		def head :Boolean = (bitmap & 1) > 0
		
		override def skip(): Unit = bitmap = bitmap >> (bitmap & 1 ) << 31
		
		override def next() :Boolean = { //00 => Nil; 01 => true::Nil; 10 => false::Nil; 11 => true::false::Nil
			val res = bitmap & 1
			bitmap = bitmap >> res << 31
			res > 0
		}
	}
	
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
}