package net.turambar.palimpsest.specialty.multisets

import java.util

import net.turambar.palimpsest.specialty
import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables._
import net.turambar.palimpsest.specialty.{ItemTypes, FitBuilder, FitTraversableOnce, RuntimeType}
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.{Fun2, Fun2Res}
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.seqs.{ArrayView, FitSeq, SharedArray, SharedArrayBuffer}
import net.turambar.palimpsest.specialty.sets.{MutableSet, MutableSetSpecialization, SetSpecialization, SpecializableSet, StableSet, ValSet}

import scala.annotation.unspecialized
import scala.collection.{GenIterable, GenSet, GenTraversableOnce, Set}
import scala.collection.generic.CanBuildFrom
import scala.compat.Platform

trait MultiSetSpecialization[@specialized(ItemTypes) E, +Repr<:MultiSetSpecialization[E, Repr]]
	extends IterableSpecialization[E, Repr] //with (E=>Boolean)
{


	override def stable :StableMultiSet[E] = StableMultiSet.empty ++ this
	def mutable :MutableMultiSet[E] = MutableMultiSet.empty ++= this
	def empty :Repr

	def unique :ValSet[E]
	def counts :FitIterable[(E, Int)]
//	override def toSet[B >: E]

	def uniqueItems :Int
	override def isEmpty :Boolean = size==0
	override def nonEmpty :Boolean = size > 0
	override def ofAtLeast(elems :Int) :Boolean = size >= elems

	def copiesOf(elem :E) :Int

	def has(elem :E) :Boolean = copiesOf(elem) > 0

	def hasAtLeast(elem :E, count :Int) :Boolean = copiesOf(elem) >= count

//	def contains(elem: E): Boolean = copiesOf(elem) > 0
//
//	/** Tests if some element is contained in this set.
//	  *
//	  *  This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
//	  *  @param elem the element to test for membership.
//	  *  @return  `true` if `elem` is contained in this set, `false` otherwise.
//	  */
//	def apply(elem: E): Boolean = this contains elem



//	def seq: MultiSet[E] //= this

	def +(elem: E): Repr

	def +(elem1 :E, elem2 :E, elems :E*) :Repr = this + elem1 + elem2 ++ elems

	def ++(elems :GenTraversableOnce[E]) :Repr = elems match {
		case fit :FitTraversableOnce[E] => this ++ elems
		case _ => (repr /: elems.seq)(_ + _)
	}

	def ++(elems :FitTraversableOnce[E]) :Repr = {
		val it = elems.toIterator
		var res = repr
		while (it.hasNext) res += it.next()
		res
	}



	def -(elem: E): Repr

	def -(elem1 :E, elem2 :E, elems :E*) :Repr = this - elem1 - elem2 -- elems

	def --(elems :GenTraversableOnce[E]) :Repr = elems match {
		case fit :FitTraversableOnce[E] => this -- elems
		case _ => (repr /: elems)(_ - _)
	}

	def --(elems :FitTraversableOnce[E]) :Repr = {
		val it = elems.toIterator
		var res = repr
		while (it.hasNext && res.nonEmpty) res -= it.next()
		res
	}



	def -*(elem :E) :Repr
	def -*(elem1 :E, elem2 :E, elems :E*) :Repr = this -* elem1 -* elem2 --* elems

	def --*(elems :GenTraversableOnce[E]) :Repr = elems match {
		case fit :FitTraversableOnce[E] => this --* fit
		case unfit => (repr /: elems){ (left, e) => left -* e }
	}

	def --*(elems :FitTraversableOnce[E]) :Repr = {
		val it = elems.toIterator
		var res = repr
		while(it.hasNext && res.nonEmpty) res -*= it.next()
		res
	}


	/** Computes the intersection between this set and another set.
	  *
	  *  @param   that  the set to intersect with.
	  *  @return  a new set consisting of all elements that are both in this
	  *  set and in the given set `that`.
	  */
	def intersect(that: GenSet[E]): Repr = this filter that

	@unspecialized
	def intersect(that :MultiSet[E]) :Repr = this filter that.has

	/** Computes the intersection between this set and another set.
	  *
	  *  '''Note:'''  Same as `intersect`.
	  *  @param   that  the set to intersect with.
	  *  @return  a new set consisting of all elements that are both in this
	  *  set and in the given set `that`.
	  */
	def &(that: GenSet[E]): Repr = this intersect that

	@unspecialized
	def &(that :MultiSet[E]) :Repr = this intersect that

	/** Computes the union between of set and another set.
	  *
	  *  @param   that  the set to form the union with.
	  *  @return  a new set consisting of all elements that are in this
	  *  set or in the given set `that`.
	  */
	def union(that: GenSet[E]): Repr = this ++ that

	@unspecialized
	def union(that :MultiSet[E]) :Repr = this ++ that

	/** Computes the union between this set and another set.
	  *
	  *  '''Note:'''  Same as `union`.
	  *  @param   that  the set to form the union with.
	  *  @return  a new set consisting of all elements that are in this
	  *  set or in the given set `that`.
	  */
	def | (that: GenSet[E]): Repr = this union that

	@unspecialized
	def |(that :MultiSet[E]) :Repr = this union that

	/** Computes the difference of this set and another set.
	  *
	  *  @param that the set of elements to exclude.
	  *  @return     a set containing those elements of this
	  *              set that are not also contained in the given set `that`.
	  */
	def diff(that: GenSet[E]): Repr = this -- that

	@unspecialized
 	def diff(that :MultiSet[E]) :Repr = this -- that

	/** The difference of this set and another set.
	  *
	  *  '''Note:'''  Same as `diff`.
	  *  @param that the set of elements to exclude.
	  *  @return     a set containing those elements of this
	  *              set that are not also contained in the given set `that`.
	  */
	def &~(that: GenSet[E]): Repr = this diff that

	@unspecialized
	def &~(that :MultiSet[E]) :Repr = this diff that

	/** Tests whether this set is a subset of another set.
	  *
	  *  @param that  the set to test.
	  *  @return     `true` if this set is a subset of `that`, i.e. if
	  *              every element of this set is also an element of `that`.
	  */
	def subsetOf(that: GenSet[E]): Boolean =
		size == uniqueItems && (this forall that)

	def subsetOf(that :MultiSet[E]) :Boolean = forall(that.hasAtLeast _)




	def forall(p :(E, Int) => Boolean) :Boolean

	
}

/**
  * @author Marcin Mościcki
  */
trait MultiSet[@specialized(ItemTypes) E]
	extends FitIterable[E] with SpecializableIterable[E, MultiSet] with MultiSetSpecialization[E, MultiSet[E]]
	   with CloneableIterable[E, MultiSet[E]]
{
//	override def seq :MultiSet[E] = this
	override def empty :MultiSet[E] = StableMultiSet.empty[E]
//	override def stable :StableMultiSet[E]
//	override def mutable :MutableMultiSet[E]

	override def companion :FitCompanion[MultiSet] = MultiSet
}




object MultiSet extends InterfaceIterableFactory[MultiSet] {
	override protected[this] type RealType[@specialized(ItemTypes) E] = StableMultiSet[E]

	override protected[this] def default: FitIterableFactory[RealType] = StableMultiSet

	@inline final override implicit def canBuildFrom[E](implicit fit: CanFitFrom[MultiSet[_], E, MultiSet[E]]): CanBuildFrom[MultiSet[_], E, MultiSet[E]] =
		fit.cbf

	@inline final implicit def multiplicities[@specialized(ItemTypes) E](item :E) :StableMultiSet.Singleton[E] =
		new StableMultiSet.Singleton[E](item)
}




trait StableMultiSet[@specialized(ItemTypes) E]
	extends MultiSet[E] with SpecializableIterable[E, StableMultiSet] with MultiSetSpecialization[E, StableMultiSet[E]]
	   with StableIterable[E] with StableIterableTemplate[E, StableMultiSet[E]] with CloneableIterable[E, StableMultiSet[E]]
{

	override def empty :StableMultiSet[E] = StableMultiSet.empty[E]
	override def stable :StableMultiSet[E] = this
	override def mutable :MutableMultiSet[E] = ???

	override def companion :FitCompanion[StableMultiSet] = StableMultiSet


}

object StableMultiSet extends InterfaceIterableFactory[StableMultiSet] {
	override protected[this] type RealType[@specialized(ItemTypes) E] = StableMultiSet[E]

	override protected[this] def default: FitIterableFactory[StableMultiSet] = ???

	@inline final override implicit def canBuildFrom[E](implicit fit: CanFitFrom[StableMultiSet[_], E, StableMultiSet[E]]): CanBuildFrom[StableMultiSet[_], E, StableMultiSet[E]] =
		fit.cbf

	@inline final def single[@specialized(ItemTypes) E](item :E) :Singleton[E] = new Singleton(item)

	class Singleton[@specialized(ItemTypes) E](item :E)
		extends SingletonFoundation[E, StableMultiSet[E]] with StableMultiSet[E] with SingletonSpecialization[E, StableMultiSet[E]]
	{
		@inline final override def head :E = item
		override def uniqueItems: Int = 1

		override def unique: StableSet[E] = StableSet.one(item)

		override def counts: FitIterable[(E, Int)] = FitSeq.one((item, 1))

		override def copiesOf(elem: E): Int = if (elem==item) 1 else 0

		override def +(elem: E): StableMultiSet[E] = ???

		override def -(elem: E): StableMultiSet[E] = if (elem==item) StableMultiSet.empty[E] else this

		@unspecialized @inline
		final override def -*(elem: E): StableMultiSet[E] = this - elem

		override def forall(p: (E, Int) => Boolean): Boolean = p(item, 1)

		def :*(copies :Int) :StableMultiSet[E] =
			if (copies<=0) StableMultiSet.empty[E] else new MultiSet1(item, copies)

		@unspecialized @inline
		final def *:(copies :Int) :StableMultiSet[E] = this :* copies
	}



	private class MultiSet1[@specialized(ItemTypes) E](item :E, copies :Int) //extends IterableFoundation[E, StableMultiSet[E]] with StableMultiSet[E] {
		extends SingletonFoundation[E, StableMultiSet[E]] with StableMultiSet[E] with SingletonSpecialization[E, StableMultiSet[E]]
	{
		override def head :E = item
		override def last :E = item
//		override def headOption = Some(item)
//		override def lastOption = Some(item)

		override def size :Int = copies
		override def ofAtLeast(n :Int) :Boolean = n <= copies
//		override def isEmpty :Boolean = size==0
//		override def nonEmpty :Boolean = size!=0
		override def uniqueItems = 1
//		override def hasFastSize = true

		override def copiesOf(elem: E): Int = if (elem==item) copies else 0

		override def has(elem: E): Boolean = elem == item

		override def -(elem: E): StableMultiSet[E] =
			if (elem==item)
				if (copies==1) StableMultiSet.empty[E] else new MultiSet1(item, copies-1)
			else
				this

		override def -(elem1: E, elem2: E, elems: E*) :StableMultiSet[E] = {
			var remove = 0
			if (elem1==item)
				if (copies==1) return StableMultiSet.empty[E]
				else remove = 1
			if (elem2==item)
				if (copies <= remove+1) return StableMultiSet.empty[E]
				else remove  += 1
			elems foreach { e :E =>
				if (e==item) {
					remove += 1
					if (remove>=copies) return StableMultiSet.empty[E]
				}
			}
			if (remove==0) this else new MultiSet1(item, copies-remove)
		}


		override def --(xs: GenTraversableOnce[E]) :StableMultiSet[E] = xs match {
			case fit :FitTraversableOnce[E] =>
				this -- fit
			case _ =>
				var count = 0
				xs foreach { e :E =>
					if (e==item) {
						count += 1
						if (count>=copies) return StableMultiSet.empty[E]
					}
				}
				if (count==0) this else new MultiSet1(item, copies-count)
		}


		override def --(elems: FitTraversableOnce[E]) :StableMultiSet[E] = {
			var left = copies
			val it = elems.toIterator
			while(it.hasNext)
				if (it.next()==item) {
					left -= 1
					if (left==0) return StableMultiSet.empty[E]
				}
			if (left==copies) this else new MultiSet1(item, left)
		}

		override def -*(elem: E): StableMultiSet[E] = if (elem==item) StableMultiSet.empty[E] else this

		override def --*(elems: FitTraversableOnce[E]): StableMultiSet[E] = {
			val it = elems.toIterator
			while(it.hasNext)
				if (it.next()==item) return StableMultiSet.empty
			this
		}

		override def -*(elem1: E, elem2: E, elems: E*) :StableMultiSet[E] = {
			if (elem1 != item)
				if (elem2 != item)
					if (!elems.contains(item))
						return this
			StableMultiSet.empty[E]
		}

		override def --*(elems: GenTraversableOnce[E]) :StableMultiSet[E] = elems match {
			case fit :FitTraversableOnce[E] => this --* fit
			case _ => if (elems.exists(item == _)) StableMultiSet.empty[E] else this
		}

		override def +(elem: E): StableMultiSet[E] = ???




		override def iterator: FitIterator[E] = FitIterator.repeated[E](item, size)

		override def foreach[@specialized(Unit) O](f :E=>O) :Unit = {
			var i = copies
			while(i > 0) { f(item); i-=1 }
		}

		@unspecialized
		override protected def reverseForeach(f: (E) => Unit): Unit = foreach(f)



		override def forall(p :(E, Int) => Boolean) = p(item, copies)
		@unspecialized
		override def forall(p: E => Boolean) :Boolean = exists(p)
		override def exists(p: E => Boolean) :Boolean = p(item)
		override def find(p: E => Boolean) :Option[E] = if (p(item)) Some(item) else None
		override def count(p: E => Boolean) :Int = if (p(item)) copies else 0

		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, E) => O) :O = {
			var res = z; var left = copies
			do {
				res = op(res, item)
				left -= 1
			} while(left>=0)
			res
		}
		override def foldRight[@specialized(Fun2) O](z: O)(op: (E, O) => O) :O = {
			var res = z; var left = copies
			do {
				res = op(item, res)
				left -= 1
			} while(left>=0)
			res
		}

		override def tail :StableMultiSet[E] = if (copies==1) StableMultiSet.empty[E] else new MultiSet1(item, copies-1)
		@unspecialized
		override def init :StableMultiSet[E] = tail


//		override protected[this] def dropTake(from: Int, until: Int) =
//			if (from>=copies || until<=from) StableMultiSet.empty[E]
//			else if (until>=copies)
//				if (from==0) this else new MultiSet1(item, copies-from)
//			else new MultiSet1(item, until-from)

		override def takeRight(n: Int) :StableMultiSet[E] =
			if (n<=0) StableMultiSet.empty[E]
			else if (n>=copies) this
			else new MultiSet1(item, n)

		override def dropRight(n: Int) :StableMultiSet[E] =
			if (n<=0) this
			else if (n>=copies) StableMultiSet.empty[E]
			else new MultiSet1(item, copies-n)

		override def splitAt(n: Int) :(StableMultiSet[E], StableMultiSet[E]) =
			if (n<=0) (empty, this)
			else if (n>=copies) (this, empty)
			else (new MultiSet1(item, n), new MultiSet1(item, copies-n))



		override def takeWhile(p: E => Boolean) :StableMultiSet[E] = if (p(item)) this else StableMultiSet.empty[E]

		override def dropWhile(p: E => Boolean) :StableMultiSet[E] = if (p(item)) StableMultiSet.empty[E] else this

		override def span(p: E => Boolean) :(StableMultiSet[E], StableMultiSet[E]) =
			if (p(item)) (this, StableMultiSet.empty[E]) else (StableMultiSet.empty[E], this)

		@unspecialized
		override def partition(p: E => Boolean) :(StableMultiSet[E], StableMultiSet[E]) = span(p)


		override def filter(p: E => Boolean, ourTruth: Boolean): StableMultiSet[E] =
			if (p(item) == ourTruth) this else StableMultiSet.empty[E]

		override def filter(p: E => Boolean) :StableMultiSet[E] = if (p(item)) this else StableMultiSet.empty[E]

		override def filterNot(p: E => Boolean) :StableMultiSet[E] = if (p(item)) StableMultiSet.empty[E] else this



		protected override def trustedCopyTo(xs: Array[E], start: Int, total: Int) :Int = {
			val count = math.min(copies, total)
			specialty.arrayFill(xs, item, start, count)
			count
		}


		override def unique :StableSet[E] = StableSet.one(item)

		override def counts :FitSeq[(E, Int)] = FitSeq.one((item, copies))

		override def inverse :MultiSet1[E] = this

		override def toSet[B >: E] :StableSet[B] = StableSet.one(item)

		override def toFitSeq :SharedArray[E] = {
			val a = specialization.newArray(copies).asInstanceOf[Array[E]]
			specialty.arrayFill(a, item)
			SharedArray(a)
		}
//		override def toFitBuffer[B >: E] = SharedArrayBuffer.of(copies, item)
//		override def toFitBuffer[U >: E](spec :Specialized[U]) = SharedArrayBuffer.of(copies, item)(spec.classTag)
	}


}


trait MutableMultiSet[@specialized(ItemTypes) E]
	extends MultiSet[E] with SpecializableIterable[E, MutableMultiSet] with MultiSetSpecialization[E, MutableMultiSet[E]]
	   with FitBuilder[E, MultiSet[E]] with MutableIterable[E] with CloneableIterable[E, MutableMultiSet[E]]
{
	override def empty :MutableMultiSet[E] = MutableMultiSet.empty[E]
	override def stable :StableMultiSet[E] = ???
	override def mutable :MutableMultiSet[E] = this

	override def companion :FitCompanion[MutableMultiSet] = MutableMultiSet
}

object MutableMultiSet extends InterfaceIterableFactory[MutableMultiSet] {
	override protected[this] type RealType[@specialized(ItemTypes) E] = MutableMultiSet[E]

	override protected[this] def default: FitIterableFactory[MutableMultiSet] = ???

	@inline final override implicit def canBuildFrom[E](implicit fit: CanFitFrom[MutableMultiSet[_], E, MutableMultiSet[E]]): CanBuildFrom[MutableMultiSet[_], E, MutableMultiSet[E]] =
		fit.cbf
}