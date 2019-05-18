package net.turambar.palimpsest.specialty.sets


import java.lang.Math

import net.turambar.palimpsest.specialty.iterables.{CloneableIterable, IterableSpecialization, IterableTemplate}
import net.turambar.palimpsest.specialty.sets.ValSet.StableSetBuilder
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitTraversableOnce, RuntimeType}

import scala.annotation.unspecialized
import scala.collection.{mutable, GenSet, GenTraversableOnce, SetLike}

/** A generic, non-specialized analogue of `SetLike` serving as the common base class for all sets in this package.
  * Provides declarations of common methods not needing public specialized variants. Subclasses may still
  * provide specialized implementations, but they will generally be specialized only for the single type specific to
  * a given implementation (i.e. an `IntSet` will implement the methods for `Int` elements and doesn't need to clutter
  * the class with declarations of variants for other element types).
  * Compare this with specialized declarations and implementations in [[SetSpecialization]], which create variants for all
  * specializable element types which can be called directly for every [[ValSet]], no matter its element type.
  * @tparam E element type of this set
  * @tparam This complete static and public type of this set, including its element type.
  * @see [[SetSpecialization]]
  */
trait SetTemplate[E, +This <: ValSet[E] with SetSpecialization[E, This]]
	extends SetLike[E, This] with IterableTemplate[E, This] //with mutable.Cloneable[This]
{

	def ++(elems :FitTraversableOnce[E]) :This

	def --(elems :FitTraversableOnce[E]) :This

	/** Symmetric difference between this set and the argument. Symmetric difference is the set which contains
	  * an element ''iff'' it is present in exactly one of the two sets.
	  * @return a set equal to `(this ++ that) -- (this & that)`.
	  */
	def ^(that :GenSet[E]) :This

	/** Symmetric difference between this set and the argument. Symmetric difference is the set which contains
	  * an element ''iff'' it is present in exactly one of the two sets.
	  * @return a set equal to `(this ++ that) -- (this & that)`.
	  */
	def ^(that :ValSet[E]) :This


	/** An immutable set for the same element type and specialization as this set and containing the same elements.
	  * @return `this` if this is an immutable set or a related, specialized snapshot for mutable sets.
	  */
	override def stable :StableSet[E] = StableSet.of[E] ++ this

	/** A mutable version of this set. For immutable sets this creates a new instance of a related class
	  * initialized with elements of this set. For mutable sets, this will generally simply return `this` -
	  * changes to either `this` or the returned set will be visible in the other!
	  */
	def mutable :MutableSet[E]

	//todo:
	def mutate :MutableSet[E] = mutable


	/** Default set builder delegating to a wrapped set's `+` and `++` methods. This implementation is ''not'' specialized
	  * and must be overriden.
	  */
	protected[this] override def newBuilder :FitBuilder[E, This] = new StableSetBuilder[E, This](empty)

}



/** A specialized base class for sets providing default specialized implementation for common methods.
  * It is a specialized analogue of `SetLike`.
  * @author Marcin Mościcki
  */
trait SetSpecialization[@specialized(Elements) E, +This <: SetSpecialization[E, This] with ValSet[E]]
	extends SetTemplate[E, This] with IterableSpecialization[E, This] with CloneableIterable[E, This]
{

	/** Runtime type used to store elements of this collection. Overriden to provide non-abstract type parameter
	  * possible due to invariance. This method remains not specialized in order to avoid an additional intermediate
	  * call in the most common case of invoking it from non-specialized code. Use the specialized target of this method
	  * [[net.turambar.palimpsest.specialty.sets.SetSpecialization#specialization]] when `E` is known or specialized.
	  */
	@inline @unspecialized
	override final def runtimeType :RuntimeType[E] = specialization

	/** Runtime type used to store elements of this collection. The difference from
	  * [[net.turambar.palimpsest.specialty.sets.SetSpecialization#specialization]] is that this method is specialized,
	  * while the former simply delegates to this instance.
	  */
	override def specialization :RuntimeType[E] = RuntimeType.specialized[E]

	override def empty :This //consider: making it non-specialized on this level (that is, simply removing it)


	override def apply(elem: E) :Boolean = contains(elem)

	override def contains(elem :E) :Boolean


	override def +(elem :E) :This

	override def -(elem :E) :This

	def ^(elem :E) :This = if (contains(elem)) this - elem else this + elem

	override def +(elem1: E, elem2: E, elems: E*) :This = this + elem1 + elem2 ++ elems

	override def -(elem1: E, elem2: E, elems: E*) :This = this - elem1 - elem2 -- elems


	override def ++(elems: GenTraversableOnce[E]) :This = elems match {
		case fit :FitTraversableOnce[E] => this ++ fit
		case _ => (repr /: elems)(_ + _)
	}

	override def --(xs: GenTraversableOnce[E]) :This = xs match {
		case fit :FitTraversableOnce[E] => this -- fit
		case _ => (repr /: xs)(_ - _)
	}

	override def ++(elems :FitTraversableOnce[E]) :This = {
		var res = repr; val it = elems.toIterator
		while (it.hasNext)
			res = res + it.next()
		res
	}

	override def --(elems :FitTraversableOnce[E]) :This = {
		var res = repr; val it = elems.toIterator
		while(it.hasNext && res.nonEmpty)
			res = res - it.next()
		res
	}

	override def ^(that :GenSet[E]) :This = that match {
		case set :ValSet[E] => this ^ set
		case _ if that.isEmpty => carbon
		case _ => (repr /: that){ _ ^ _ }
	}

	override def ^(that :ValSet[E]) :This =
		if (that.isEmpty)
			carbon //to make sure we won't return this in case we are mutable
		else {
			var res = repr //don't use fold as it will box the element type
			val it = that.iterator
			while (it.hasNext) {
				val e = it.next()
				if (contains(e)) res -= e else res += e
			}
			res
		}

	override def intersect(that :GenSet[E]) :This = that match {
		case set :ValSet[E] => &(set)
		case _ if that.isEmpty => empty
		case _ => filter(that)
	}

	def &(elems :ValSet[E]) :This =
		if (isEmpty || elems.isEmpty)
			empty
		else if (elems.hasFastSize && hasFastSize && elems.size < size) {
			SetSpecialization.intersection(elems, repr, newBuilder)
		} else {
			SetSpecialization.intersection(repr, elems, newBuilder)
		}


	override def subsetOf(that :GenSet[E]) :Boolean = that match {
		case vals :ValSet[E] => subsetOf(vals)
		case _ => forall(that)
	}

	def subsetOf(that :ValSet[E]) :Boolean =
		if (hasFastSize && that.hasFastSize && size > that.size)
			false
		else {
			val it = iterator
			while (it.hasNext)
				if (!that.contains(it.next())) return false
			true
		}

//	@unspecialized
//	def copyToFitArray(xs: Array[E], start: Int=0, total: Int=Int.MaxValue): Unit =
//		if (start<0)
//			throw new IllegalArgumentException(s"$stringPrefix.copyToArray([], $start, $total)")
//		else {
//			val max = Math.min(xs.length-start, total)
//			if (max > 0)
//				trustedCopyTo(xs, start, max)
//		}


	protected override def trustedCopyTo(xs :Array[E], start :Int, total :Int) :Int =
		if (isEmpty) 0
		else {
			iterator.copyToArray(xs, start, total); Math.min(total, size)
		}

//	override def clone() :This = repr //empty ++ this //this should be in StableSetSpecialization, but we don't have such a trait, so instead we override it in MutableSetSpecialization ...

//	def stable :StableSet[E]
//	def mutable :MutableSet[E]

 //= newBuilder.result()
	/** Default set builder delegating to a wrapped set's `+` and `++` methods. Good for most immutable sets,
	  * overriden by [[MutableSetSpecialization]] for mutable sets.
	  */
	override def newBuilder :FitBuilder[E, This] = new StableSetBuilder[E, This](empty)


	//	override def sameElements[U >: E](that: GenIterable[U]) = super.sameElements(that)
//	override def toSeq :Seq[E] = toFitSeq
}




object SetSpecialization {
//	@inline final private[palimpsest] def friendCopy[E](set :ValSet[E], xs :Array[E], start :Int, total :Int) :Int =
//		set.trustedCopyTo(xs, start, total)


	private[sets] def intersection[@specialized(Elements) E, R](set1 :ValSet[E], set2 :ValSet[E], builder :FitBuilder[E, R]) :R = {
		val it = set1.iterator
		while (it.hasNext) {
			val e = it.next()
			if (set2.contains(e))
				builder += e
		}
		builder.result()
	}


	private[sets] def buildDifference[@specialized(Elements) E, R](set1 :ValSet[E], set2 :ValSet[E], builder :FitBuilder[E, R]) :Unit = {
		val it = set1.iterator
		while (it.hasNext) {
			val e = it.next()
			if (!set2.contains(e))
				builder += e
		}
	}


}







/*

/**
  * @author Marcin Mościcki
  */
trait EmptySetTemplate[E, +T<:ValSet[E] with SetSpecialization[E, T]] extends EmptyIterableTemplate[E, T] { this :T =>

	override def empty :T = this

	override def ++[B >: E, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[T, B, That]) :That = super.++(that)

	override def --(xs: GenTraversableOnce[E]) :T = this

	override def --(elems: FitTraversableOnce[E]) :T = this

	@unspecialized
	override def copyToFitArray(xs: Array[E], start: Int, total: Int) :Unit = ()
}



@deprecated
trait EmptySetSpecialization[@specialized(Elements) E, +This <: StableSet[E] with SetSpecialization[E, This]]
	extends StableSet[E] with SetSpecialization[E, This] with EmptyIterableTemplate[E, This] //with OrderedAs[E, This] //with OrderedEmpty[E, This]
{
	@inline override final def repr :This = this.asInstanceOf[This]

	/** Overriden to override back implementation from [[IterableSpecialization]] to forgo the iterator. */
	override def head :E = throw new NoSuchElementException("empty " +typeStringPrefix)

	/** Overrides implementation from `scala.SetLike` back to fixed `true`. */
	override def isEmpty = true

	override def empty: This = repr

	override def clone() :This = repr


	override def span(p: E => Boolean) :(This, This) = (repr, repr)

	override def partition(p: E => Boolean) :(This, This) = (repr, repr)






	override def contains(elem: E): Boolean = false


	override def -(elem: E): This = repr

	override def -(elem1: E, elem2: E, elems: E*) :This = repr

	override def --(xs: GenTraversableOnce[E]) :This = repr

	override def --(elems: FitTraversableOnce[E]) :This = repr

	override def &(that :ValSet[E]) :This = repr

	override def diff(that: GenSet[E]) :This = repr

	override def subsets(len: Int) :Iterator[This] = if (len==0) Iterator.single(repr) else Iterator.empty

	override def subsets() :Iterator[This] = Iterator.single(repr)

	override def intersect(that: GenSet[E]) :This = repr



	override def subsetOf(that: GenSet[E]) :Boolean = true

	override def equals(that :Any) :Boolean = that match {
		case set :GenSet[_] => set.isEmpty
		case _ => false
	}



	@unspecialized
	override def copyToFitArray(xs: Array[E], start: Int, total: Int) :Unit = ()

	protected override def trustedCopyTo(xs :Array[E], start :Int, total :Int) :Int = 0




	/*
		override def from(from: E) :This = repr
		override def until(until: E) :This = repr
		override def range(from: E, until: E) :This = repr
		override def to(to: E) :This = repr
		override def rangeImpl(from: Option[E], until: Option[E]): This = repr

		override def keyAt(n: Int): E = throw new IndexOutOfBoundsException(s"empty $stringPrefix: rank($n)")

		@unspecialized
		override def reverseKeyIterator: FitIterator[E] = FitIterator.empty

		@unspecialized
		override def keysIteratorFrom(start: E): FitIterator[E] = FitIterator.empty
	*/
}
*/

