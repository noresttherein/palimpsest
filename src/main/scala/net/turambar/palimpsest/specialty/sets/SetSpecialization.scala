package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterables.EmptyIterableTemplate
import net.turambar.palimpsest.specialty.ordered.OrderedAs
import net.turambar.palimpsest.specialty.ordered.OrderedAs.EmptyOrderedTemplate
import net.turambar.palimpsest.specialty.sets.ValSet.ImmutableSetBuilder
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterator, FitTraversableOnce, IterableSpecialization, IterableTemplate, Specialized}

import scala.annotation.unspecialized
import scala.collection.generic.CanBuildFrom
import scala.collection.{GenSet, GenTraversableOnce, SetLike, mutable}

trait SetTemplate[E, +This <: ValSet[E] with SetSpecialization[E, This]]
	extends SetLike[E, This] with IterableTemplate[E, This] with mutable.Cloneable[This]
{
//	protected override def verifiedCopyTo(xs :Array[E], start :Int, total :Int) :Int =
//		if (isEmpty) 0
//		else {
//			iterator.copyToArray(xs, start, total); math.min(total, size)
//		}

	def ++(elems :FitTraversableOnce[E]) :This

	def --(elems :FitTraversableOnce[E]) :This

	def stable :ValSet.Stable[E]
	def mutable :ValSet.Mutable[E]

	override def clone() :This = repr

	protected[this] override def newBuilder :FitBuilder[E, This] = empty.newBuilder
}

/**
  * @author Marcin Mościcki
  */
trait SetSpecialization[@specialized(Elements) E, +This <: SetSpecialization[E, This] with ValSet[E]]
	extends SetTemplate[E, This] with IterableSpecialization[E, This]
//	extends SetLike[E, This] with IterableSpecialization[E, This] with mutable.Cloneable[This]
{

	override def specialization :Specialized[E] = mySpecialization

//	override def clone() :This = repr //empty ++ this //this should be in StableSetSpecialization, but we don't have such a trait, so instead we override it in MutableSetLike ...

	def stable :ValSet.Stable[E]
	def mutable :ValSet.Mutable[E]

	override def empty :This //= newBuilder.result()
	override def newBuilder :FitBuilder[E, This] = new ImmutableSetBuilder[E, This](empty)

	override def apply(elem: E) = contains(elem)
	override def contains(elem :E) :Boolean

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

	override def ++(elems :FitTraversableOnce[E]) :This =
		elems.foldLeft(repr)(_ + _)

	override def --(elems :FitTraversableOnce[E]) :This = {
		var res = repr; val it = elems.fitIterator
		while(it.hasNext && res.nonEmpty)
			res = res - it.next()
		res
	}

	override def +(elem :E) :This

	override def -(elem :E) :This



	@unspecialized
	def copyToFitArray(xs: Array[E], start: Int=0, total: Int=Int.MaxValue): Unit =
		if (start<0)
			throw new IllegalArgumentException(s"$stringPrefix.copyToArray([], $start, $total)")
		else {
			val max = math.min(xs.length-start, total)
			if (max > 0)
				verifiedCopyTo(xs, start, max)

		}

	protected override def verifiedCopyTo(xs :Array[E], start :Int, total :Int) :Int =
		if (isEmpty) 0
		else {
			iterator.copyToArray(xs, start, total); math.min(total, size)
		}


	//	override def sameElements[U >: E](that: GenIterable[U]) = super.sameElements(that)
	override def toSeq :Seq[E] = toFitSeq
}

object SetSpecialization {
	@inline final private[palimpsest] def friendCopy[E](set :ValSet[E], xs :Array[E], start :Int, total :Int) :Int =
		set.verifiedCopyTo(xs, start, total)
}



/**
  * @author Marcin Mościcki
  */
trait EmptySetTemplate[E, +T<:ValSet[E] with SetSpecialization[E, T]] extends EmptyIterableTemplate[E, T] { this :T =>
	override def ++[B >: E, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[T, B, That]) = super.++(that)

	override def empty :T = this

	override def --(xs: GenTraversableOnce[E]) :T = this

	override def --(elems: FitTraversableOnce[E]) :T = this

	@unspecialized
	override def copyToFitArray(xs: Array[E], start: Int, total: Int) :Unit = ()
}



trait EmptySetSpecialization[@specialized(Elements) E, +This <: StableSet[E] with SetSpecialization[E, This]]
	extends StableSet[E] with SetSpecialization[E, This] with EmptyIterableTemplate[E, This] //with OrderedAs[E, This] //with EmptyOrderedTemplate[E, This]
{
	@inline override final def repr :This = this.asInstanceOf[This]

	/** Overriden to override back implementation from [[IterableSpecialization]] to forgo the iterator. */
	override def head :E = throw new NoSuchElementException("empty " +typeStringPrefix)

	/** Overrides implementation from `scala.SetLike` back to fixed `true`. */
	override def isEmpty = true

	override def empty: This = repr

	override def clone() :This = repr


	override def span(p: (E) => Boolean) = (repr, repr)

	override def partition(p: (E) => Boolean) = (repr, repr)


	override def contains(elem: E): Boolean = false

	override def -(elem: E): This = repr

	override def -(elem1: E, elem2: E, elems: E*) :This = repr

	override def --(xs: GenTraversableOnce[E]) :This = repr

	override def --(elems: FitTraversableOnce[E]) :This = repr

	@unspecialized
	override def copyToFitArray(xs: Array[E], start: Int, total: Int) :Unit = ()

	protected override def verifiedCopyTo(xs :Array[E], start :Int, total :Int) :Int = 0


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
	override def diff(that: GenSet[E]) = super.diff(that)

	override def subsets(len: Int) = super.subsets(len)

	override def subsets() = super.subsets()

	override def intersect(that: GenSet[E]) = super.intersect(that)

	override def subsetOf(that: GenSet[E]) = super.subsetOf(that)
}