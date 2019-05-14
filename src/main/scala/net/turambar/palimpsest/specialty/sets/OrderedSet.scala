package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterator, RuntimeType}

import scala.collection.generic.CanBuildFrom
import scala.collection.{mutable, GenTraversableOnce, SortedSet, SortedSetLike}
import OrderedSet.{Mutable, Stable}
import net.turambar.palimpsest.specialty.ordered.{OrderedAs, OrderedVals}
import net.turambar.palimpsest.specialty.sets.ValSet.StableSetBuilder

/** A counterpart of `SortedSetLike`, it brings together the declarations from the latter and [[OrderedAs]]. As this
  * trait lacks specialization, its sole purpose is  to resolve conflicts from inheriting identical method declarations
  * from `OrderedAs` and `SortedSetLike`.
  *
  * '''Ugly''': this trait is not specialized but mixes in specialized [[SetSpecialization]]. In order not to split the synthetic
  * generic parent and specialized child of `SetSpecialization`, any descending class/trait must mix it in before this trait.
  * Fortunately, the convention is to always extend the collection class first: `extends OrderedSet[E] with OrderedSetTemplate[E, Repr]`
  * and it already includes the specialized interface in its linearization, so all is well in the end.
  */
trait OrderedSetTemplate[E, +This<:OrderedSetTemplate[E, This] with OrderedSet[E]]
	extends SortedSetLike[E, This] with OrderedAs[E, This] with SetSpecialization[E, This]
{
	//redeclarations to quiet conflicts from identical signatures in OrderedAs and SortedSetLike

	override def iteratorFrom(start: E) :FitIterator[E] = keysIteratorFrom(start)


	def +(elem :E) :This

	def +(elem1 :E, elem2 :E, elems :E*) :This

	def ++(elems :GenTraversableOnce[E]) :This

	def -(elem :E) :This

	def -(elem1 :E, elem2 :E, elems :E*) :This

	def --(elems :GenTraversableOnce[E]) :This

	override def contains(key :E) :Boolean

//	protected[this] override def newBuilder :FitBuilder[E, This] = super[SetSpecialization].newBuilder
}


//todo: make this not specialized. Needs not implement specialized methods and as long as specialized OrderedVals and ValSet
//todo: are mixed in before this trait, all should be well (but check it to make sure)
/**
  * @author Marcin Mościcki
  */
trait OrderedSet[@specialized(Elements) E] //todo: mix-in order of OrderedVals and ValSet
	extends SortedSet[E] with OrderedVals[E] with ValSet[E]
	   with SetSpecialization[E, OrderedSet[E]] with OrderedAs[E, OrderedSet[E]] with OrderedSetTemplate[E, OrderedSet[E]]
{
//	override def compare(e1 :E, e2 :E) = ordering.compare(e1, e2) //todo: specialization




	override def reverseIterator: FitIterator[E] = inverse.iterator

	override def stable :Stable[E] //= (Stable.newBuilder[E] ++= this).result
	override def mutable :Mutable[E] = MutableSet.from(this)

	override def empty :OrderedSet[E] = OrderedSet.empty[E]

	/** Overriden due to inheriting double declarations: from `SortedSetLike` and `OrderedAs`. */
	@inline final override def iteratorFrom(start: E) :FitIterator[E] = keysIteratorFrom(start)


	override def typeStringPrefix = "OrderedSet"

}





//todo: possibly doesn't need specialization
trait MutableOrderedSet[@specialized(Elements) E] extends OrderedSet[E] with MutableSet[E]
	with MutableSetSpecialization[E, MutableOrderedSet[E]] with OrderedSetTemplate[E, MutableOrderedSet[E]]
{
	override def mutable :Mutable[E] = this
	override def stable :Stable[E] = (Stable.newBuilder[E] ++= this).result()
	override def empty :MutableOrderedSet[E] = OrderedSet.Mutable.empty[E]
//	override def newBuilder :FitBuilder[E, MutableOrderedSet[E]] = empty //SortedFitSet.Mutable.newBuilder
}




//todo: possibly doesn't need specialization
trait StableOrderedSet[@specialized(Elements) E] extends OrderedSet[E] with StableSet[E]
	with SetSpecialization[E, StableOrderedSet[E]] with OrderedSetTemplate[E, StableOrderedSet[E]]
{
	override def empty :StableOrderedSet[E] = OrderedSet.Stable.empty
	override def stable :StableOrderedSet[E] = this

//	override protected[this] def newBuilder :FitBuilder[E, StableOrderedSet[E]] = new StableSetBuilder(empty)
}







object OrderedSet  {
	type Stable[@specialized(Elements) E] = StableOrderedSet[E]
	type Mutable[@specialized(Elements) E] = MutableOrderedSet[E]
	//todo: wrap Ordering into CanFitFrom

	object Mutable {
		def empty[@specialized(Elements) E :Ordering] :Mutable[E] = MutableSet.from(Stable.empty[E])//MutableSet.Sorted.empty[E]
		def newBuilder[@specialized(Elements) E :Ordering] :FitBuilder[E, Mutable[E]] = MutableSet.from(Stable.empty[E])
	}

	object Stable {
		def empty[@specialized(Elements) E :Ordering] :Stable[E] = ???
		def newBuilder[@specialized(Elements) E :Ordering] :FitBuilder[E, Stable[E]] = ???
	}

	@inline final implicit def canBuildFrom[E](implicit fit: CanFitFrom[OrderedSet[_], E, OrderedSet[E]]): CanBuildFrom[OrderedSet[_], E, OrderedSet[E]] =
		fit.cbf

	//todo: infinite loops here with empty set ++
	def apply[@specialized(Elements) E](elems :E*)(implicit ordering :Ordering[E]) :OrderedSet[E] =
		(newBuilder[E] ++= elems).result()

	//todo: infinite loops here with empty set ++
	def empty[@specialized(Elements) E](implicit ord :Ordering[E]) :OrderedSet[E] = newBuilder[E].result()

	def singleton[@specialized(Elements) E](elem :E)(implicit ord :Ordering[E]) :OrderedSet[E] =
		(newBuilder[E] += elem).result()

	def newBuilder[@specialized(Elements) E](implicit ord :Ordering[E]): FitBuilder[E, OrderedSet[E]] = ???

	def specializedBuilder[@specialized(Elements) E: RuntimeType :Ordering]: FitBuilder[E, OrderedSet[E]] = ???
}
