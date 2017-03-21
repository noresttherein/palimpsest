package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterator, ImplementationIterableFactory, IterableSpecialization, SpecializableIterable, Specialized}

import scala.collection.generic.CanBuildFrom
import scala.collection.{SortedSet, SortedSetLike, mutable}
import OrderedSet.{Mutable, Stable}
import net.turambar.palimpsest.specialty.ordered.{OrderedAs, OrderedVals}


trait OrderedSetTemplate[E, +This<:OrderedSetTemplate[E, This] with OrderedSet[E]]
	extends SortedSetLike[E, This] with OrderedAs[E, This] with SetSpecialization[E, This]
{
	//override me, I'm not specialized
	override def iteratorFrom(start: E) :FitIterator[E] = keysIteratorFrom(start)
}

/**
  * @author Marcin Mościcki
  */
trait OrderedSet[@specialized(Elements) E]
	extends SortedSet[E] with OrderedVals[E] with ValSet[E] with SetSpecialization[E, OrderedSet[E]] with OrderedAs[E, OrderedSet[E]] with OrderedSetTemplate[E, OrderedSet[E]]
{
//	override def compare(e1 :E, e2 :E) = ordering.compare(e1, e2) //todo: specialization


	//todo
	override def keyAt(idx: Int): E = drop(idx).head

	//todo
	override def reverseIterator: FitIterator[E] = inverse.iterator

	override def stable :Stable[E] //= (Stable.newBuilder[E] ++= this).result
	override def mutable :Mutable[E] = MutableSet.from(this)

	override def empty :OrderedSet[E] = OrderedSet.empty[E]

	@inline final override def iteratorFrom(start: E) = keysIteratorFrom(start)


	override def typeStringPrefix = "OrderedSet"
}



trait MutableOrderedSet[@specialized(Elements) E]
	extends MutableSet[E] with OrderedSet[E] with MutableSetLike[E, MutableOrderedSet[E]] with SortedSetLike[E, MutableOrderedSet[E]]
			with OrderedAs[E, MutableOrderedSet[E]] with SetSpecialization[E, MutableOrderedSet[E]] with FitBuilder[E, MutableOrderedSet[E]]
{
	override def mutable :Mutable[E] = this
	override def stable :Stable[E] = (Stable.newBuilder[E] ++= this).result
	override def empty :MutableOrderedSet[E] = OrderedSet.Mutable.empty[E]
	override def newBuilder :FitBuilder[E, MutableOrderedSet[E]] = empty //SortedFitSet.Mutable.newBuilder
}


trait StableOrderedSet[@specialized(Elements) E]
	extends SortedSetLike[E, StableOrderedSet[E]] with OrderedSet[E] with StableSet[E]
			with OrderedAs[E, StableOrderedSet[E]] with SetSpecialization[E, StableOrderedSet[E]]
{
	override def empty :StableOrderedSet[E] = OrderedSet.Stable.empty
	override def stable :Stable[E] = this


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

	def newBuilder[@specialized(Elements) E](implicit ord :Ordering[E]): FitBuilder[E, OrderedSet[E]] = ???

	def specializedBuilder[@specialized(Elements) E: Specialized :Ordering]: FitBuilder[E, OrderedSet[E]] = ???
}
