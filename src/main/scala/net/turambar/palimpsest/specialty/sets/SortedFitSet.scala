package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterator, ImplementationIterableFactory, IterableSpecialization, SpecializableIterable, Specialized}

import scala.collection.generic.CanBuildFrom
import scala.collection.{SortedSet, SortedSetLike, mutable}



/**
  * @author Marcin Mościcki
  */
trait SortedFitSet[@specialized(Elements) E]
	extends SortedSet[E] with SortedSetLike[E, SortedFitSet[E]] with FitSet[E] //with SpecializableSet[E, SortedFitSet]
		with SetSpecialization[E, SortedFitSet[E]]
//			with IterableSpecialization[E, SortedFitSet[E]] with SpecializableTraversableTemplate[E, SortedFitSet]
{
	override def compare(e1 :E, e2 :E) = ordering.compare(e1, e2) //todo: specialization

	override def stable :FitSet.Sorted.Stable[E] = this
	override def mutable :FitSet.Sorted.Mutable[E] = MutableSet.adapt(this)

	override def empty :SortedFitSet[E] = SortedFitSet.empty[E]

	override def keysIteratorFrom(start: E): FitIterator[E]

	@inline final override def firstKey = head

	@inline final override def lastKey = last

	override def from(from: E) = rangeImpl(Some(from), None)

	override def until(until: E) = rangeImpl(None, Some(until))

	override def range(from: E, until: E) = rangeImpl(Some(from), Some(until))

	@inline final override def iteratorFrom(start: E) = keysIteratorFrom(start)

//	override def companion = SortedFitSet

//	override def newBuilder = SortedFitSet.newBuilder[E]

	override def typeStringPrefix = "SortedSet"
}

trait MutableSortedSet[@specialized(Elements) E]
	extends MutableSet[E] with SortedFitSet[E] with mutable.SetLike[E, MutableSortedSet[E]] with SortedSetLike[E, MutableSortedSet[E]]
			with SetSpecialization[E, MutableSortedSet[E]] with FitBuilder[E, MutableSortedSet[E]]
{
	override def empty :MutableSortedSet[E] = SortedFitSet.Mutable.empty[E]
	override def newBuilder :FitBuilder[E, MutableSortedSet[E]] = empty //SortedFitSet.Mutable.newBuilder
}


object SortedFitSet  {
	type Stable[@specialized(Elements) E] = SortedFitSet[E]
	type Mutable[@specialized(Elements) E] = MutableSortedSet[E]
	//todo: wrap Ordering into CanFitFrom

	object Mutable {
		def empty[E] :Mutable[E] = MutableSet.Sorted.empty[E]
		def newBuilder[E] :FitBuilder[E, Mutable[E]] = MutableSet.Sorted.newBuilder[E]
	}

	@inline final implicit def canBuildFrom[E](implicit fit: CanFitFrom[SortedFitSet[_], E, SortedFitSet[E]]): CanBuildFrom[SortedFitSet[_], E, SortedFitSet[E]] =
		fit.cbf

	def apply[@specialized(Elements) E](elems :E*)(implicit ordering :Ordering[E]) :SortedFitSet[E] =
		(newBuilder[E] ++= elems).result()

	def empty[@specialized(Elements) E](implicit ord :Ordering[E]) :SortedFitSet[E] = newBuilder[E].result()

	def newBuilder[@specialized(Elements) E](implicit ord :Ordering[E]): FitBuilder[E, SortedFitSet[E]] = ???

	def specializedBuilder[@specialized(Elements) E: Specialized :Ordering]: FitBuilder[E, SortedFitSet[E]] = ???
}
