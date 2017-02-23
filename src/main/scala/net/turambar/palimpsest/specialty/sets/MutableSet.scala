package net.turambar.palimpsest.specialty.sets


import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.IterableAdapter
import net.turambar.palimpsest.specialty.sets.FitSet.{SetAdapter, Sorted}
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterator, FitTraversableOnce, ImplementationIterableFactory, IterableSpecialization, SpecializableIterable, Specialized}

import scala.collection.generic.{CanBuildFrom, Shrinkable}
import scala.collection.{GenSet, GenTraversableOnce, mutable}



trait MutableSetLike[E, +This <: MutableSet[E] with MutableSetLike[E, This]]
	extends mutable.SetLike[E, This] with SetSpecialization[E, This] with FitBuilder[E, This]
{
	override def clone() :This = (this :SetSpecialization[E, This]).empty ++= this

	override def newBuilder :FitBuilder[E, This] = (this :SetSpecialization[E, This]).empty

}

/**
  * @author Marcin Mościcki
  */
trait MutableSet[@specialized(Elements) E]
//	extends mutable.Set[E] with mutable.SetLike[E, MutableSet[E]] with FitSet[E] with FitBuilder[E, MutableSet[E]]
	extends mutable.Set[E] with mutable.SetLike[E, MutableSet[E]]
			with FitSet[E] with SpecializableSet[E, MutableSet] with MutableSetLike[E, MutableSet[E]] with FitBuilder[E, MutableSet[E]] //FitBuilder extended again for specialization
{

	override def add(elem: E) = { val r = contains(elem); this += elem; !r }

	override def remove(elem: E) = { val r = contains(elem); this -= elem; r }

	@inline final override def update(elem: E, included: Boolean) :Unit =
		if (included) this += elem else this -= elem

	override def +=(elem1: E, elem2: E, elems: E*) = { this += elem1 += elem2 ++= elems }

	override def -=(elem1: E, elem2: E, elems: E*) = { this -= elem1 -= elem2 --= elems }

	override def +=(elem: E): this.type

	override def -=(elem: E): this.type



//	override def retain(p: (E) => Boolean) = super.retain(p)

	override def mutable = this

	override def companion = MutableSet

//	override def newBuilder :FitBuilder[E, MutableSet[E]] = this

	override def typeStringPrefix = "MutableSet"
}




object MutableSet extends ImplementationIterableFactory[MutableSet] {
	type Sorted[@specialized(Elements) E] = MutableSortedSet[E]

	object Sorted {
		def empty[@specialized E] :Sorted[E] = new MutableSortedSetAdapter[E](SortedFitSet.Mutable.empty[E])
		def newBuilder[@specialized E] :FitBuilder[E, Sorted[E]] = empty[E]
	}

	@inline final override implicit def canBuildFrom[E](implicit fit: CanFitFrom[MutableSet[_], E, MutableSet[E]]): CanBuildFrom[MutableSet[_], E, MutableSet[E]] =
		fit.cbf

	private[sets] def adapt[@specialized(Elements) E](immutable :FitSet[E]) :MutableSet[E] =
		new MutableSetAdapter(immutable)

	private[sets] def adapt[@specialized(Elements) E](immutable :SortedFitSet[E]) :MutableSortedSet[E] =
		new MutableSortedSetAdapter(immutable)

	override def empty[@specialized(Elements) E] :MutableSet[E] = adapt(FitSet.empty[E])

	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, MutableSet[E]] = adapt(FitSet.empty[E])


	override def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, MutableSet[E]] =
		FitSet.specializedBuilder[E].mapResult(adapt[E](_))




	private[sets] abstract class AbstractMutableSetAdapter[
		+Source <: FitSet[E] with SetSpecialization[E, Source],
		E,
		+This<:MutableSet[E] with SetSpecialization[E, This] with mutable.SetLike[E, This] with FitBuilder[E, This]]
			(src :Source)
//		extends IterableAdapter[Source, E, This] with MutableSet[E] with SetSpecialization[E, This]//with Shrinkable[E] with FitSet[E] with SetSpecialization[E, This] with FitBuilder[E, This]
		extends SetAdapter[Source, E, This](src) with mutable.SetLike[E, This] with MutableSet[E] with SetSpecialization[E, This]
	{
		override def stable = source.stable


		override def ++=(xs: TraversableOnce[E]) :this.type = { source = source ++ xs; this }

		override def ++=(xs: FitTraversableOnce[E]) :this.type = { source = source ++ xs; this }

		override def --=(xs: TraversableOnce[E]) :this.type = { source = source -- xs; this }


		override def newBuilder :FitBuilder[E, This] = fromSource((source :SetSpecialization[E, Source]).empty)

		override def clone() = fromSource(source.clone())

		override def clear() :Unit = source = (source :SetSpecialization[E, Source]).empty

		override def stringPrefix = "Mutable"+source.stringPrefix


		override def count = source.size

		override def origin :Any = MutableSet


		//these could do with specialization
		override def contains(elem: E): Boolean = source.contains(elem)

		override def +(elem: E): This = fromSource(source + elem)

		override def -(elem: E): This = fromSource(source - elem)

	}



	private class MutableSetAdapter[@specialized(Elements) E](src :FitSet[E])
		extends AbstractMutableSetAdapter[FitSet[E], E, MutableSet[E]](src) with MutableSet[E] //for specialization
	{
		type Source = FitSet[E]
		type This = MutableSet[E]

		override protected[this] def fromSource(other: Source): This = new MutableSetAdapter(other)

		override def +=(elem: E): this.type = { source = source + elem; this }

		override def +=(elem1: E, elem2: E, elems: E*) :this.type  =
			{ source = source + (elem1, elem2, elems:_*); this }


		override def -=(elem: E): this.type = { source = source - elem; this }

		override def -=(elem1: E, elem2: E, elems: E*) :this.type = { source = source - (elem1, elem2, elems:_*); this }


		override def +(elem: E) :This = fromSource(source + elem)

		override def +(elem1: E, elem2: E, elems: E*) :This =
			fromSource(source + (elem1, elem2, elems:_*))


		override def -(elem: E) :This = fromSource(source - elem)

		override def -(elem1: E, elem2: E, elems: E*) :This  =
			fromSource(source - (elem1, elem2, elems:_*))


		override def contains(elem: E): Boolean = source.contains(elem)

		override def head = source.head
		override def last = source.last

	}



	private class MutableSortedSetAdapter[@specialized(Elements) E](src :FitSet.Sorted[E])
		extends AbstractMutableSetAdapter[FitSet.Sorted[E], E, Sorted[E]](src) with Sorted[E]
	{
		override protected[this] def fromSource(other: FitSet.Sorted[E]): Sorted[E] =
			new MutableSortedSetAdapter(other)

		override implicit def ordering: Ordering[E] = source.ordering

		override def stable :FitSet.Sorted.Stable[E] = source.stable
		override def mutable :FitSet.Sorted.Mutable[E] = fromSource(source)

		override def rangeImpl(from: Option[E], until: Option[E]): Sorted[E] =
			fromSource(source.rangeImpl(from, until))

		override def keysIteratorFrom(start: E): FitIterator[E] = source.keysIteratorFrom(start)


		override def +=(elem: E): this.type = { source += elem; this }

		override def +=(elem1: E, elem2: E, elems: E*) :this.type  =
			{ source = source + (elem1, elem2, elems:_*); this }


		override def -=(elem: E): this.type = { source -= elem; this }

		override def -=(elem1: E, elem2: E, elems: E*) :this.type =
			{ source = source - (elem1, elem2, elems:_*); this }

		override def +(elem: E) :Sorted[E] = fromSource(source + elem)

		override def +(elem1: E, elem2: E, elems: E*) :Sorted[E] =
			fromSource(source + (elem1, elem2, elems:_*))

		override def -(elem: E) :Sorted[E] = fromSource(source - elem)

		override def -(elem1: E, elem2: E, elems: E*) :Sorted[E]  =
			fromSource(source - (elem1, elem2, elems:_*))



		override def contains(elem: E): Boolean = source.contains(elem)

		override def head = source.head
		override def last = source.last

		override def origin = SortedFitSet //todo
	}

}


