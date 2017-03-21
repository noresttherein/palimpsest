package net.turambar.palimpsest.specialty.sets


import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.IterableAdapter
import net.turambar.palimpsest.specialty.sets.ValSet.{SetAdapter, Sorted}
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterator, FitTraversableOnce, ImplementationIterableFactory, IterableSpecialization, SpecializableIterable, Specialized}

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, Shrinkable}
import scala.collection.{GenSet, GenTraversableOnce, mutable}



trait MutableSetLike[E, +This <: MutableSet[E] with MutableSetLike[E, This] with SetSpecialization[E, This]]
	extends mutable.SetLike[E, This] with SetTemplate[E, This]/*with SetSpecialization[E, This]*/ with FitBuilder[E, This]
{ self :SetSpecialization[E, This] =>

	//todo: specialize these
	override def +(elem1: E, elem2: E, elems: E*) :This = clone() += elem1 += elem2 ++= elems

	override def -(elem1: E, elem2: E, elems: E*) :This = clone() -= elem1 -= elem2 --= elems

	override def ++(xs: GenTraversableOnce[E]) :This = clone() ++= xs.seq

	override def --(xs: GenTraversableOnce[E]) :This = clone() --= xs.seq

	override def ++(elems: FitTraversableOnce[E]) :This = clone() ++= elems

	override def --(elems: FitTraversableOnce[E]) :This = clone() --= elems


	override def clone() :This = (this :SetSpecialization[E, This]).empty ++= this

	override def newBuilder :FitBuilder[E, This] = (this :SetSpecialization[E, This]).empty

}

/**
  * @author Marcin Mościcki
  */
trait MutableSet[@specialized(Elements) E]
	extends mutable.Set[E] with mutable.SetLike[E, MutableSet[E]]
			with ValSet[E] with FitBuilder[E, MutableSet[E]] with SpecializableSet[E, MutableSet] with MutableSetLike[E, MutableSet[E]] //FitBuilder extended again for specialization
{

	override def add(elem: E) = { val r = contains(elem); this += elem; !r }

	override def remove(elem: E) = { val r = contains(elem); this -= elem; r }

	@inline final override def update(elem: E, included: Boolean) :Unit =
		if (included) this += elem else this -= elem

	override def +=(elem1: E, elem2: E, elems: E*) = { this += elem1 += elem2 ++= elems }

	override def -=(elem1: E, elem2: E, elems: E*) = { this -= elem1 -= elem2 --= elems }

	override def +=(elem: E): this.type

	override def -=(elem: E): this.type


	override def --=(xs: TraversableOnce[E]) :this.type = xs match {
		case vals :FitTraversableOnce[E] => this --= vals
		case _ => xs foreach -=; this
	}

	def --=(xs :FitTraversableOnce[E]) :this.type = {
		val it = xs.fitIterator
		while (it.hasNext && nonEmpty) this -= it.next()
		this
	}



	//	override def retain(p: (E) => Boolean) = super.retain(p)

	@unspecialized
	override def mutable :MutableSet[E] = this

	override def stable :StableSet[E] = StableSet.empty[E] ++ this

	override def companion = MutableSet

	override def typeStringPrefix = "MutableSet"
}




object MutableSet extends ImplementationIterableFactory[MutableSet] {
	type Ordered[@specialized(Elements) E] = MutableOrderedSet[E]

	final val Ordered = OrderedSet.Mutable
//	object Ordered {
//		def empty[@specialized E] :Sorted[E] = new MutableSortedSetAdapter[E](OrderedSet.Mutable.empty[E])
//		def newBuilder[@specialized E] :FitBuilder[E, MutableOrderedSet[E]] = empty[E]
//	}

	@inline final override implicit def canBuildFrom[E](implicit fit: CanFitFrom[MutableSet[_], E, MutableSet[E]]): CanBuildFrom[MutableSet[_], E, MutableSet[E]] =
		fit.cbf


	def from[@specialized(Elements) E](immutable :ValSet[E]) :MutableSet[E] =
		new MutableSetAdapter(immutable)

	def from[@specialized(Elements) E](immutable :OrderedSet[E]) :MutableOrderedSet[E] =
		new MutableSortedSetAdapter(immutable)


	override def empty[@specialized(Elements) E] :MutableSet[E] = from(ValSet.empty[E])

	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, MutableSet[E]] = from(ValSet.empty[E])


	override def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, MutableSet[E]] =
		ValSet.specializedBuilder[E].mapResult(from[E](_))




	private[sets] abstract class MutableSetAdapterFoundation[
				+Source <: ValSet[E] with SetSpecialization[E, Source],
				E,
				+This <: MutableSet[E] with SetSpecialization[E, This] with MutableSetLike[E, This] with FitBuilder[E, This]
			](src :Source)
		extends SetAdapter[Source, E, This](src) with MutableSetLike[E, This] //with MutableSet[E] with SetSpecialization[E, This]
	{ this :MutableSet[E] with SetSpecialization[E, This] =>
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



	private class MutableSetAdapter[@specialized(Elements) E](src :ValSet[E])
		extends MutableSetAdapterFoundation[ValSet[E], E, MutableSet[E]](src) with MutableSet[E] //for specialization
	{
		type Source = ValSet[E]
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

		override def newBuilder :FitBuilder[E, This] = fromSource((source :SetSpecialization[E, Source]).empty)
	}



	private class MutableSortedSetAdapter[@specialized(Elements) E](src :ValSet.Sorted[E])
		extends MutableSetAdapterFoundation[ValSet.Sorted[E], E, MutableOrderedSet[E]](src) with MutableOrderedSet[E]
	{
		override protected[this] def fromSource(other: ValSet.Sorted[E]): MutableOrderedSet[E] =
			new MutableSortedSetAdapter(other)

		override implicit def ordering: Ordering[E] = source.ordering

		override def stable :ValSet.Sorted.Stable[E] = source.stable
		override def mutable :ValSet.Sorted.Mutable[E] = fromSource(source)

		override def rangeImpl(from: Option[E], until: Option[E]): MutableOrderedSet[E] =
			fromSource(source.rangeImpl(from, until))

		override def keysIteratorFrom(start: E): FitIterator[E] = source.keysIteratorFrom(start)


		override def +=(elem: E): this.type = { source += elem; this }

		override def +=(elem1: E, elem2: E, elems: E*) :this.type  =
			{ source = source + (elem1, elem2, elems:_*); this }


		override def -=(elem: E): this.type = { source -= elem; this }

		override def -=(elem1: E, elem2: E, elems: E*) :this.type =
			{ source = source - (elem1, elem2, elems:_*); this }

		override def +(elem: E) :MutableOrderedSet[E] = fromSource(source + elem)

		override def +(elem1: E, elem2: E, elems: E*) :MutableOrderedSet[E] =
			fromSource(source + (elem1, elem2, elems:_*))

		override def -(elem: E) :MutableOrderedSet[E] = fromSource(source - elem)

		override def -(elem1: E, elem2: E, elems: E*) :MutableOrderedSet[E]  =
			fromSource(source - (elem1, elem2, elems:_*))



		override def contains(elem: E): Boolean = source.contains(elem)

		override def head = source.head
		override def last = source.last

		override def newBuilder :FitBuilder[E, This] = fromSource((source :SetSpecialization[E, OrderedSet[E]]).empty)

		override def origin = OrderedSet.Mutable
	}

}


