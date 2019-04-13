package net.turambar.palimpsest.specialty.sets


import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.IterableAdapter
import net.turambar.palimpsest.specialty.sets.ValSet.{SetAdapter, Sorted, Stable}
import net.turambar.palimpsest.specialty._
import net.turambar.palimpsest.specialty.ordered.ValOrdering

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, Shrinkable}
import scala.collection.{mutable, GenSet, GenTraversableOnce}



trait MutableSetSpecialization[@specialized(Elements) E, +This <: MutableSet[E] with MutableSetSpecialization[E, This]]
	extends mutable.SetLike[E, This] with SetSpecialization[E, This] with FitBuilder[E, This]
{

	override def +(elem :E) :This = clone() += elem

	override def -(elem :E) :This = clone() -= elem

	override def ^(elem :E) :This = clone() ^= elem

	override def +(elem1 :E, elem2 :E, elems :E*) :This = clone() += elem1 += elem2 ++= elems

	override def -(elem1 :E, elem2 :E, elems :E*) :This = clone() -= elem1 -= elem2 --= elems


	override def ++(xs :GenTraversableOnce[E]) :This = clone() ++= xs.seq

	override def --(xs :GenTraversableOnce[E]) :This = clone() --= xs.seq

	override def ++(elems :FitTraversableOnce[E]) :This = clone() ++= elems

	override def --(elems :FitTraversableOnce[E]) :This = clone() --= elems

	override def ^(elems :GenSet[E]) :This = clone() ^= elems

	override def ^(elems :ValSet[E]) :This = clone() ^= elems

	override def intersect(that :GenSet[E]) :This = clone() &= that

	override def &(that :ValSet[E]) :This = clone() &= that


	override def +=(elem1 :E, elem2 :E, elems :E*) :this.type = this += elem1 += elem2 ++= elems

	override def -=(elem1 :E, elem2 :E, elems :E*) :this.type = this -= elem1 -= elem2 --= elems

	override def +=(elem :E) :this.type

	override def -=(elem :E) :this.type

	def ^=(elem :E) :this.type

	override def add(elem :E) :Boolean = {
		val r = contains(elem)
		if (!r) {
			this += elem
			true
		} else
			false
	}

	override def remove(elem :E) :Boolean = {
		val r = contains(elem)
		if (r) {
			this -= elem
			true
		} else
			  false
	}

	@inline final override def update(elem :E, included :Boolean) :Unit =
		if (included) this += elem else this -= elem


	override def --=(xs :TraversableOnce[E]) :this.type = xs match {
		case vals :FitTraversableOnce[E] => this --= vals
		case _ => xs foreach -=; this
	}

	def --=(xs :FitTraversableOnce[E]) :this.type = {
		val it = xs.toIterator
		while (it.hasNext && nonEmpty) this -= it.next()
		this
	}


	def ^=(that :GenSet[E]) :this.type = that match {
		case set :ValSet[E] => this ^= set
		case _ =>
			that foreach ^=
			this
	}

	def ^=(that :ValSet[E]) :this.type = {
		val it = that.iterator
		while (it.hasNext) this ^= it.next()
		this
	}


	def &=(that :GenSet[E]) :this.type = that match {
		case set :ValSet[E] => this &= set
		case _ => retain(that); this
	}

	def &=(that :ValSet[E]) :this.type =
		if (that.isEmpty) {
			clear(); this
		} else {
			val it = toSeq.iterator
			do {
				val e = it.next()
				if (!that.contains(e))
					this -= e
			} while(it.hasNext)
			this
		}




	override def retain(f :E => Boolean) :Unit = {
		val it = toFitSeq.iterator
		while (it.hasNext) {
			val e = it.next()
			if (!f(e)) this -= e
		}
	}


	override def clone() :This = (this :SetSpecialization[E, This]).empty ++= this

	override def newBuilder :FitBuilder[E, This] = (this :SetSpecialization[E, This]).empty

	override def count :Int = size
}

//trait MutableSetSpecialization[E, +This <: MutableSet[E] with MutableSetSpecialization[E, This]]
//	extends mutable.SetLike[E, This] with SetSpecialization[E, This] with FitBuilder[E, This]
//{
//
//}


/**
  * @author Marcin Mościcki
  */
trait MutableSet[@specialized(Elements) E]
	extends mutable.Set[E] with mutable.SetLike[E, MutableSet[E]]
	   with ValSet[E] with MutableSetSpecialization[E, MutableSet[E]] with SpecializableSet[E, MutableSet]
{



	//	override def retain(p: (E) => Boolean) = super.retain(p)

	@unspecialized
	override def mutable :MutableSet[E] = clone()//this

	override def stable :StableSet[E] = StableSet.empty[E] ++ this

	override def companion :FitCompanion[MutableSet] = MutableSet

	override def typeStringPrefix = "MutableSet"
}




object MutableSet extends ImplementationIterableFactory[MutableSet] {
	type Ordered[@specialized(Elements) E] = MutableOrderedSet[E]

	final val Ordered = OrderedSet.Mutable
//	object Ordered {
//		def empty[@specialized E] :Sorted[E] = new MutableOrderedSetAdapter[E](OrderedSet.Mutable.empty[E])
//		def newBuilder[@specialized E] :FitBuilder[E, MutableOrderedSet[E]] = empty[E]
//	}

	@inline final override implicit def canBuildFrom[E](implicit fit: CanFitFrom[MutableSet[_], E, MutableSet[E]]): CanBuildFrom[MutableSet[_], E, MutableSet[E]] =
		fit.cbf

	/** Creates a mutable wrapper over another specialized set. This is especially useful if {{immutable}} is
	  * an immutable set, but it isn't required. Result of concurrent modifications done to the argument and
	  * returned instance is not defined.
	  * Specialization of the returned set is static and determined solely based on caller specialization context
	  * rather than the specialization of the argument.
	  * @param immutable initial contents of the set
	  * @return a [[MutableSet]] maintaining its state using a private variable
	  *         and 'immutable' API provided by the wrapped instance
	  *
	  */
	def from[@specialized(Elements) E](immutable :ValSet[E]) :MutableSet[E] =
		new MutableSetAdapter(immutable)

	def from[@specialized(Elements) E](immutable :OrderedSet[E]) :MutableOrderedSet[E] =
		new MutableOrderedSetAdapter(immutable)


	override def empty[@specialized(Elements) E] :MutableSet[E] = from(ValSet.empty[E])

	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, MutableSet[E]] = from(ValSet.empty[E])


	override def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, MutableSet[E]] =
		ValSet.specializedBuilder[E].mapResult(from[E](_))







	private[sets] trait MutableSetAdapterTemplate[
				+Source <: ValSet[E] with SetSpecialization[E, Source],
				@specialized(Elements) E,
				+This <: MutableSet[E] with SetSpecialization[E, This] with MutableSetSpecialization[E, This] with FitBuilder[E, This]
			]
		extends SetAdapter[Source, E, This] with MutableSetSpecialization[E, This] //with MutableSet[E] with SetSpecialization[E, This]
	{
		//compiler reports error without this redeclaration
		override protected[this] def fromSource(source :Source) :This

		override def +=(elem: E): this.type = { source = source + elem; this }

		override def +=(elem1: E, elem2: E, elems: E*) :this.type  = { source = source + (elem1, elem2, elems:_*); this }


		override def -=(elem: E): this.type = { source = source - elem; this }

		override def -=(elem1: E, elem2: E, elems: E*) :this.type = { source = source - (elem1, elem2, elems:_*); this }

		override def ^=(elem :E) :this.type = { source = source ^ elem; this }



		override def apply(elem :E) :Boolean = source(elem)

		override def contains(elem: E): Boolean = source.contains(elem)

		override def head :E = source.head
		override def last :E = source.last


		@unspecialized
		override def stable :Stable[E] = source.stable


		override def ++=(xs: TraversableOnce[E]) :this.type = { source = source ++ xs; this }

		@unspecialized
		override def ++=(xs: FitTraversableOnce[E]) :this.type = { source = source ++ xs; this }

		@unspecialized
		override def --=(xs: TraversableOnce[E]) :this.type = { source = source -- xs; this }

		@unspecialized
		override def newBuilder :FitBuilder[E, This] = fromSource((source :SetSpecialization[E, Source]).empty)

		@unspecialized
		override def clone() :This = fromSource(source.clone())

		@unspecialized
		override def clear() :Unit = source = (source :SetSpecialization[E, Source]).empty

		override def stringPrefix :String = "Mutable-"+source.stringPrefix


		override def origin :Any = MutableSet



	}



	private class MutableSetAdapter[@specialized(Elements) E](src :ValSet[E])
		extends SetAdapter[ValSet[E], E, MutableSet[E]](src)
		   with MutableSet[E] with MutableSetAdapterTemplate[ValSet[E], E, MutableSet[E]]
	{
		protected[this] def fromSource(other :ValSet[E]) :MutableSet[E] = new MutableSetAdapter(other)
	}



	private class MutableOrderedSetAdapter[@specialized(Elements) E](src :ValSet.Sorted[E])
		extends SetAdapter[ValSet.Sorted[E], E, MutableOrderedSet[E]](src)
		   with MutableOrderedSet[E] with MutableSetAdapterTemplate[Sorted[E], E, MutableOrderedSet[E]]
	{
		protected[this] def fromSource(other: ValSet.Sorted[E]): MutableOrderedSet[E] =
			new MutableOrderedSetAdapter(other)

		override implicit def ordering: ValOrdering[E] = source.ordering

		override def stable :ValSet.Sorted.Stable[E] = source.stable
		override def mutable :ValSet.Sorted.Mutable[E] = fromSource(source)


		override def keyAt(idx :Int) :E = source.keyAt(idx)

		override def from(start :E) :MutableOrderedSet[E] = new MutableOrderedSetAdapter(source.from(start))

		override def until(end :E) :MutableOrderedSetAdapter[E] = new MutableOrderedSetAdapter(source.until(end))

		override def to(end :E) :MutableOrderedSetAdapter[E] = new MutableOrderedSetAdapter(source.to(end))

		override def range(from :E, until :E) :MutableOrderedSet[E] = new MutableOrderedSetAdapter(source.range(from, until))

		override def rangeImpl(from: ?[E], until: ?[E]): MutableOrderedSet[E] =
			fromSource(source.rangeImpl(from, until))

		override def keysIteratorFrom(start: E): FitIterator[E] = source.keysIteratorFrom(start)


		override def +=(elem: E): this.type = { source += elem; this }

		override def +=(elem1: E, elem2: E, elems: E*) :this.type  =
			{ source = source + (elem1, elem2, elems:_*); this }


		override def -=(elem: E): this.type = { source -= elem; this }

		override def -=(elem1: E, elem2: E, elems: E*) :this.type =
			{ source = source - (elem1, elem2, elems:_*); this }


		override def origin :Any = OrderedSet.Mutable
	}

}


