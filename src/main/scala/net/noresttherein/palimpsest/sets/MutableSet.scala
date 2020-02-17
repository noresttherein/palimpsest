package net.noresttherein.palimpsest.sets

import net.noresttherein.palimpsest.iterables.AptCompanion.CanFitFrom
import net.noresttherein.palimpsest.sets.ValSet.SetAdapter
import net.noresttherein.palimpsest._
import net.noresttherein.palimpsest.iterables.{AptCompanion, AptIterableFactory, InterfaceIterableFactory, MutableIterable, SpecializableIterableFactory}
import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.ordered.ValOrdering

import scala.annotation.unspecialized
import scala.collection.generic.{CanBuildFrom, Shrinkable}
import scala.collection.{mutable, GenSet, GenTraversableOnce}



trait MutableSetSpecialization[@specialized(ItemTypes) E, +This <: MutableSet[E] with MutableSetSpecialization[E, This]]
	extends mutable.SetLike[E, This] with AptBuilder[E, This] with SetSpecialization[E, This] //with IsMutable[E]
{

	override def +(elem :E) :This = carbon += elem

	override def -(elem :E) :This = carbon -= elem

	override def ^(elem :E) :This = carbon ^= elem

	override def +(elem1 :E, elem2 :E, elems :E*) :This = carbon += elem1 += elem2 ++= elems

	override def -(elem1 :E, elem2 :E, elems :E*) :This = carbon -= elem1 -= elem2 --= elems


	override def ++(xs :GenTraversableOnce[E]) :This = carbon ++= xs.seq

	override def --(xs :GenTraversableOnce[E]) :This = carbon --= xs.seq

	override def ++(elems :Vals[E]) :This = carbon ++= elems

	override def --(elems :Vals[E]) :This = carbon --= elems

	override def ^(elems :GenSet[E]) :This = carbon ^= elems

	override def ^(elems :ValSet[E]) :This = carbon ^= elems

	override def intersect(that :GenSet[E]) :This = carbon &= that

	override def &(that :ValSet[E]) :This = carbon &= that


	override def +=(elem1 :E, elem2 :E, elems :E*) :this.type = this += elem1 += elem2 ++= elems

	override def -=(elem1 :E, elem2 :E, elems :E*) :this.type = this -= elem1 -= elem2 --= elems

	override def +=(elem :E) :this.type

	override def -=(elem :E) :this.type

	def ^=(elem :E) :this.type = { flip(elem); this }

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


	def flip(elem :E) :Boolean = {
		if (contains(elem)) {
			this -= elem; false
		} else {
			this += elem; true
		}
	}

	override def --=(xs :TraversableOnce[E]) :this.type = xs match {
		case vals :Vals[E] => this --= vals
		case _ => xs foreach -=; this
	}

	def --=(xs :Vals[E]) :this.type = {
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


	@unspecialized
	override def carbon :This = clone()

	override def clone() :This = (this :SetSpecialization[E, This]).empty ++= this

	@unspecialized
	override def mutable :MutableSet[E] = carbon

	override def stable :StableSet[E] = StableSet.empty[E] ++ this



	/** Default mutable builder simply repurposing `this.empty`. Should be good for all set implementations. */
	override def newBuilder :AptBuilder[E, This] = (this :SetSpecialization[E, This]).empty

}





/**
  * @author Marcin Mościcki
  */
trait MutableSet[@specialized(ItemTypes) E]
	extends mutable.Set[E] with mutable.SetLike[E, MutableSet[E]] //with IsMutable[E]
	   with ValSet[E] with MutableSetSpecialization[E, MutableSet[E]] with SpecializableSet[E, MutableSet]
	   with MutableIterable[E]
{
	override def companion :AptCompanion[MutableSet] = MutableSet

	override def origin :AnyRef = companion

	protected[this] override def debugPrefix = "MutableSet"
}





object MutableSet extends InterfaceIterableFactory[MutableSet] {

	@inline final override implicit def canBuildFrom[E](implicit fit: CanFitFrom[MutableSet[_], E, MutableSet[E]]): CanBuildFrom[MutableSet[_], E, MutableSet[E]] =
		fit.cbf


	override protected[this] type RealType[@specialized(ItemTypes) E] = MutableHashSet[E]
	override protected[this] def default :AptIterableFactory[MutableHashSet] = MutableHashSet

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
	def from[@specialized(ItemTypes) E](immutable :ValSet[E]) :MutableSet[E] =
		new MutableSetAdapter(immutable)

	def from[@specialized(ItemTypes) E](immutable :OrderedSet[E]) :MutableOrderedSet[E] =
		new MutableOrderedSetAdapter(immutable)






	private[sets] trait MutableSetAdapterTemplate[
				+Source <: ValSet[E] with SetSpecialization[E, Source],
				@specialized(ItemTypes) E,
				+This <: MutableSet[E] with MutableSetSpecialization[E, This] with AptBuilder[E, This]
			]
		extends SetAdapter[Source, E, This] with MutableSetSpecialization[E, This] //with MutableSet[E] with SetSpecialization[E, This]
	{
		//compiler reports error without this redeclaration
		override protected[this] def fromSource(source :Source) :This

		override def +=(elem: E): this.type = { source = source + elem; this }

		override def +=(elem1: E, elem2: E, elems: E*) :this.type  = { source = source + (elem1, elem2, elems:_*); this }


		override def -=(elem: E): this.type = { source = source - elem; this }

		override def -=(elem1: E, elem2: E, elems: E*) :this.type = { source = source - (elem1, elem2, elems:_*); this }

		override def flip(elem :E) :Boolean = { source = source ^ elem; source.contains(elem) }



		override def apply(elem :E) :Boolean = source(elem)

		override def contains(elem: E): Boolean = source.contains(elem)

		override def head :E = source.head
		override def last :E = source.last


		@unspecialized
		override def stable :StableSet[E] = source.stable


		override def ++=(xs: TraversableOnce[E]) :this.type = { source = source ++ xs; this }

		@unspecialized
		override def ++=(xs: Vals[E]) :this.type = { source = source ++ xs; this }

		@unspecialized
		override def --=(xs: TraversableOnce[E]) :this.type = { source = source -- xs; this }

		@unspecialized
		override def newBuilder :AptBuilder[E, This] = fromSource((source :SetSpecialization[E, Source]).empty)

		@unspecialized
		override def clone() :This = fromSource(source.clone())

		@unspecialized
		override def clear() :Unit = source = (source :SetSpecialization[E, Source]).empty

		override def stringPrefix :String = "Mutable-"+source.stringPrefix


		override def origin :AnyRef = MutableSet

	}



	private class MutableSetAdapter[@specialized(ItemTypes) E](src :ValSet[E])
		extends SetAdapter[ValSet[E], E, MutableSet[E]](src)
		   with MutableSet[E] with MutableSetAdapterTemplate[ValSet[E], E, MutableSet[E]]
	{
		protected[this] def fromSource(other :ValSet[E]) :MutableSet[E] = new MutableSetAdapter(other)
	}



	private class MutableOrderedSetAdapter[@specialized(ItemTypes) E](src :OrderedSet[E])
		extends SetAdapter[OrderedSet[E], E, MutableOrderedSet[E]](src)
		   with MutableOrderedSet[E] with MutableSetAdapterTemplate[OrderedSet[E], E, MutableOrderedSet[E]]
	{
		protected[this] def fromSource(other: OrderedSet[E]): MutableOrderedSet[E] =
			new MutableOrderedSetAdapter(other)

		override implicit def ordering: ValOrdering[E] = source.ordering

		override def stable :StableOrderedSet[E] = source.stable
		override def mutable :MutableOrderedSet[E] = fromSource(source)


		override def keyAt(idx :Int) :E = source.keyAt(idx)

		override def from(start :E) :MutableOrderedSet[E] = new MutableOrderedSetAdapter(source.from(start))

		override def until(end :E) :MutableOrderedSetAdapter[E] = new MutableOrderedSetAdapter(source.until(end))

		override def to(end :E) :MutableOrderedSetAdapter[E] = new MutableOrderedSetAdapter(source.to(end))

		override def range(from :E, until :E) :MutableOrderedSet[E] = new MutableOrderedSetAdapter(source.range(from, until))

		override def rangeImpl(from: ?[E], until: ?[E]): MutableOrderedSet[E] =
			fromSource(source.rangeImpl(from, until))

		override def keysIteratorFrom(start: E): AptIterator[E] = source.keysIteratorFrom(start)


		override def +=(elem: E): this.type = { source += elem; this }

		override def +=(elem1: E, elem2: E, elems: E*) :this.type  =
			{ source = source + (elem1, elem2, elems:_*); this }


		override def -=(elem: E): this.type = { source -= elem; this }

		override def -=(elem1: E, elem2: E, elems: E*) :this.type =
			{ source = source - (elem1, elem2, elems:_*); this }


		override def origin :AnyRef = MutableOrderedSet
	}



}


