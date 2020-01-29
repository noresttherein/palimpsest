package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.{?, FitBuilder, ItemTypes, RuntimeType, Specialize}
import net.turambar.palimpsest.specialty.iterables.{FitCompanion, StableIterableTemplate}
import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable, mutable, GenTraversableOnce, SortedSet, SortedSetLike}

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.ordered.{OrderedAs, OrderedVals, ValOrdering}
import net.turambar.palimpsest.specialty.sets.ValSet.{ConvertingSet, StableSetBuilder}
import scala.annotation.unspecialized

import net.turambar.palimpsest.specialty.ordered.OrderedBy.OrderedProxy
import net.turambar.palimpsest.specialty.sets.MutableOrderedSet.MutableOrderedSetRange
import net.turambar.palimpsest.specialty.sets.OrderedSet.{OrderedSetRange, OrderedSetRangeSpecialization}
import net.turambar.palimpsest.specialty.sets.StableOrderedSet.StableOrderedSetRange

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

	override def iteratorFrom(start: E) :FitIterator[E] = keysIteratorFrom(start)


	def +(elem :E) :This

	def +(elem1 :E, elem2 :E, elems :E*) :This

	def ++(elems :GenTraversableOnce[E]) :This

	def -(elem :E) :This

	def -(elem1 :E, elem2 :E, elems :E*) :This

	def --(elems :GenTraversableOnce[E]) :This

	override def contains(key :E) :Boolean

//	override def empty =

//	protected[this] def factory

	override def stable :StableOrderedSet[E] = (StableOrderedSet.builder[E] ++= this).result()

	override def mutable :MutableOrderedSet[E] = MutableOrderedSet.of[E] ++= this

}


//todo: make this not specialized. Needs not implement specialized methods and as long as specialized OrderedVals and ValSet
//todo: are mixed in before this trait, all should be well (but check it to make sure)
/**
  * @author Marcin Mościcki
  */
trait OrderedSet[@specialized(ItemTypes) E] //todo: mix-in order of OrderedVals and ValSet
	extends SortedSet[E] with OrderedVals[E] with ValSet[E]
	   with SetSpecialization[E, OrderedSet[E]] with OrderedAs[E, OrderedSet[E]] with OrderedSetTemplate[E, OrderedSet[E]]
{

	override def reverseIterator: FitIterator[E] = inverse.iterator

//	override def stable :StableOrderedSet[E] = (StableOrderedSet.newBuilder[E] ++= this).result
//	override def mutable :MutableOrderedSet[E] = MutableOrderedSet.empty[E] ++= this

	@unspecialized
	override def empty :OrderedSet[E] = OrderedSet.of[E](ordering, specialization)

	override def rangeImpl(from: ?[E], until: ?[E]) :OrderedSet[E] =
		if (from.isEmpty && until.isEmpty) this
		else new OrderedSetRange[E](this, from, until)

	/** Overriden due to inheriting double declarations: from `SortedSetLike` and `OrderedAs`. */
	@inline final override def iteratorFrom(start: E) :FitIterator[E] = keysIteratorFrom(start)



	override def typeStringPrefix = "OrderedSet"

	protected[this] override def debugPrefix = "OrderedSet"
}





//todo: possibly doesn't need specialization
trait MutableOrderedSet[@specialized(ItemTypes) E]
	extends mutable.SortedSet[E] with OrderedSet[E] with OrderedAs[E, MutableOrderedSet[E]] with MutableSet[E]
	   with MutableSetSpecialization[E, MutableOrderedSet[E]] with OrderedSetTemplate[E, MutableOrderedSet[E]]
{
	override def mutable :MutableOrderedSet[E] = carbon
	override def stable :StableOrderedSet[E] = (StableOrderedSet.newBuilder[E] ++= this).result()

	override def empty :MutableOrderedSet[E] = MutableOrderedSet.empty[E]

	override def rangeImpl(from: ?[E], until: ?[E]) :MutableOrderedSet[E] =
		if (from.isEmpty && until.isEmpty) this
		else new MutableOrderedSetRange[E](this, from, until)

}







//todo: possibly doesn't need specialization
trait StableOrderedSet[@specialized(ItemTypes) E]
	extends immutable.SortedSet[E] with OrderedSet[E] with StableSet[E]
	   with OrderedAs[E, StableOrderedSet[E]] with SetSpecialization[E, StableOrderedSet[E]]
	   with OrderedSetTemplate[E, StableOrderedSet[E]] with StableIterableTemplate[E, StableOrderedSet[E]]
{
	@unspecialized
	override def empty :StableOrderedSet[E] = StableOrderedSet.of(ordering, specialization)

	override def rangeImpl(from: ?[E], until: ?[E]) :StableOrderedSet[E] =
		if (from.isEmpty && until.isEmpty) this
		else new StableOrderedSetRange[E](this, from, until)
}






abstract class OrderedSetFactory[+S[E] <: OrderedSet[E] with SetSpecialization[E, S[E]]] {

	def apply[@specialized(ItemTypes) E :ValOrdering](elems :E*) :S[E] =
		(newBuilder[E] ++= elems).result()


	def empty[@specialized(ItemTypes) E :ValOrdering] :S[E] //= newBuilder[E].result()

	def of[E :ValOrdering :RuntimeType] :S[E] = EmptySet(ValOrdering[E])

	def one[@specialized(ItemTypes) E :ValOrdering](singleton :E) :S[E] = (newBuilder[E] += singleton).result()

	def newBuilder[@specialized(ItemTypes) E :ValOrdering] :FitBuilder[E, S[E]] = new StableSetBuilder[E, S[E]](empty)

	def builder[E :ValOrdering :RuntimeType] :FitBuilder[E, S[E]] = Builder(ValOrdering[E])



//	implicit def canFitFrom[@specialized(Elements) E :ValOrdering] :CanFitFrom[S[_], E, S[E]] =
//		new CanBuildOrderedSet[E]


	private[this] val EmptySet :Specialize.With[S, ValOrdering] = new Specialize.With[S, ValOrdering] {
		override def specialized[@specialized E :RuntimeType](param :ValOrdering[E]) = empty[E](param)
	}

	private[this] type Builder[E] = FitBuilder[E, S[E]]

	private[this] final val Builder :Specialize.With[Builder, ValOrdering] = new Specialize.With[Builder, ValOrdering] {
		override def specialized[@specialized E :RuntimeType](param :ValOrdering[E]) = newBuilder(param)
	}


//	type CFF[E] = CanFitFrom[S[_], E, S[E]]
//	type CFF[E] = CanBuildFrom[S[_], E, S[E]]

	protected[this] class CanBuildOrderedSet[@specialized(ItemTypes) E :ValOrdering]
		extends CanBuildFrom[S[_], E, S[E]] with CanFitFrom[S[_], E, S[E]]
	{
		override def specialization :RuntimeType[E] = RuntimeType.specialized[E]

		override def apply(from :S[_]) :FitBuilder[E, S[E]] = newBuilder[E]

		override def apply() :FitBuilder[E, S[E]] = newBuilder[E]

		override def mapped[O](from :S[_], f :O => E) :FitBuilder[O, S[E]] = ???

		override def mapped[O :RuntimeType](f :O => E) :FitBuilder[O, S[E]] = ???
	}

}



/** Base class for companion objects of ordered sets containing additionally an implicit `CanFitFrom` for ordered sets,
  * based on implicitly available `ValOrdering`. Extracted here to have a single implementation of the required
  * specialized method.
  */
abstract class OrderedSetFactoryImplicits[S[E] <: OrderedSet[E] with SetSpecialization[E, S[E]]] extends OrderedSetFactory[S] {

	implicit def canFitFrom[@specialized(ItemTypes) E :ValOrdering] :CanFitFrom[S[_], E, S[E]] =
		new CanBuildOrderedSet[E]
}





object OrderedSet extends OrderedSetFactoryImplicits[OrderedSet] {

//	@inline final implicit def canBuildFrom[@specialized(Elements) E :ValOrdering]: CanBuildFrom[OrderedSet[_], E, OrderedSet[E]] =
//		new CanBuildOrderedSet[E]
	@inline final implicit def canBuildFrom[E](implicit fit :CanFitFrom[OrderedSet[_], E, OrderedSet[E]])
			:CanBuildFrom[OrderedSet[_], E, OrderedSet[E]] =
		fit.cbf


	override def empty[@specialized(ItemTypes) E :ValOrdering] :OrderedSet[E] = StableOrderedSet.empty[E]

//	def one[@specialized(Elements) E](elem :E)(implicit ord :Ordering[E]) :OrderedSet[E] =
//		(newBuilder[E] += elem).result()

	private[sets] trait OrderedSetRangeSpecialization[@specialized(ItemTypes) E, +S <: OrderedSet[E] with OrderedSetTemplate[E, S]]
		extends OrderedSet[E] with ConvertingSet[E, S]
	{
		protected val minKey: ?[E]
		protected val maxKey: ?[E]
		protected val source :S

		override def ordering :ValOrdering[E] = source.ordering

		override protected def reverseForeach(f :E => Unit) :Unit = {
			val ord = source.ordering
			source reverseTraverse { e =>
				if ((minKey.isEmpty || ord.compare(minKey.get, e) <= 0) && (maxKey.isEmpty || ord.compare(e, maxKey.get) < 0))
					f(e)
			}
		}


		override def contains(key :E) :Boolean = {
			val ord = source.ordering
			(minKey.isEmpty || ord.compare(minKey.get, key) <= 0) && (maxKey.isEmpty || ord.compare(key, maxKey.get) < 0) &&
				source.contains(key)
		}

		@unspecialized
		override def empty :S = (source :OrderedSetTemplate[E, S]).empty

		override def rangeImpl(from: ?[E], until: ?[E]) :S = {
			val ord = source.ordering
			val min =
				if (from.isEmpty) minKey
				else if (minKey.isEmpty) from
				else if (ord.compare(minKey.get, from.get) < 0) from
				else minKey
			val max =
				if (until.isEmpty) maxKey
				else if (maxKey.isEmpty) until
				else if (ord.compare(maxKey.get, until.get) > 0) until
				else maxKey
			(source :OrderedSetTemplate[E, S]).rangeImpl(min, max)
		}

		override def iterator :FitIterator[E] = {
			val iter =
				if (minKey.isEmpty) source.iterator
				else source.iteratorFrom(minKey.get)
			if (maxKey.isEmpty)
				source.iterator
			else {
				val ord = source.ordering
				val max = maxKey.get
				iter.takeWhile(ord.compare(_, max) < 0)
			}
		}

		override def keysIteratorFrom(start :E) :FitIterator[E] = {
			val ord = source.ordering
			val iter =
				if (minKey.isEmpty) source.iteratorFrom(start)
				else if (ord.compare(minKey.get, start) <= 0) source.iteratorFrom(start)
				else source.iteratorFrom(minKey.get)
			if (maxKey.isEmpty)
				source.iterator
			else {
				val ord = source.ordering
				val max = maxKey.get
				iter.takeWhile(ord.compare(_, max) < 0)
			}
		}

	}

	private[sets] class OrderedSetRange[@specialized(ItemTypes) E](
			protected override val source :OrderedSet[E],
			protected override val minKey: ?[E],
			protected override val maxKey: ?[E]
		) extends OrderedSet[E] with OrderedSetRangeSpecialization[E, OrderedSet[E]]

}





object StableOrderedSet extends OrderedSetFactoryImplicits[StableOrderedSet] {

	@inline final implicit def canBuildFrom[E](implicit fit :CanFitFrom[StableOrderedSet[_], E, StableOrderedSet[E]])
			:CanBuildFrom[StableOrderedSet[_], E, StableOrderedSet[E]] =
		fit.cbf

	override def empty[@specialized(ItemTypes) E :ValOrdering] :StableOrderedSet[E] = ???

	private[sets] class StableOrderedSetRange[@specialized(ItemTypes) E](
			protected override val source :StableOrderedSet[E],
			protected override val minKey: ?[E],
			protected override val maxKey: ?[E]
		) extends StableOrderedSet[E] with OrderedSetRangeSpecialization[E, StableOrderedSet[E]]

}





object MutableOrderedSet extends OrderedSetFactoryImplicits[MutableOrderedSet] {

	@inline final implicit def canBuildFrom[E](implicit fit :CanFitFrom[MutableOrderedSet[_], E, MutableOrderedSet[E]])
			:CanBuildFrom[MutableOrderedSet[_], E, MutableOrderedSet[E]] =
		fit.cbf

	override def empty[@specialized(ItemTypes) E :ValOrdering] :MutableOrderedSet[E] = ???

	override def newBuilder[@specialized(ItemTypes) E :ValOrdering] :FitBuilder[E, MutableOrderedSet[E]] = empty[E]


	private[sets] class MutableOrderedSetRange[@specialized(ItemTypes) E](
			protected override val source :MutableOrderedSet[E],
			protected override val minKey: ?[E],
			protected override val maxKey: ?[E]
		) extends MutableOrderedSet[E] with OrderedSetRangeSpecialization[E, MutableOrderedSet[E]]
	{
		override def +=(elem :E) :this.type = {
			source += elem; this
		}

		override def -=(elem :E) :this.type = {
			source -= elem; this
		}

	}

}