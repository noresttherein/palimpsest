package net.turambar.palimpsest.specialty.maps

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized
import scala.collection.{SortedMap, SortedMapLike}
import scala.collection.generic.SortedMapFactory

import net.turambar.palimpsest.specialty.{?, FitBuilder, RuntimeType}
import net.turambar.palimpsest.specialty.iterables.{FitCompanion, FitIterable, IterableFoundation, IterableSpecialization, MappedIterableTemplate}
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.maps.FitMap.{FilteredKeysView, KeySetView, MappedValuesView}
import net.turambar.palimpsest.specialty.maps.OrderedMap.{FilteredOrderedMapKeys, MappedOrderedMapValues, OrderedMapKeySet}
import net.turambar.palimpsest.specialty.maps.StableOrderedMap.{MakesStableOrderedMaps, StableOrderedMapKeySet}
import net.turambar.palimpsest.specialty.ordered.{OrderedBy, OrderedVals, ValOrdering}
import net.turambar.palimpsest.specialty.ordered.OrderedBy.OrderedProxy
import net.turambar.palimpsest.specialty.sets.{OrderedSet, StableOrderedSet, StableSet}
import net.turambar.palimpsest.specialty.sets.ValSet.ConvertingSet




trait OrderedMapKeySpecialization[@specialized(KeyTypes) K, +V,
	                           +M <: OrderedMap[K, V] with OrderedMapKeySpecialization[K, V, M]]
	extends SortedMapLike[K, V, M] with MapInterfaceLike[K, V, OrderedMap] with MapKeySpecialization[K, V, M] with OrderedBy[M, K]
{
	override def firstKey :K = head._1
	override def lastKey :K = last._1

	override def keyAt(n :Int) :K = {
		val i = keysIterator.drop(n)
		if (!i.hasNext)
			throw new IndexOutOfBoundsException(stringPrefix + ".keyAt(" +n + ") out of " + size)
		i.next()
	}


	override def keySet :OrderedSet[K] = new OrderedMapKeySet[K](repr)

	override def filterKeys(p :K => Boolean) :OrderedMap[K, V] = new FilteredOrderedMapKeys(repr, p)

	override def mapValues[@specialized(ValueTypes) O](f :V => O) :OrderedMap[K, O] = new MappedOrderedMapValues(repr, f, mapEntryValue(f))


	override def iteratorFrom(start :K) :FitIterator[(K, V)]

	override def valuesIteratorFrom(start :K) :FitIterator[V]
}






abstract class OrderedMapFactory[
		+M[@specialized(KeyTypes) K, @specialized(ValueTypes) V] <: OrderedMap[K, V] with OrderedMapKeySpecialization[K, V, M[K, V]]
	] //extends SortedMapFactory[M]
{
	def apply[@specialized(KeyTypes) K :ValOrdering, @specialized(ValueTypes) V](kv :(K, V)) :M[K, V] = ???

	def empty[@specialized(KeyTypes) K :ValOrdering, @specialized(ValueTypes) V] :M[K, V] = ???

	def newBuilder[@specialized(KeyTypes) K :ValOrdering, @specialized(ValueTypes) V] :FitBuilder[(K, V), M[K, V]] = ???


	def of[K :ValOrdering :RuntimeType, V :RuntimeType] :M[K, V] = ???

	def builder[K :ValOrdering :RuntimeType, V :RuntimeType] :FitBuilder[(K, V), M[K, V]] = ???


}






/**
  * @author Marcin Mościcki
  */
trait OrderedMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
	extends SortedMap[K, V] with FitMap[K, V]
	   with OrderedMapKeySpecialization[K, V, OrderedMap[K, V]] with SpecializableMap[K, V, OrderedMap]
{
	override def filterKeys(p :K => Boolean) :OrderedMap[K, V] = new FilteredOrderedMapKeys(repr, p)

	@unspecialized
	override def empty :OrderedMap[K, V] = OrderedMap.of(ordering, keyType, valueSpecialization)
}



object OrderedMap extends OrderedMapFactory[OrderedMap] {


	override def empty[@specialized(KeyTypes) K :ValOrdering, @specialized(ValueTypes) V] :OrderedMap[K, V] = ???


	private[maps] class OrderedMapKeySet[@specialized(KeyTypes) K](final override protected[this] val source :OrderedMap[K, Any])
		extends IterableFoundation[K, OrderedSet[K]] with OrderedProxy[OrderedSet[K], K] with OrderedVals[K]
			with KeySetView[K, OrderedSet[K]] with ConvertingSet[K, StableOrderedSet[K]] with OrderedSet[K]
	{
		override def rangeImpl(from : ?[K], until : ?[K]) :StableOrderedSet[K] =
			new StableOrderedMapKeySet(source.rangeImpl(from, until))

		@unspecialized
		override def empty :StableOrderedSet[K] = StableOrderedSet.of(ordering, specialization)
	}



	private[maps] trait FilteredOrderedKeysView[@specialized(KeyTypes) K, +V, +M[K, V] <: OrderedMap[K, V] with MapInterfaceLike[K, V, M]]
		extends MakesStableOrderedMaps[K, V] with FilteredKeysView[K, V, M]
	{

		@unspecialized
		override def ordering :ValOrdering[K] = source.ordering

		override def keysIteratorFrom(start :K) :FitIterator[K] =
			source.keysIteratorFrom(start).filter(filt)

		override def iteratorFrom(start :K) :FitIterator[(K, V)] =
			source.iteratorFrom(start).filter(entryFilt)

		override def valuesIteratorFrom(start :K) :FitIterator[V] =
			source.iteratorFrom(start).filter(entryFilt).map(FitMap.entryValue)
	}


	private[maps] class FilteredOrderedMapKeys[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
			(protected[this] final override val source :OrderedMap[K, V], protected[this] override val filt :K => Boolean)
		extends IterableFoundation[(K, V), OrderedMap[K, V] @uncheckedVariance]
		   with OrderedMap[K, V] with FilteredOrderedKeysView[K, V, OrderedMap] //MakesStableOrderedMaps[K, V] with FilteredKeysView[K, V, OrderedMap]
	{

		protected[this] override val entryFilt = FitMap.entryFilter(filt)

		override def rangeImpl(from : ?[K], until : ?[K]) :OrderedMap[K, V] =
			new FilteredOrderedMapKeys(source.rangeImpl(from, until), filt)

	}



	private[maps] class MappedOrderedMapValues[@specialized(KeyTypes) K, V, @specialized(ValueTypes) +T](
			protected[this] override final val source :OrderedMap[K, V],
			protected[this] override final val forVal :V=>T,
			protected[this] override final val forEntry :((K, V)) => T
		) extends IterableFoundation[(K, T), OrderedMap[K, T]]
		     with OrderedProxy[OrderedMap[K, T], K] with OrderedMap[K, T] with MakesStableOrderedMaps[K, T]
		     with MappedValuesView[K, V, T] with MappedIterableTemplate[(K, V), (K, T), OrderedMap[K, T]]
	{
		override def rangeImpl(from : ?[K], until : ?[K]) :OrderedMap[K, T] =
			new MappedOrderedMapValues(source.rangeImpl(from, until), forVal, forEntry)

		override def iteratorFrom(start :K) :FitIterator[(K, T)] =
			source.iteratorFrom(start).map(mine)

		override def valuesIteratorFrom(start :K) :FitIterator[T] =
			source.valuesIteratorFrom(start).map(forVal)
	}

}
