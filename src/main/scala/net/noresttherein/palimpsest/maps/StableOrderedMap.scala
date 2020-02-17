package net.noresttherein.palimpsest.maps

import net.noresttherein.palimpsest.sets.StableOrderedSet

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized
import scala.collection.immutable.SortedMap
import net.noresttherein.palimpsest.{?, AptBuilder}
import net.noresttherein.palimpsest.iterables.{IterableFoundation, MappedIterableTemplate, StableIterableTemplate}
import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.maps.AptMap.{ConvertingMap, KeySetView, MappedValuesView}
import net.noresttherein.palimpsest.maps.OrderedMap.{FilteredOrderedKeysView, FilteredOrderedMapKeys, MappedOrderedMapValues, OrderedMapKeySet}
import net.noresttherein.palimpsest.maps.StableMap.MakesStableMaps
import net.noresttherein.palimpsest.maps.StableOrderedMap.{FilteredStableOrderedMapKeys, MappedStableOrderedMapValues, StableOrderedMapKeySet}
import net.noresttherein.palimpsest.ordered.OrderedBy.OrderedProxy
import net.noresttherein.palimpsest.ordered.OrderedVals
import net.noresttherein.palimpsest.sets.{OrderedSet, StableOrderedSet, StableSet}
import net.noresttherein.palimpsest.sets.ValSet.ConvertingSet




trait StableOrderedMapKeySpecialization[@specialized(KeyTypes) K, +V,
                                        +M <: StableOrderedMap[K, V] with StableOrderedMapKeySpecialization[K, V, M]]
	extends OrderedMapKeySpecialization[K, V, M] with MapInterfaceLike[K, V, StableOrderedMap]
{
	override def keySet :StableOrderedSet[K] = new StableOrderedMapKeySet[K](repr)

	override def filterKeys(p :K => Boolean) :StableOrderedMap[K, V] = new FilteredStableOrderedMapKeys(repr, p)

	override def mapValues[@specialized(ValueTypes) O](f :V => O) :StableOrderedMap[K, O] =
		new MappedStableOrderedMapValues(repr, f, mapEntryValue(f))

}



/**
  * @author Marcin Mościcki
  */
trait StableOrderedMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
	extends SortedMap[K, V] with StableMap[K, V] with OrderedMap[K, V]
	   with StableOrderedMapKeySpecialization[K, V, StableOrderedMap[K, V]]
	   with StableMapKeySpecialization[K, V, StableOrderedMap[K, V]]
	   with SpecializableMap[K, V, StableOrderedMap]
{

	@unspecialized
	override def +[U >: V](kv :(K, U)) :StableOrderedMap[K, U] = throw new AbstractMethodError("StableOrderedMap.+")


	override def filterKeys(p :K => Boolean) :StableOrderedMap[K, V] = new FilteredStableOrderedMapKeys(repr, p)

	@unspecialized
	override def empty :StableOrderedMap[K, V] = StableOrderedMap.of(ordering, keyType, valueSpecialization)
}





object StableOrderedMap extends OrderedMapFactory[StableOrderedMap] {



	trait MakesStableOrderedMaps[K, +V] extends OrderedMap[K, V] with MakesStableMaps[K, V] with ConvertingMap[K, V, OrderedMap] {

		protected[this] override def build[U] :AptBuilder[(K, U), StableOrderedMap[K, U]] = StableOrderedMap.newBuilder
	}


	private[maps] class StableOrderedMapKeySet[@specialized(KeyTypes) K](final override protected[this] val source :OrderedMap[K, Any])
		extends IterableFoundation[K, StableOrderedSet[K]] with OrderedProxy[StableOrderedSet[K], K] with OrderedVals[K]
			with KeySetView[K, StableOrderedSet[K]] with ConvertingSet[K, StableOrderedSet[K]] with StableOrderedSet[K]
	{
		override def rangeImpl(from: ?[K], until: ?[K]) :StableOrderedSet[K] =
			new StableOrderedMapKeySet(source.rangeImpl(from, until))
	}



	private[maps] class FilteredStableOrderedMapKeys[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
			(protected[this] final override val source :StableOrderedMap[K, V], protected[this] override val filt :K => Boolean)
		extends IterableFoundation[(K, V), StableOrderedMap[K, V] @uncheckedVariance]
			with StableOrderedMap[K, V] with FilteredOrderedKeysView[K, V, StableOrderedMap] with ConvertingMap[K, V, StableOrderedMap]
	{

		protected[this] override val entryFilt = AptMap.entryFilter(filt)

		override def rangeImpl(from : ?[K], until : ?[K]) :StableOrderedMap[K, V] =
			new FilteredStableOrderedMapKeys(source.rangeImpl(from, until), filt)

	}



	private[maps] class MappedStableOrderedMapValues[@specialized(KeyTypes) K, V, @specialized(ValueTypes) +T](
			protected[this] override final val source :OrderedMap[K, V],
			protected[this] override final val forVal :V=>T,
			protected[this] override final val forEntry :((K, V)) => T
		) extends IterableFoundation[(K, T), StableOrderedMap[K, T]]
		     with OrderedProxy[OrderedMap[K, T], K] with StableOrderedMap[K, T] with MakesStableOrderedMaps[K, T]
		     with MappedValuesView[K, V, T] with MappedIterableTemplate[(K, V), (K, T), StableOrderedMap[K, T]]
	         with ConvertingMap[K, T, StableOrderedMap]
	{
		override def rangeImpl(from : ?[K], until : ?[K]) :StableOrderedMap[K, T] =
			new MappedStableOrderedMapValues(source.rangeImpl(from, until), forVal, forEntry)

		override def iteratorFrom(start :K) :AptIterator[(K, T)] =
			source.iteratorFrom(start).map(mine)

		override def valuesIteratorFrom(start :K) :AptIterator[T] =
			source.valuesIteratorFrom(start).map(forVal)
	}

}


