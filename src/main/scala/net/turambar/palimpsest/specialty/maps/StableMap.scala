package net.turambar.palimpsest.specialty.maps


import scala.collection.immutable

import net.turambar.palimpsest.specialty.{FitBuilder, RuntimeType}
import net.turambar.palimpsest.specialty.iterables.{FitCompanion, FitIterable, IterableFoundation, IterableSpecialization, IterableViewTemplate, MappedIterableTemplate, SpecializableIterable, StableIterable, StableIterableTemplate}
import net.turambar.palimpsest.specialty.maps.FitMap.{ConvertingMap, FilteredKeysView, KeySetView, MappedValuesView, MapWithDefaultView}
import net.turambar.palimpsest.specialty.sets.StableSet
import net.turambar.palimpsest.specialty.maps.StableMap.{FilteredStableMapKeys, MakesStableMaps, MappedStableMapValues, StableMapKeySet, StableMapWithDefault}
import scala.annotation.unspecialized
import scala.collection.generic.CanBuildFrom

import net.turambar.palimpsest.specialty.sets.ValSet.ConvertingSet



trait StableMapKeySpecialization[@specialized(KeyTypes) K, +V, +M <: StableMap[K, V] with StableMapKeySpecialization[K, V, M]]
	extends immutable.MapLike[K, V, M] with MapInterfaceLike[K, V, StableMap]
	   with MapKeySpecialization[K, V, M] with StableIterableTemplate[(K, V), M]
{ //this :SpecializableIterable[(K, V), StableIterable] =>
	override def keySet :StableSet[K] = new StableMapKeySet[K](repr)

	override def filterKeys(p :K => Boolean) :StableMap[K, V] =
		new FilteredStableMapKeys[K, V](repr, p)

	override def mapValues[@specialized(ValueTypes) O](f :V => O) :StableMap[K, O] =
		new MappedStableMapValues[K, V, O](repr, f, mapEntryValue(f))

//	@unspecialized
//	override def carbon :M = repr
//	override def transform[@specialized(EntryTypes) O, That](f :(K, V) => O)(implicit bf :CanBuildFrom[StableMap[K, V], (K, O), That]) :That

}





/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait StableMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
	extends immutable.Map[K, V] with immutable.MapLike[K, V, StableMap[K, V]]
	   with FitIterable[(K, V)] with StableIterable[(K, V)] //with MapInterfaceLike[K, V, StableMap[K, V]]
	   with FitMap[K, V] with SpecializableMap[K, V, StableMap] with StableMapKeySpecialization[K, V, StableMap[K, V]] //with MakesStableMaps[K, V]
{

	@unspecialized
	override def empty :StableMap[K, V] = StableMap.of[K, V](keyType, valueSpecialization)

	@unspecialized
	override def withDefault[U >: V](d :K => U) :StableMap[K, U] =
		new StableMapWithDefault[K, U](this, d) with StableMap[K, U]

	@unspecialized
	override def withDefaultValue[U >: V](d :U) :StableMap[K, U] = withDefault(FitMap.defaultValue(d))

}





object StableMap extends SpecializableMapFactory[StableMap] {
	type From[K] = { type To[+V] = StableMap[K, V] }


	override def empty[@specialized(KeyTypes) K, @specialized(ValueTypes) V] :StableMap[K, V] = ???





	private[maps] class StableMapKeySet[@specialized(KeyTypes) K](final override protected[this] val source :FitMap[K, Any])
		extends IterableFoundation[K, StableSet[K]]
		   with KeySetView[K, StableSet[K]] with ConvertingSet[K, StableSet[K]] with StableSet[K]



	private[maps] class FilteredStableMapKeys[@specialized(KeyTypes) K, @specialized(ValueTypes) V](
			protected[this] final override val source :StableMap[K, V],
			protected[this] final override val filt :K => Boolean
		) extends IterableFoundation[(K, V), StableMap[K, V]]
		     with FilteredKeysView[K, V, StableMap] with ConvertingMap[K, V, StableMap] with StableMap[K, V]
	{
		protected[this] override val entryFilt = FitMap.entryFilter(filt)
	}



	private[maps] class MappedStableMapValues[@specialized(KeyTypes) K, V, @specialized(ValueTypes) +T](
			protected[this] final override val source :StableMap[K, V],
			protected[this] final override val forVal :V => T,
			protected[this] final override val forEntry :((K, V)) => T
		) extends IterableFoundation[(K, T), StableMap[K, T]]
		     with MappedValuesView[K, V, T] with MappedIterableTemplate[(K, V), (K, T), StableMap[K, T]]
	         with ConvertingMap[K, T, StableMap] with StableMap[K, T]



	private[maps] class StableMapWithDefault[@specialized(KeyTypes) K, @specialized(ValueTypes) +V](
			protected[this] final override val source :StableMap[K, V],
			protected[this] final override val whenNoKey :K => V
		) extends IterableFoundation[(K, V), StableMap[K, V]]
	         with StableMap[K, V] with MakesStableMaps[K, V] with ConvertingMap[K, V, StableMap]
		     with MapWithDefaultView[K, V, StableMap[K, V]]
	{
		override def empty :StableMap[K, V] = source.empty
	}



	private[maps] trait MakesStableMaps[K, +V] extends FitMap[K, V] with ConvertingMap[K, V, FitMap] {

		protected[this] override def build[U] :FitBuilder[(K, U), StableMap[K, U]] = StableMap.newBuilder

		override def companion :FitCompanion[StableIterable] = StableIterable

	}
}
