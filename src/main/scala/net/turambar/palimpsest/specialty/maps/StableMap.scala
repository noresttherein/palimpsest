package net.turambar.palimpsest.specialty.maps


import scala.collection.immutable
import net.turambar.palimpsest.specialty.RuntimeType
import net.turambar.palimpsest.specialty.iterables.{FitCompanion, FitIterable, IterableFoundation, IterableSpecialization, IterableViewTemplate, SpecializableIterable, StableIterable, StableIterableTemplate}
import net.turambar.palimpsest.specialty.maps.FitMap.{FilteredKeysView, KeySetView, MappedValuesView, MapWithDefaultFoundation}
import net.turambar.palimpsest.specialty.sets.StableSet
import net.turambar.palimpsest.specialty.maps.StableMap.{FilteredStableMapKeys, MappedStableMapValues, StableMapKeySet, StableMapOverrides, StableMapWithDefault}

import scala.annotation.unspecialized
import scala.collection.generic.CanBuildFrom



trait StableMapKeySpecialization[@specialized(KeyTypes) K, +V, +M <: StableMap[K, V] with StableMapKeySpecialization[K, V, M]]
	extends immutable.MapLike[K, V, M] with MapKeySpecialization[K, V, M] with StableIterableTemplate[(K, V), M]
{ //this :SpecializableIterable[(K, V), StableIterable] =>
	override def keySet :StableSet[K] = new StableMapKeySet[K](repr)

	override def filterKeys(p :K => Boolean) :StableMap[K, V] =
		new FilteredStableMapKeys[K, V](repr, p)

	override def mapValues[@specialized(ValueTypes) O](f :V => O) :StableMap[K, O] =
		new MappedStableMapValues[K, V, O](repr, f, mapEntryValue(f))

	@unspecialized
	override def carbon :M = repr
//	override def transform[@specialized(EntryTypes) O, That](f :(K, V) => O)(implicit bf :CanBuildFrom[StableMap[K, V], (K, O), That]) :That

}





/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait StableMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
	extends immutable.Map[K, V] with immutable.MapLike[K, V, StableMap[K, V]] //with IsStable[(K, V)]
	   with FitIterable[(K, V)] with IterableSpecialization[(K, V), StableMap[K, V]] with StableIterable[(K, V)]
	   with FitMap[K, V] with SpecializableMap[K, V, StableMap] with StableMapKeySpecialization[K, V, StableMap[K, V]] //with StableMapOverrides[K, V]
{

	protected[this] override def factory :SpecializableMapFactory[StableMap] = StableMap

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
		extends IterableFoundation[K, StableSet[K]] with KeySetView[K] with StableSet[K]



	private[maps] class FilteredStableMapKeys[@specialized(KeyTypes) K, @specialized(ValueTypes) V]
			(protected[this] final override val source :StableMap[K, V], protected[this] final override val pred :K => Boolean)
		extends IterableFoundation[(K, V), StableMap[K, V]] with FilteredKeysView[K, V] with StableMap[K, V]



	private[maps] class MappedStableMapValues[@specialized(KeyTypes) K, V, @specialized(ValueTypes) +T](
			protected[this] final override val source :StableMap[K, V],
			protected[this] final override val forVal :V => T,
			protected[this] final override val forEntry :((K, V)) => T
		) extends IterableFoundation[(K, T), StableMap[K, T]] with MappedValuesView[K, V, T] with StableMap[K, T]



	private[maps] class StableMapWithDefault[K, +V](target :StableMap[K, V], d :K => V)
		extends MapWithDefaultFoundation[K, V, StableMap[K, V]](target, d)
		   with StableMap[K, V] with StableMapOverrides[K, V]
	{
		override def empty :StableMap[K, V] = source.empty
	}



	trait StableMapOverrides[K, +V] extends FitMap[K, V] with SpecializableMap[K, V, StableMap] {

		protected override def factory :SpecializableMapFactory[StableMap] = StableMap

		override def companion :FitCompanion[StableIterable] = StableIterable

		override def +[U >: V](kv :(K, U)) :StableMap[K, U] = (empty ++ this) + kv

		override def -(key :K) :StableMap[K, V] = empty ++ this - key

		override def clone() :StableMap[K, V] = (empty ++ this)

	}
}
