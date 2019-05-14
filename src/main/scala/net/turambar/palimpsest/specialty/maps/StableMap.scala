package net.turambar.palimpsest.specialty.maps


import scala.collection.immutable
import net.turambar.palimpsest.specialty.{FitCompanion, RuntimeType}
import net.turambar.palimpsest.specialty.iterables.{StableIterable, StableIterableOverrides}
import net.turambar.palimpsest.specialty.maps.FitMap.{FilteredKeysView, KeySetView, MappedValuesView}
import net.turambar.palimpsest.specialty.sets.StableSet
import net.turambar.palimpsest.specialty.maps.StableMap.StableMapOverrides

import scala.collection.generic.CanBuildFrom




/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait StableMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
	extends immutable.Map[K, V] with immutable.MapLike[K, V, StableMap[K, V]] with StableIterableOverrides[(K, V)]
		with FitMap[K, V] with MapSpecialization[K, V, StableMap.From[K]#To] with StableMapOverrides[K, V]
{
	override def empty :StableMap[K, V] = StableMap.empty

	override def companion :FitCompanion[StableIterable] = StableIterable

	override def keySet :StableSet[K] = new KeySetView[K](this) with StableSet[K]

	override def filterKeys(p :K => Boolean) :StableMap[K, V] = ???
	//		new FilteredKeysView[K, V](this, p) with StableMap[K, V]
	//
	override def mapValues[C](f :V => C) :StableMap[K, C] = ???
	//		new MappedValuesView(this, f) with IterableViewTemplate[(K, V), (K, C), StableMap[K, C]] with StableMap[K, C]

	override def withDefault[U >: V](d :K => U) :StableMap[K, U] = ??? //super.withDefault(d)

	override def withDefaultValue[U >: V](d :U) :StableMap[K, U] = ??? //super.withDefaultValue(d)

	override def transform[C, That](f :(K, V) => C)(implicit bf :CanBuildFrom[StableMap[K, V], (K, C), That]) :That =
		super.transform(f)
}





object StableMap extends FitMapFactory[StableMap] {
	type From[K] = { type To[+V] = StableMap[K, V] }


	override def empty[@specialized(KeyTypes) K, @specialized(ValueTypes) V] :StableMap[K, V] = ???





	trait StableMapOverrides[K, +V] extends FitMap[K, V] {
		override def empty :StableMap[K, V] = StableMap.empty[K, V]

		override def +[U >: V](kv :(K, U)) :StableMap[K, U] = StableMap.empty[K, U] ++ this + kv

		override def -(key :K) :StableMap[K, V] = StableMap.empty[K, V] ++ this - key
		//			if (contains(key)) StableMap.empty[K, V] ++ this - key
		//			else key


	}
}
