package net.turambar.palimpsest.specialty.maps

import scala.annotation.unspecialized
import scala.collection.{mutable, GenTraversableOnce, SortedMap}

import net.turambar.palimpsest.specialty.AptBuilder


/**
  * @author Marcin Mościcki
  */
trait MutableOrderedMap[@specialized(KeyTypes) K, @specialized(ValueTypes) V]
	extends mutable.Map[K, V] with mutable.MapLike[K, V, MutableOrderedMap[K, V]]
	   with OrderedMap[K, V] with OrderedMapKeySpecialization[K, V, MutableOrderedMap[K, V]]
	   with MutableMap[K, V] with SpecializableMap[K, V, MutableOrderedMap]
	   with AptBuilder[(K, V), MutableOrderedMap[K, V]]
{
	@unspecialized
	override def empty :MutableOrderedMap[K, V] = MutableOrderedMap.of[K, V](ordering, keyType, valueSpecialization)

	override def newBuilder :MutableOrderedMap[K, V] = empty


	override def -(key :K) :MutableOrderedMap[K, V] = MutableOrderedMap.empty(ordering) ++= this -= key

	@unspecialized
	override def +[U >: V](kv :(K, U)) :MutableOrderedMap[K, U] =
		MutableOrderedMap.empty[K, U](ordering) ++= this += kv

	@unspecialized
	override def ++[U >: V](xs :GenTraversableOnce[(K, U)]) :MutableOrderedMap[K, U] =
		MutableOrderedMap.empty[K, U](ordering) ++= this ++= xs.seq
}



object MutableOrderedMap extends OrderedMapFactory[MutableOrderedMap] {

}
