package net.turambar.palimpsest.specialty.maps

import net.turambar.palimpsest.specialty.{Elements, FitIterable, IterableSpecialization, Specialized}
import net.turambar.palimpsest.specialty.Specialized.Numbers

import scala.collection.MapLike

/**
  * @author Marcin Mościcki
  */
/*

trait MapSpecialization[@specialized(Numbers) K, @specialized(Numbers) +V, +M <: FitMap[K, V] with MapSpecialization[K, V, M]]
	extends MapLike[K, V, M] with IterableSpecialization[(K, V), M]
{
	protected[this] def valSpecialization :Specialized[V] = Specialized[V]
	def keySpecialization :Specialized[K] = Specialized[K]
	def valueSpecialization :Specialized[_<:V] = valSpecialization

	override def apply(key: K) :V
	override def get(key :K) :Option[V]
	override def getOrElse[U >: V](key :K, default :U) :U
	override def contains(key: K) :Boolean
	override def filterKeys(p: K => Boolean) :FitMap[K, V]

}



trait FitMap[@specialized(Elements) K, @specialized(Elements) +V] extends collection.Map[K, V] with FitIterable[(K, V)] with MapSpecialization[K, V, FitMap[K ,V]] {

}
*/
