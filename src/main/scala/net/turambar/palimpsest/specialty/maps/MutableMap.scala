package net.turambar.palimpsest.specialty.maps

import net.turambar.palimpsest.specialty.iterables.{MutableIterable, MutableIterableOverrides}
import net.turambar.palimpsest.specialty.FitCompanion

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait MutableMap[@specialized(KeyTypes) K, @specialized(ValueTypes) V]
	extends mutable.Map[K, V] with mutable.MapLike[K, V, MutableMap[K, V]] with MutableIterableOverrides[(K, V)]
	   with FitMap[K, V] with MapSpecialization[K, V, MutableMap.From[K]#To]
{
	override def empty :MutableMap[K, V] = MutableMap.empty

	override def companion :FitCompanion[MutableIterable] = MutableIterable



	override def put(key :K, value :V) :Option[V] = super.put(key, value)

	override def update(key :K, value :V) :Unit = super.update(key, value)

	override def remove(key :K) :Option[V] = super.remove(key)

	override def getOrElseUpdate(key :K, op : => V) :V = super.getOrElseUpdate(key, op)



	override def +[U >: V](kv :(K, U)) :MutableMap[K, U] = ???

	override  def -(key:  K): MutableMap[K, V] = ??? //super.-(key)


	override def withDefault(d :K => V) :MutableMap[K, V] = ??? //super.withDefault(d)

	override def withDefaultValue(d :V) :MutableMap[K, V] = ??? //super.withDefaultValue(d)


	override def transform(f :(K, V) => V) :this.type = super.transform(f)

	override def retain(p :(K, V) => Boolean) :this.type = super.retain(p)
}



object MutableMap {
	type From[K] = { type To[V] = MutableMap[K, V] }

	def empty[@specialized(KeyTypes) K, @specialized(ValueTypes) V] :MutableMap[K, V] = ???


}