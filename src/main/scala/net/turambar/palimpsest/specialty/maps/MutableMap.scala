package net.turambar.palimpsest.specialty.maps

import net.turambar.palimpsest.specialty.iterables.{FitCompanion, FitIterable, IterableFoundation, IterableSpecialization, MutableIterable, SpecializableIterable}
import net.turambar.palimpsest.specialty.{FitBuilder, FitTraversableOnce, Sure}
import net.turambar.palimpsest.specialty.maps.FitMap.{MapWithDefaultView}
import net.turambar.palimpsest.specialty.maps.MutableMap.MutableMapWithDefault
import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized
import scala.collection.{mutable, GenTraversableOnce}



/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait MutableMap[@specialized(KeyTypes) K, @specialized(ValueTypes) V]
	extends mutable.Map[K, V] with mutable.MapLike[K, V, MutableMap[K, V]] //with IsMutable[(K, V)]
	   with FitIterable[(K, V)] /*with IterableSpecialization[(K, V), MutableMap[K, V]]*/ with MutableIterable[(K, V)]
	   with FitMap[K, V] with SpecializableMap[K, V, MutableMap] with FitBuilder[(K, V), MutableMap[K, V]]
{ outer =>

	@unspecialized
	override def empty :MutableMap[K, V] = MutableMap.of(keyType, valueSpecialization)

	@unspecialized
	override def newBuilder :MutableMap[K, V] = MutableMap.empty(keyType, valueSpecialization)

	override def companion :FitCompanion[MutableIterable] = MutableIterable



	override def put(key :K, value :V) :Option[V] = {
		val res = get(key); update(key, value); res
	}

	override def update(key :K, value :V) :Unit = this += ((key, value))

	override def +=(entry :(K, V)) :this.type = { update(entry._1, entry._2); this }

	override def remove(key :K) :Option[V] = {
		val res = get(key)
		if (res.isDefined)
			this -= key
		res
	}

	override def getOrElseUpdate(key :K, op : => V) :V = ?(key) match {
		case present :Sure[V] => present.value
		case _ => val v = op; put(key, v); v
	}


	@unspecialized
	override def +[U >: V](kv :(K, U)) :MutableMap[K, U] = MutableMap.empty[K, U] ++= this += kv

	override  def -(key:  K): MutableMap[K, V] = empty ++= this -= key


	@unspecialized
	override def ++[U >: V](xs :GenTraversableOnce[(K, U)]) :MutableMap[K, U] =
		MutableMap.empty[K, U] ++= this ++= xs.seq

	override def withDefault(d :K => V) :MutableMap[K, V] = new MutableMapWithDefault[K, V](this, d)

	override def withDefaultValue(d :V) :MutableMap[K, V] = withDefault(FitMap.defaultValue(d))


	override def transform(f :(K, V) => V) :this.type = {
		for { kv <- iterator; swap = f(kv._1, kv._2); if swap != kv._2 }
			put(kv._1, swap)
		this
	}

	override def retain(p :(K, V) => Boolean) :this.type = {
		for { kv <- toSeq; if !p(kv._1, kv._2) }
			this -= kv._1
		this
	}

}





object MutableMap extends SpecializableMapFactory[MutableMap] {
	type From[K] = { type To[V] = MutableMap[K, V] }

	override def empty[@specialized(KeyTypes) K, @specialized(ValueTypes) V] :MutableMap[K, V] = ???



	private[maps] class MutableMapWithDefault[@specialized(KeyTypes) K, @specialized(ValueTypes) V](
			protected[this] override val source :MutableMap[K, V],
			protected[this] override val whenNoKey :K => V
		) extends IterableFoundation[(K, V), MutableMap[K, V]]
	         with MapWithDefaultView[K, V, MutableMap[K, V]] with MutableMap[K, V]
	{
		override def -=(key :K) :this.type = { source -= key; this }

		override def +=(elem :(K, V)) :this.type = { source += elem; this }

		override def ++=(xs :FitTraversableOnce[(K, V)]) :this.type = { source ++= xs; this }

		override def ++=(xs :TraversableOnce[(K, V)]) :this.type = { source ++= xs; this }

		override def empty :MutableMap[K, V] = source.empty
	}

}

