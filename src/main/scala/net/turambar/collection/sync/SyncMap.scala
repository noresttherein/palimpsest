package net.turambar.collection.sync

import scala.collection.generic.{GenericTraversableTemplate, GenericCompanion, MutableMapFactory}
import scala.collection.{GenTraversable, GenTraversableLike, mutable}

/**
  * @author Marcin Mościcki
  */
class SyncMap[K, V](items :mutable.Map[K, V])
	extends collection.concurrent.Map[K, V] with mutable.MapLike[K, V, SyncMap[K, V]] with GenTraversable[(K, V)]
{
	def this() = this(mutable.Map[K, V]())

	override def putIfAbsent(k: K, v: V): Option[V] = synchronized(items.get(k) match {
		case None => items += k -> v; None
		case old => old

	})

	override def replace(k: K, oldvalue: V, newvalue: V): Boolean = synchronized(items.get(k) match {
		case Some(old) if old==oldvalue => items += k->newvalue; true
		case _ => false
	})

	override def replace(k: K, v: V): Option[V] = synchronized(items.get(k) match {
		case None => None
		case old => items += k -> v; old

	})

	override def remove(k: K, v: V): Boolean = synchronized(items.get(k) match {
		case Some(x) if x==v => items -= k; true
		case _ => false
	})

	override def -=(key: K): this.type = synchronized {
		items -= key; this
	}

	override def +=(kv: (K, V)): this.type = synchronized {
		items += kv; this
	}

	override def get(key: K): Option[V] = synchronized(items.get(key))

	override def iterator: Iterator[(K, V)] = synchronized(items.toList.iterator)

	protected[this] override def newBuilder: mutable.Builder[(K, V), SyncMap[K, V]] = new SyncMap[K, V]


	override def empty: SyncMap[K, V] = new SyncMap[K, V]

	override def stringPrefix = "SyncMap"
}


object SyncMap extends MutableMapFactory[SyncMap] {
	override def empty[A, B]: SyncMap[A, B] = new SyncMap[A, B]()
	override def newBuilder[K, V] = new SyncMap[K, V]()
}
