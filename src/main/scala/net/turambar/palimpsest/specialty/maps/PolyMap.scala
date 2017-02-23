package net.turambar.palimpsest.specialty.maps

import scala.collection.{Map, MapLike}

/**
  * @author Marcin Mościcki
  */
/*
abstract class PolyMap[K, V[X]](impl :Map[K, V[_]]) extends Map[K, V[_]] with MapLike[K, V[_], PolyMap[K, V]] {
	override def seq: Map[K, V[_]] = this

	override def empty: PolyMap[K, V] = new PolyMap[K, V](impl.empty)

	override def get(key: K): Option[V[K]] = impl.get(key).asInstanceOf[Option[V[K]]]

//	override def +(kv: (K, V[K])): PolyMap[K, V] = new PolyMap[K, V](impl + kv)

	override def -(key: K): PolyMap[K, V] = new PolyMap[K, V](impl - key)

	override def iterator: Iterator[(K, V[_])] = ???

}
*/
