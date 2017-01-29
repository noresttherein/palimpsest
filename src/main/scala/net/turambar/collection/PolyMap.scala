package net.turambar.collection

import net.turambar.collection.PolyMap.Entry

import scala.collection.generic.CanBuildFrom
import scala.collection.{AbstractIterable, AbstractMap, GenIterable, mutable}


class PolyMap[K[X], +V[X]] private (private final val entries :Map[K[_], V[_]]) extends AbstractIterable[Entry[K, V, _]] {
	@inline private[this] def cast[X](v :V[_]) :V[X] = v.asInstanceOf[V[X]]

	def apply[E](key :K[E]) :V[E] = cast(entries(key))

	def get[X](key: K[X]): Option[V[X]] = entries.get(key).asInstanceOf[Option[V[X]]]

	def getOrElse[X, B>:V[X]](key: K[X], default : =>B) :B = get(key) getOrElse default

	override def iterator: Iterator[Entry[K, V, _]] = entries.iterator.asInstanceOf[Iterator[Entry[K, V, _]]]

	def +[V1[X] >: V[X], E](kv: (K[E], V1[E])): PolyMap[K, V1] = new PolyMap[K, V1](entries + (kv :(K[_], V1[_])))

	def -[E](key: K[E]): PolyMap[K, V] = new PolyMap[K, V](entries - key)


	def contains[X](key :K[X]) :Boolean = entries.contains(key)

	def isDefinedAt[X](key :K[X]) :Boolean = entries.isDefinedAt(key)

	def keys :Iterable[K[_]] = entries.keys

	def keySet :scala.collection.Set[K[_]] = entries.keySet

	def values :Iterable[V[_]] = entries.values

	def filterKeys(keep :K[_]=>Boolean) :PolyMap[K, V] = new PolyMap[K, V](entries.filterKeys(keep))


	override def canEqual(that :Any) = that.isInstanceOf[PolyMap[k, v] forSome { type k[X]; type v[X] }]

	override def hashCode = entries.hashCode

	override def equals(that :Any) = that match {
		case map :PolyMap[_, _] => map.canEqual(this) && entries == map.entries
		case _ => false
	}

	override def stringPrefix: String = "PolyMap"
}


object PolyMap {
	def apply[K[X], V[X]]() = empty

	implicit def cbf[K1[X], V1[X], K[X], V[X]] :CanBuildFrom[PolyMap[K1, V1], Entry[K, V, _], PolyMap[K, V]] =
		new CanBuildFrom[PolyMap[K1, V1], Entry[K, V, _], PolyMap[K, V]] {
			private val cons = { entries :Iterable[Entry[K, V, _]] =>
				new PolyMap[K, V](entries.map{ entry => entry.key -> entry.value }.toMap[K[_], V[_]])
			}

			override def apply(from: PolyMap[K1, V1]): mutable.Builder[Entry[K, V, _], PolyMap[K, V]] =
				from.entries.genericBuilder[Entry[K, V, _]].mapResult { cons }

			override def apply(): mutable.Builder[Entry[K, V, _], PolyMap[K, V]] =
				List.newBuilder[Entry[K, V, _]].mapResult { cons }

		}



	def empty[K[X], V[X]] = new PolyMap[K, V](Map.empty)


	final case class Entry[+K[X], +V[X], E](key :K[E], value :V[E]) {
		override def toString = s"$key -> $value"
	}


}
