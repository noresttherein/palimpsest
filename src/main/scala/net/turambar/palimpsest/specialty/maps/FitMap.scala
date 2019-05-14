package net.turambar.palimpsest.specialty.maps

import net.turambar.palimpsest.specialty.{?, Blank, FitBuilder, FitCompanion, FitIterator}
import net.turambar.palimpsest.specialty.iterables.{FitIterable, IterableViewTemplate}
import net.turambar.palimpsest.specialty.maps.StableMap.StableMapOverrides
import net.turambar.palimpsest.specialty.seqs.{FitBuffer, FitSeq}
import net.turambar.palimpsest.specialty.sets.{MutableSet, SpecializableSet, StableSet, ValSet}

import scala.annotation.unspecialized


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait FitMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
	extends collection.Map[K, V] with collection.MapLike[K, V, FitMap[K, V]]
		with FitIterable[(K, V)] with MapSpecialization[K, V, FitMap.From[K]#To]
{
	override def empty :FitMap[K, V] = FitMap.empty
}







object FitMap extends FitMapFactory[FitMap] {

	type From[K] = { type To[+V] = FitMap[K, V] }


	private[maps] final def entryKey[@specialized(KeyTypes) K] = (entry :(K, Any)) => entry._1
	private[maps] final def entryValue[@specialized(ValueTypes) V] = (entry :(Any, V)) => entry._2



	override def empty[@specialized(KeyTypes) K, @specialized(ValueTypes) V] :FitMap[K, V] = StableMap.empty[K, V]

//	def newBuilder[@specialized(KeyTypes) K, @specialized(ValueTypes) V] :FitBuilder[(K, V), FitMap[K, V]] =
//		new FitMapBuilder(empty[K, V])









	private[maps] class KeySetView[@specialized(KeyTypes) K](final override protected[this] val source :FitMap[K, Any])
		extends ValSet[K] with SpecializableSet[K, StableSet] with IterableViewTemplate[(K, Any), K, StableSet[K]]
	{
		override def companion :FitCompanion[StableSet] = StableSet

		override protected[this] def forSource[@specialized(Boolean, Unit) O](f :K => O) = e => f(e._1)

		override protected[this] def mine :((K, Any)) => K = FitMap.entryKey

		@unspecialized
		override def iterator :FitIterator[K] = source.keysIterator


		override def contains(elem :K) :Boolean = source.contains(elem)

		override def +(elem :K) :StableSet[K] = StableSet[K]() ++ this + elem

		override def -(elem :K) :StableSet[K] = StableSet[K]() ++ this - elem

		override def ^(elem :K) :StableSet[K] = StableSet[K]() ++ this ^ elem



		//todo: all methods below should be extracted to some base class. Problem: scala collections overriding base class methods
		override def stable :StableSet[K] = StableSet.empty[K] ++ this

		override def mutable :MutableSet[K] = MutableSet.empty[K] ++= this

		@unspecialized
		override def toSeq :FitSeq[K] = toFitBuffer[K]

		@unspecialized
		override def toBuffer[U >: K] :FitBuffer[U] = toFitBuffer[U]
	}




	private[maps] class ValuesView[@specialized(ValueTypes) +V](final override protected[this] val source :FitMap[_, V])
		extends FitIterable[V] with IterableViewTemplate[(Any, V), V, FitIterable[V]]
	{
		override protected[this] def forSource[@specialized(Boolean, Unit) O](f :V => O) :((Any, V)) => O =
			(entry :(Any, V)) => f(entry._2)

		override protected[this] def mine :((Any, V)) => V = FitMap.entryValue

		override def iterator :FitIterator[V] = source.valuesIterator
	}




	private[maps] class FilteredKeysView[@specialized(KeyTypes) K, @specialized(ValueTypes) +V](
			final private[this] val source :FitMap[K, V], f :K => Boolean)
		extends FitMap[K, V] with StableMapOverrides[K, V]//with MapSpecialization[K, V, StableMap.From[K]#To] //with StableMapOverrides[K, V]
	{
		@unspecialized
		override protected def reverseForeach(op :((K, V)) => Unit) :Unit =
			source reverseTraverse { kv => if (f(kv._1)) op(kv) }

		override def foreach[@specialized(Unit) O](op :((K, V)) => O) :Unit =
			source foreach { kv => if (f(kv._1)) op(kv) }



		override def ?(key :K) : ?[V] = if (f(key)) source ? key else Blank

		//		@unspecialized
		//		override def +[U >: V](kv :(K, U)) :StableMap[K, U] = StableMap.empty[K, U] ++ source + kv
		//
		//		@unspecialized
		//		override def -(key :K) :FitMap[K, V] = if (!contains(key)) this else source - key

		@unspecialized
		override def iterator :FitIterator[(K, V)] = source.iterator.filter { e => f(e._1) }

		override def filterKeys(p :K => Boolean) :FitMap[K, V] = source.filterKeys(k => f(k) && p(k))
	}





	private[maps] class MappedValuesView[@specialized(KeyTypes) K, +V, @specialized(ValueTypes) +T]
	(protected[this] override final val source :FitMap[K, V], m :V=>T)
		extends FitMap[K, T] with IterableViewTemplate[(K, V), (K, T), FitMap[K, T]] with StableMapOverrides[K, T]
	{
		override protected[this] final def forSource[@specialized(Boolean, Unit) O](f :((K, T)) => O) :((K, V)) => O =
			kv => f((kv._1, m(kv._2)))

		@unspecialized
		override protected[this] final def mine :((K, V)) => (K, T) = kv => (kv._1, m(kv._2))

		override def ?(key :K) : ?[T] = source.?(key).map(m)

		//		override def +[U >: T](kv :(K, U)) :FitMap[K, U] = FitMap.empty[K, U] ++ this + kv
		//
		//		override def -(key :K) :FitMap[K, T] =
		//			if (contains(key)) FitMap.empty[K, T] ++ this - key
		//			else this
	}

}
