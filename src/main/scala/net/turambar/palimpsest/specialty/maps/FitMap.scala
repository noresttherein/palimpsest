package net.turambar.palimpsest.specialty.maps

import net.turambar.palimpsest.specialty.{?, Blank, FitBuilder, Sure}
import net.turambar.palimpsest.specialty.iterables.{FitCompanion, FitIterable, IterableFoundation, IterableViewTemplate, MappedIterableTemplate}
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.maps.StableMap.StableMapOverrides
import net.turambar.palimpsest.specialty.seqs.{FitBuffer, FitSeq}
import net.turambar.palimpsest.specialty.sets.{MutableSet, SpecializableSet, StableSet, ValSet}
import net.turambar.palimpsest.specialty.sets.StableSet.MakesStableSets

import scala.annotation.unspecialized


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait FitMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
	extends collection.Map[K, V] with collection.MapLike[K, V, FitMap[K, V]]
		with FitIterable[(K, V)] with SpecializableMap[K, V, FitMap]
{
	protected[this] override def factory :SpecializableMapFactory[FitMap] = FitMap

//	override def empty :FitMap[K, V] = FitMap.empty
}







object FitMap extends SpecializableMapFactory[FitMap] {

	type From[K] = { type To[+V] = FitMap[K, V] }


	private[maps] final def entryKey[@specialized(KeyTypes) K] = (entry :(K, Any)) => entry._1
	private[maps] final def entryValue[@specialized(ValueTypes) V] = (entry :(Any, V)) => entry._2
	private[maps] final def defaultValue[K, V](value :V) :K => V = _ => value


	override def empty[@specialized(KeyTypes) K, @specialized(ValueTypes) V] :FitMap[K, V] = StableMap.empty[K, V]









	private[maps] trait KeySetView[@specialized(KeyTypes) K] extends ValSet[K] with MakesStableSets[K]
		   with MappedIterableTemplate[(K, Any), K, StableSet[K]]
	{
		override protected[this] def source :FitMap[K, Any]
		override protected[this] def forSource[@specialized(Boolean, Unit) O](f :K => O) :((K, Any)) => O = e => f(e._1)

		override protected[this] def mine :((K, Any)) => K = FitMap.entryKey

		@unspecialized
		override def iterator :FitIterator[K] = source.keysIterator

		override def contains(elem :K) :Boolean = source.contains(elem)
	}


	private[maps] class MapKeySet[@specialized(KeyTypes) K](final override protected[this] val source :FitMap[K, Any])
		extends IterableFoundation[K, StableSet[K]] with KeySetView[K]



	private[maps] class ValuesView[@specialized(ValueTypes) +V](final override protected[this] val source :FitMap[_, V])
		extends FitIterable[V] with MappedIterableTemplate[(Any, V), V, FitIterable[V]]
	{
		override protected[this] def forSource[@specialized(Boolean, Unit) O](f :V => O) :((Any, V)) => O =
			(entry :(Any, V)) => f(entry._2)

		override protected[this] def mine :((Any, V)) => V = FitMap.entryValue

		override def iterator :FitIterator[V] = source.valuesIterator
	}




	private[maps] trait FilteredKeysView[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
		extends FitMap[K, V] with StableMapOverrides[K, V]
	{
		protected[this] def source :FitMap[K, V]
		protected[this] def pred :K => Boolean

		@unspecialized
		override protected def reverseForeach(op :((K, V)) => Unit) :Unit =
			source reverseTraverse { kv => if (pred(kv._1)) op(kv) }

		override def foreach[@specialized(Unit) O](op :((K, V)) => O) :Unit =
			source foreach { kv => if (pred(kv._1)) op(kv) }


		override def ?(key :K) : ?[V] = if (pred(key)) source ? key else Blank


		@unspecialized
		override def iterator :FitIterator[(K, V)] = source.iterator.filter { e => pred(e._1) }

		override def filterKeys(p :K => Boolean) :FitMap[K, V] = source.filterKeys(k => pred(k) && p(k))

		@unspecialized
		override def clone() :StableMap[K, V] = empty ++ this
	}


	private[maps] class FilteredMapKeys[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
			(protected[this] final override val source :FitMap[K, V], protected[this] override val pred :K => Boolean)
		extends IterableFoundation[(K, V), StableMap[K, V]] with FilteredKeysView[K, V]





	trait MappedValuesView[@specialized(KeyTypes) K, V, @specialized(ValueTypes) +T]
		extends FitMap[K, T] with MappedIterableTemplate[(K, V), (K, T), StableMap[K, T]] with StableMapOverrides[K, T]
	{
		protected[this] override def source :FitMap[K, V]
		protected[this] def forVal :V => T
		protected[this] def forEntry :((K, V)) => T

		override protected[this] final def forSource[@specialized(Boolean, Unit) O](f :((K, T)) => O) :((K, V)) => O =
			kv => f((kv._1, forEntry(kv)))

		override protected[this] final def mine :((K, V)) => (K, T) = kv => (kv._1, forEntry(kv))

		override def ?(key :K) : ?[T] = source.?(key).map(forVal)

		@unspecialized
		override def clone() :StableMap[K, T] = empty ++ this
	}


	//todo: check that forEntry specialized apply is called and, if not, create a separate class for it.
	private[maps] class MappedMapValues[@specialized(KeyTypes) K, V, @specialized(ValueTypes) +T](
			protected[this] override final val source :FitMap[K, V],
			protected[this] override final val forVal :V=>T,
			protected[this] override final val forEntry :((K, V)) => T
		) extends IterableFoundation[(K, T), StableMap[K, T]] with MappedValuesView[K, V, T]



	abstract class MapWithDefaultFoundation[K, +V, +M <: FitMap[K, V] with MapKeySpecialization[K, V, M]]
			(protected[this] override val source :M, whenNoKey :K=>V)
		extends IterableFoundation[(K, V), M]
		   with FitMap[K, V] with MapKeySpecialization[K, V, M] with IterableViewTemplate[(K, V), M]
	{
		override def apply(key :K) :V = source ? key getOrElse whenNoKey(key)

		override def ?(key :K) : ?[V] = source ? key match {
			case sure :Sure[V] => sure
			case _ => Sure(whenNoKey(key))
		}

		override def empty :M = (source :MapKeySpecialization[K, V, M]).empty
	}
//	trait MapWithDefaultFoundation[K, V] extends FitMap[K, V] with IterableViewTemplate[(K, V), (K, V), FitMap[K, V]] {
//		@unspecialized
//		override protected[this] def source :FitMap[K, V]
//		@unspecialized
//		override protected def forSource[O](f :((K, V)) => O) :((K, V)) => O = f
//		@unspecialized
//		override protected def mine :((K, V)) => (K, V) = identity
//
//		override def ?(key :K) : ?[V] = Sure(source.getOrElse(key, default(key)))
//
//
//	}



}
