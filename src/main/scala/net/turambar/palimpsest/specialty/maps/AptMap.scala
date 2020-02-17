package net.turambar.palimpsest.specialty.maps

import scala.annotation.unchecked.uncheckedVariance

import net.turambar.palimpsest.specialty.{?, Blank, AptBuilder, Sure}
import net.turambar.palimpsest.specialty.iterables.{AptCompanion, AptIterable, IterableFoundation, IterableViewTemplate, MappedIterableTemplate}
import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.maps.StableMap.MakesStableMaps
import net.turambar.palimpsest.specialty.seqs.{AptBuffer, AptSeq}
import net.turambar.palimpsest.specialty.sets.{MutableSet, SetSpecialization, SpecializableSet, StableSet, ValSet}
import scala.annotation.unspecialized

import net.turambar.palimpsest.specialty.maps.AptMap.FilteredMapKeys
import net.turambar.palimpsest.specialty.sets.ValSet.ConvertingSet


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait AptMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
	extends collection.Map[K, V] with collection.MapLike[K, V, AptMap[K, V]]
		with AptIterable[(K, V)] with SpecializableMap[K, V, AptMap]
{

	override def apply(key :K) :V = this ? key match {
		case found :Sure[V] => found.value
		case _ => default(key)
	}

	override def filterKeys(p :K => Boolean) :AptMap[K, V] = new FilteredMapKeys[K, V](repr, p)

	@unspecialized
	override def empty :AptMap[K, V] = AptMap.of(keyType, valueSpecialization)
}







object AptMap extends SpecializableMapFactory[AptMap] {

	type From[K] = { type To[+V] = AptMap[K, V] }


	private[maps] final def entryKey[@specialized(KeyTypes) K] = (entry :(K, Any)) => entry._1
	private[maps] final def entryValue[@specialized(ValueTypes) V] = (entry :(Any, V)) => entry._2
	private[maps] final def defaultValue[K, V](value :V) :K => V = _ => value
	private[maps] final def entryFilter[@specialized(KeyTypes) K, V](p :K => Boolean) = { kv :(K, V) => p(kv._1) }


	override def empty[@specialized(KeyTypes) K, @specialized(ValueTypes) V] :AptMap[K, V] = StableMap.empty[K, V]






	trait ConvertingMap[K, +V, +M[X, Y] <: AptMap[X, Y] with SpecializableMap[X, Y, M]]
		extends SpecializableMap[K, V, M]
	{

		override def +[U >: V](kv :(K, U)) :M[K, U] = (build[U] ++= this += kv).result()

		override def -(key :K) :M[K, V @uncheckedVariance] =
			((newBuilder ++= this).result() :MapKeySpecialization[K, V, M[K, V]]) - key

		override def clone() :M[K, V @uncheckedVariance] = (newBuilder ++= this).result()

		override def empty :M[K, V @uncheckedVariance] = newBuilder.result()


		protected[this] def build[U] :AptBuilder[(K, U), M[K, U]]
	}



	private[maps] trait KeySetView[@specialized(KeyTypes) K, +S <: ValSet[K] with SetSpecialization[K, S]]
		extends SetSpecialization[K, S] with MappedIterableTemplate[(K, Any), K, S]
	{
		override protected[this] def source :AptMap[K, Any]
		override protected[this] def forSource[@specialized(Boolean, Unit) O](f :K => O) :((K, Any)) => O = e => f(e._1)

		override protected[this] def mine :((K, Any)) => K = AptMap.entryKey

		@unspecialized
		override def iterator :AptIterator[K] = source.keysIterator

		override def contains(elem :K) :Boolean = source.contains(elem)

	}


	private[maps] class MapKeySet[@specialized(KeyTypes) K](final override protected[this] val source :AptMap[K, Any])
		extends IterableFoundation[K, ValSet[K]] with ValSet[K] with KeySetView[K, ValSet[K]] with ConvertingSet[K, StableSet[K]]
	{
		@unspecialized
		override def empty :StableSet[K] = StableSet.of(specialization)
	}


	private[maps] trait MapValuesView[@specialized(ValueTypes) +V]
		extends AptIterable[V] with MappedIterableTemplate[(Any, V), V, AptIterable[V]]
	{
		override protected[this] def source :AptMap[_, V]

		override protected[this] def forSource[@specialized(Boolean, Unit) O](f :V => O) :((Any, V)) => O =
			(entry :(Any, V)) => f(entry._2)

		override protected[this] def mine :((Any, V)) => V = AptMap.entryValue

		override def iterator :AptIterator[V] = source.valuesIterator

	}

	private[maps] class MapValues[@specialized(ValueTypes) +V](final override protected[this] val source :AptMap[_, V])
		extends IterableFoundation[V, AptIterable[V]] with MapValuesView[V]




	private[maps] trait FilteredKeysView[@specialized(KeyTypes) K, +V, +M[K, V] <: AptMap[K, V] with MapInterfaceLike[K, V, M]]
		extends AptMap[K, V] with MakesStableMaps[K, V]
	{
		protected[this] val entryFilt :((K, V)) => Boolean
		protected[this] def source :M[K, V]
		protected[this] val filt :K => Boolean
		private[this] def src :MapInterfaceLike[K, V, M] = source //without it the typechecker complains on specialized methods

		@unspecialized
		override protected def reverseForeach(op :((K, V)) => Unit) :Unit =
			source reverseTraverse { kv => if (entryFilt(kv)) op(kv) }

		@unspecialized
		override def foreach[@specialized(Unit) O](op :((K, V)) => O) :Unit =
			source foreach { kv => if (entryFilt(kv)) op(kv) }


		override def ?(key :K) : ?[V] = if (filt(key)) source ? key else Blank


		@unspecialized
		override def iterator :AptIterator[(K, V)] = source.iterator.filter(entryFilt)

		override def filterKeys(p :K => Boolean) :M[K, V @uncheckedVariance] = src.filterKeys(k => filt(k) && p(k))

	}


	private[maps] class FilteredMapKeys[@specialized(KeyTypes) K, @specialized(ValueTypes) +V]
			(protected[this] final override val source :AptMap[K, V], protected[this] override val filt :K => Boolean)
		extends IterableFoundation[(K, V), AptMap[K, V]] with AptMap[K, V] with FilteredKeysView[K, V, AptMap]
	{
		protected[this] override val entryFilt = entryFilter(filt)
	}




	trait MappedValuesView[@specialized(KeyTypes) K, V, @specialized(ValueTypes) +T]
		extends AptMap[K, T] with MappedIterableTemplate[(K, V), (K, T), AptMap[K, T]] with MakesStableMaps[K, T]
	{
		protected[this] override def source :AptMap[K, V]
		protected[this] val forVal :V => T
		protected[this] val forEntry :((K, V)) => T

		override protected[this] final def forSource[@specialized(Boolean, Unit) O](f :((K, T)) => O) :((K, V)) => O =
			kv => f((kv._1, forEntry(kv)))

		override protected[this] final def mine :((K, V)) => (K, T) = kv => (kv._1, forEntry(kv))

		override def ?(key :K) : ?[T] = source.?(key).map(forVal)

	}


	//todo: check that forEntry specialized apply is called and, if not, create a separate class for it.
	private[maps] class MappedMapValues[@specialized(KeyTypes) K, V, @specialized(ValueTypes) +T](
		                                                                                             protected[this] override final val source :AptMap[K, V],
		                                                                                             protected[this] override final val forVal :V=>T,
		                                                                                             protected[this] override final val forEntry :((K, V)) => T
		) extends IterableFoundation[(K, T), AptMap[K, T]] with MappedValuesView[K, V, T]



	private[maps] trait MapWithDefaultView[@specialized(KeyTypes) K, @specialized(ValueTypes) +V, +M <: AptMap[K, V] with MapKeySpecialization[K, V, M]]
		extends AptMap[K, V] with MapKeySpecialization[K, V, M] with IterableViewTemplate[(K, V), M]
	{
		protected[this] val source :M
		protected[this] val whenNoKey :K => V

		override def default(key :K) :V = whenNoKey(key)

		override def apply(key :K) :V = source ? key match {
			case sure :Sure[V] => sure.value
			case _ => whenNoKey(key)
		}

		override def ?(key :K) : ?[V] = source ? key match {
			case sure :Sure[V] => sure
			case _ => Sure(whenNoKey(key))
		}

		@unspecialized
		override def empty :M = (source :MapKeySpecialization[K, V, M]).empty

	}


}

