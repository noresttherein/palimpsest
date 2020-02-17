package net.turambar.palimpsest.specialty.maps

import net.turambar.palimpsest.specialty._
import net.turambar.palimpsest.specialty.maps.AptMap.{FilteredKeysView, FilteredMapKeys, KeySetView, MapKeySet, MappedMapValues, MappedValuesView, MapValues}

import scala.collection.{GenTraversableOnce, MapLike}
import net.turambar.palimpsest.specialty.sets.ValSet
import net.turambar.palimpsest.specialty.iterators.AptIterator.MappedIterator
import net.turambar.palimpsest.specialty.iterables.{CloneableIterable, AptIterable, IterableSpecialization}
import net.turambar.palimpsest.specialty.Specialize.SpecializeSome
import net.turambar.palimpsest.specialty.SpecializePair.{Curry, SpecializeSomePairs}
import net.turambar.palimpsest.specialty.iterators.AptIterator

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized




trait MapInterfaceLike[K, +V, +M[K, V]] {
	def filterKeys(p :K => Boolean) :M[K, V @uncheckedVariance]

	def mapValues[@specialized(ValueTypes) O](f :V => O) :M[K, O]
}



trait MapKeySpecialization[@specialized(KeyTypes) K, +V, +M <: AptMap[K, V] with MapKeySpecialization[K, V, M]]
	extends MapLike[K, V, M] with MapInterfaceLike[K, V, AptMap] with IterableSpecialization[(K, V), M] with CloneableIterable[(K, V), M]
{

	protected[this] override def specialization :RuntimeType[(K, V)] = RuntimeType.erased

	def keyType :RuntimeType[K] = RuntimeType.specialized[K]



	@unspecialized
	override def get(key :K) :Option[V] = ?(key).toOption

	def ?(key :K): ?[V]

	override def apply(key :K) :V = ?(key) match {
		case some :Sure[V] => some.value
		case _ => default(key)
	}

	override def contains(key :K) :Boolean = ?(key).isDefined

	override def isDefinedAt(key :K) :Boolean = contains(key)


	//todo: not specialized on values
	override def filterKeys(p :K => Boolean) :AptMap[K, V] = new FilteredMapKeys[K, V](repr, p)

	//somewhat counterintuitively, we define it here to avoid mind-boggling specializing on K, V, O. This is enough
	override def mapValues[@specialized(ValueTypes) O](f :V => O) :AptMap[K, O] =
		new MappedMapValues[K, V, O](repr, f, mapEntryValue(f))

	protected[this] def mapEntryValue[@specialized(ValueTypes) O](f :V => O) :((Any, V)) => O



	override def keySet :ValSet[K] = new MapKeySet[K](repr)

	@unspecialized
	override def keys :AptIterable[K] = keySet

	override def keysIterator :AptIterator[K] = new MappedIterator[(K, V), K](AptMap.entryKey)(iterator)


	protected[this] override def newBuilder :AptBuilder[(K, V), M] = new AptMapBuilder[K, V, M](empty)

}




/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait SpecializableMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V, +M[X, Y] <: AptMap[X, Y] with SpecializableMap[X, Y, M]]
	extends MapLike[K, V, M[K, V] @uncheckedVariance] with MapKeySpecialization[K, V, M[K, V] @uncheckedVariance]
{

	def valueType :RuntimeType[_ <: V] = valueSpecialization

	protected[this] def valueSpecialization :RuntimeType[V] = RuntimeType.specialized[V]



	@unspecialized
	override def updated[U >: V](key :K, value :U) :M[K, U] = this + ((key, value))

	@unspecialized
	override def +[U >: V](kv :(K, U)) :M[K, U]

	override def -(key :K) :M[K, V @uncheckedVariance]

	@unspecialized
	override def +[U >: V](kv1 :(K, U), kv2 :(K, U), kvs :(K, U)*) :M[K, U] =
		(this + kv1 + kv2).asInstanceOf[M[K, U]] ++ kvs //it's just cruel to the type system, but it's its inadequacy at fault

	@unspecialized
	override def ++[U >: V](xs :GenTraversableOnce[(K, U)]) :M[K, U] =
		((this.asInstanceOf[M[K, U]]) /: xs.seq)(_.asInstanceOf[M[K, U]] + _)


	override def filterKeys(p :K => Boolean) :AptMap[K, V] = new FilteredMapKeys[K, V](repr, p)


	override protected[this] def mapEntryValue[@specialized(ValueTypes) O](f :V => O) :((Any, V)) => O = kv => f(kv._2)



	override def valuesIterator :AptIterator[V] = new MappedIterator[(K, V), V](AptMap.entryValue)(iterator)


	override def values :AptIterable[V] = new MapValues[V](repr)



//	override def empty :M[K, V @uncheckedVariance] = factory.empty[K, V]

//	protected[this] override def newBuilder :AptBuilder[(K, V), M[K, V]] = new AptMapBuilder[K, V, M](empty)

	//no factory possible for ordered maps
//	protected[this] def factory :SpecializableMapFactory[M]

	//	override def toSeq :AptSeq
}










abstract class SpecializableMapFactory[
	+M[@specialized(KeyTypes) K, @specialized(ValueTypes) V] <: AptMap[K, V] with SpecializableMap[K, V, M]
]{


	def apply[@specialized(KeyTypes) K, @specialized(ValueTypes) V](kvs :(K, V)*) :M[K, V] =
		(newBuilder[K, V] ++= kvs).result()


	def empty[@specialized(KeyTypes) K, @specialized(ValueTypes) V] :M[K, V]

	def erased[K, V] :M[K, V] = Empty.asInstanceOf[M[K, V]]

	def of[K :RuntimeType, V :RuntimeType] :M[K, V] = EmptyMap[K, V]()

	private[this] final val Empty :M[Any, Any] = empty[Any, Any]


	def newBuilder[@specialized(KeyTypes) K, @specialized(ValueTypes) V] :AptBuilder[(K, V), M[K, V]] =
		new AptMapBuilder[K, V, M[K, V]](empty[K, V])

	def builder[K :RuntimeType, V :RuntimeType] :AptBuilder[(K, V), M[K, V]] =
		new AptMapBuilder[K, V, M[K, V]](of[K, V])




	private[this] final val EmptyMap :SpecializePair[M] = new SpecializeSomePairs[M] {
		private class SpecializeValue[@specialized K :RuntimeType] extends SpecializeSome[Curry[M]#T1[K]#T2] {
			override final val forChar :M[K, Char] = empty
			override final val forInt :M[K, Int] = empty
			override final val forLong :M[K, Long] = empty
			override final val forFloat :M[K, Float] = empty
			override final val forDouble :M[K, Double] = empty

			override protected def generic[V :RuntimeType] :M[K, V] = erased[K, V]
		}
		override final val forChar :First[Char] = new SpecializeValue[Char]
		override final val forInt :First[Int] = new SpecializeValue[Int]
		override final val forDouble :First[Double] = new SpecializeValue[Double]
		override final val forLong :First[Long] = new SpecializeValue[Long]
		override final val forFloat :First[Float] = new SpecializeValue[Float]

		override protected def generic[X :RuntimeType, Y :RuntimeType] :M[X, Y] = erased[X, Y]
	}


}






private[maps] class AptMapBuilder[K, V, +M <: AptMap[K, V] with MapLike[K, V, M]](empty :M)
	extends AptBuilder[(K, V), M]
{
	private[this] var elems = empty

	override def +=(elem :(K, V)) :this.type = { elems = (elems + elem).asInstanceOf[M]; this }

	override def ++=(xs :Vals[(K, V)]) :this.type = { elems = (elems ++ xs).asInstanceOf[M]; this}

	override def ++=(xs :TraversableOnce[(K, V)]) :this.type = { elems = (elems ++ xs).asInstanceOf[M]; this }

	override def result() :M = elems

	override def clear() :Unit = elems = empty
}

/*
private[maps] class AptMapBuilder[K, V,
                                  +M[X, Y] <: AptMap[X, Y] with SpecializableMap[X, Y, M]]
                                 (empty :M[K, V])
	extends AptBuilder[(K, V), M[K, V]]
{
	private[this] var elems = empty

	override def +=(elem :(K, V)) :this.type = { elems = (elems + elem).asInstanceOf[M[K, V]]; this }

	override def ++=(xs :FitTraversableOnce[(K, V)]) :this.type = { elems = (elems ++ xs).asInstanceOf[M[K, V]]; this}

	override def ++=(xs :TraversableOnce[(K, V)]) :this.type = { elems = (elems ++ xs).asInstanceOf[M[K, V]]; this }

	override def result() :M[K, V] = elems

	override def clear() :Unit = elems = empty
}
*/
