package net.turambar.palimpsest.specialty.maps

import net.turambar.palimpsest.specialty._
import net.turambar.palimpsest.specialty.maps.FitMap.{FilteredKeysView, KeySetView, MappedValuesView, ValuesView}

import scala.collection.{GenTraversableOnce, MapLike}
import net.turambar.palimpsest.specialty.sets.ValSet
import net.turambar.palimpsest.specialty.FitIterator.MappedIterator
import net.turambar.palimpsest.specialty.iterables.{FitIterable, IterableSpecialization}
import net.turambar.palimpsest.specialty.Specialize.SpecializeSome
import net.turambar.palimpsest.specialty.SpecializePair.{Curry, SpecializeSomePairs}

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized







/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait MapSpecialization[@specialized(KeyTypes) K, @specialized(ValueTypes) +V, +M[Y] <: FitMap[K, Y] with MapSpecialization[K, Y, M]]
	extends MapLike[K, V, M[V] @uncheckedVariance] with IterableSpecialization[(K, V), M[V] @uncheckedVariance]
{


	protected[this] override def specialization :RuntimeType[(K, V)] = RuntimeType.erased

	def keyType :RuntimeType[K] = RuntimeType.specialized[K]

	def valueType :RuntimeType[_ <: V] = valueSpecialization

	protected[this] def valueSpecialization :RuntimeType[V] = RuntimeType.specialized[V]


	@unspecialized
	override def get(key :K) :Option[V] = ?(key).toOption

	def ?(key :K): ?[V]

	override def apply(key :K) :V = ?(key) match {
		case some :Sure[V] => some.value
		case _ => default(key)
	}

	override def contains(key :K) :Boolean = ?(key).isDefined

	override def isDefinedAt(key :K) :Boolean = contains(key)


	@unspecialized
	override def updated[U >: V](key :K, value :U) :M[U] = this + ((key, value))

	@unspecialized
	override def +[U >: V](kv :(K, U)) :M[U] //FitMap[K, U]

	override def -(key :K) :M[V @uncheckedVariance]

	@unspecialized
	override def +[U >: V](kv1 :(K, U), kv2 :(K, U), kvs :(K, U)*) :M[U] =
		(this + kv1 + kv2).asInstanceOf[M[U]] ++ kvs //it's just cruel to the type system, but it's its inadequacy at fault

	@unspecialized
	override def ++[U >: V](xs :GenTraversableOnce[(K, U)]) :M[U] =
		((this.asInstanceOf[M[U]]) /: xs.seq)(_.asInstanceOf[M[U]] + _)





	override def filterKeys(p :K => Boolean) :FitMap[K, V] = new FilteredKeysView[K, V](repr, p)

	override def mapValues[C](f :V => C) :FitMap[K, C] = new MappedValuesView[K, V, C](repr, f)



	override def keySet :ValSet[K] = new KeySetView[K](repr)

	@unspecialized
	override def keys :FitIterable[K] = keySet

	override def keysIterator :FitIterator[K] = new MappedIterator[(K, V), K](FitMap.entryKey)(iterator)

	override def valuesIterator :FitIterator[V] = new MappedIterator[(K, V), V](FitMap.entryValue)(iterator)


	override def values :FitIterable[V] = new ValuesView[V](repr)

	protected[this] override def newBuilder :FitBuilder[(K, V), M[V]] = ???

//	override def toSeq :FitSeq
}










abstract class FitMapFactory[
	M[@specialized(KeyTypes) K, @specialized(ValueTypes) +V] <:
		FitMap[K, V] with MapSpecialization[K, V, ({ type L[+T] = M[K, T]})#L]]
{


	def apply[@specialized(KeyTypes) K, @specialized(ValueTypes) V](kvs :(K, V)*) :M[K, V] =
		(newBuilder[K, V] ++= kvs).result()


	def empty[@specialized(KeyTypes) K, @specialized(ValueTypes) V] :M[K, V]

	def erased[K, V] :M[K, V] = Empty.asInstanceOf[M[K, V]]

	def emptyOf[K :RuntimeType, V :RuntimeType] :M[K, V] = EmptyMap[K, V]()

	private[this] final val Empty :M[Any, Any] = empty[Any, Any]

	
	def newBuilder[@specialized(KeyTypes) K, @specialized(ValueTypes) V] :FitBuilder[(K, V), M[K, V]] =
		new FitMapBuilder[K, V, M](empty[K, V])

	def builder[K :RuntimeType, V :RuntimeType] :FitBuilder[(K, V), M[K, V]] =
		new FitMapBuilder[K, V, M](emptyOf[K, V])




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

	private type Builder[K, V] = FitBuilder[(K, V), M[K, V]]

}






private[maps] class FitMapBuilder[K, V,
                                  M[X, +Y] <: FitMap[X, Y] with MapSpecialization[X, Y, ({ type L[+T] = M[X, T]})#L]]
                                 (empty :M[K, V])
	extends FitBuilder[(K, V), M[K, V]]
{
	private[this] var elems = empty

	override def +=(elem :(K, V)) :this.type = { elems = (elems + elem).asInstanceOf[M[K, V]]; this }

	override def ++=(xs :FitTraversableOnce[(K, V)]) :this.type = { elems = (elems ++ xs).asInstanceOf[M[K, V]]; this}

	override def ++=(xs :TraversableOnce[(K, V)]) :this.type = { elems = (elems ++ xs).asInstanceOf[M[K, V]]; this }

	override def result() :M[K, V] = elems

	override def clear() :Unit = elems = empty
}
