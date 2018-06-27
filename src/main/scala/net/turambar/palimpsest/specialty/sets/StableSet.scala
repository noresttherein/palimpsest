package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.EmptyIterable
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitCompanion, FitTraversableOnce, ImplementationIterableFactory, Specialize, Specialized}

import scala.collection.generic.CanBuildFrom
import scala.collection.{GenTraversableOnce, SetLike, immutable}

/**
  * @author Marcin Mościcki
  */
trait StableSet[@specialized(Elements) E] extends ValSet[E] with immutable.Set[E] with SpecializableSet[E, StableSet] {
	override def companion :FitCompanion[StableSet] = StableSet
	override def stable :StableSet[E] = this

//	override def mutable :MutableSet[E] = MutableSet.from(this)
//	override def clone() = repr
}


object StableSet extends ImplementationIterableFactory[StableSet] {
	type Ordered[@specialized(Elements) E] = StableOrderedSet[E]

	final val Ordered = OrderedSet.Stable

//	final val Empty :StableSet[Nothing] = new EmptyIterable[Nothing, ValSet[Nothing]] with StableSet[Nothing] {
//		override def stable: StableSet[Nothing] = this
//		override def mutable :MutableSet[Nothing] = MutableSet.empty
//
//		override def contains(elem: Nothing): Boolean = false
//		override def +(elem: Nothing): ValSet[Nothing] =
//			throw new UnsupportedOperationException(s"FitSet[Nothing] + $elem")
//		override def -(elem: Nothing): ValSet[Nothing] = this
//		override def toString = "FitSet.Empty"
//	}

	def single[@specialized(Elements) E](elem :E) :StableSet[E] = ???

	override def empty[@specialized(Elements) E] :StableSet[E] = ???

	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, StableSet[E]] = ???

	override def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, StableSet[E]] = ???

	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[StableSet[_], E, StableSet[E]]): CanBuildFrom[StableSet[_], E, StableSet[E]] =
		fit.cbf


	type SetBuilder[@specialized(Elements) E] = FitBuilder[E, StableSet[E]]

	final private val SetBuilder = new Specialize.Distinct[SetBuilder] {
		override def forBoolean: SetBuilder[Boolean] = BooleanSet.newBuilder
		override def forByte: SetBuilder[Byte] = ByteSet.newBuilder
		override def forShort :SetBuilder[Short] = ShortSet.newBuilder
		override def forInt: SetBuilder[Int] = IntSet.newBuilder
		override def forLong :SetBuilder[Long] = DirectLongSet.newBuilder
		override def forFloat :SetBuilder[Float] = FloatSet.newBuilder
		override def forDouble :SetBuilder[Double] = DoubleSet.newBuilder
		override def forChar :SetBuilder[Char] = CharSet.newBuilder

		override def specialized[@specialized E : Specialized]: SetBuilder[E] = ???
	}

	final private val EmptySet = new Specialize.Distinct[StableSet] {
		override def forBoolean = BooleanSet.Empty
		override def forByte = ByteSet.Empty
		override def forShort = ShortSet.Empty
		override def forInt = IntSet.Empty
		override def forLong = DirectLongSet.Empty
		override def forChar = CharSet.Empty
		override def forFloat = FloatSet.Empty
		override def forDouble = DoubleSet.Empty
		override def specialized[@specialized E : Specialized]: StableSet[E] = ???
	}



}
