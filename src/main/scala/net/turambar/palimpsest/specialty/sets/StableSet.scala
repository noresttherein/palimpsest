package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitTraversableOnce, ImplementationIterableFactory, Specialized}

import scala.collection.generic.CanBuildFrom
import scala.collection.{GenTraversableOnce, SetLike, immutable}

/**
  * @author Marcin Mościcki
  */
trait StableSet[@specialized(Elements) E] extends FitSet[E] with immutable.Set[E] with SpecializableSet[E, StableSet] {
	override def companion = StableSet
	override def stable = this
}


object StableSet extends ImplementationIterableFactory[StableSet] {
	override def empty[@specialized(Elements) E] :StableSet[E] = ???

	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, StableSet[E]] = ???

	override def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, StableSet[E]] = ???

	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[StableSet[_], E, StableSet[E]]): CanBuildFrom[StableSet[_], E, StableSet[E]] =
		fit.cbf


/*
	class StableSetBuilder[@specialized(Elements) E](set :MutableSet[E]) extends FitBuilder[E, StableSet[E]] {

		override private[specialty] def addOne :E => Unit = e => set += e
		override private[specialty] def addMany :TraversableOnce[E] => Unit = xs => set ++= xs
		override def build :()=>StableSet[E] = ??? //set.stable

		override def +=(elem1: E, elem2: E, elems: E*) :this.type =
			{ set += (elem1, elem2, elems:_*); this }

		override def ++=(xs: FitTraversableOnce[E]) :this.type = { set ++= xs; this }

		override def ++=(xs: TraversableOnce[E]) : this.type = { set ++= xs; this }

		override def +=(elem: E): this.type = { set += elem; this }

		override def result(): StableSet[E] = ??? //set.stable

		override def clear(): Unit = set.clear()

		override def count: Int = set.size

	}
*/
}
