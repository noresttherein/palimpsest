package net.turambar.palimpsest.specialty.iterables

import net.turambar.palimpsest.specialty.FitCompanion
import net.turambar.palimpsest.specialty.seqs.{MutableSeq, StableSeq}
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom

import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable, mutable}

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
//trait StableIterable[+E]
//	extends immutable.Iterable[E] with IterableTemplate[E, StableIterable[E]] with SpecializableIterable[E, StableIterable]
//{
//	override def companion :FitCompanion[StableIterable] = StableSeq
//
////	override def seq :StableIterable[E] = this
//}


trait StableIterableOverrides[+E]
	extends immutable.Iterable[E] with IterableTemplate[E, StableIterable[E]] with SpecializableIterable[E, StableIterable]
{
	override def companion :FitCompanion[StableIterable] = StableSeq
}


trait MutableIterableOverrides[E]
	extends mutable.Iterable[E] with IterableTemplate[E, MutableIterable[E]] with SpecializableIterable[E, MutableIterable]
{
	override def companion :FitCompanion[MutableIterable] = MutableSeq
}

/*
object StableIterable extends InterfaceIterableFactory[StableIterable] {
	override protected[this] type RealType[E] = StableSeq[E]

	override protected def default :FitIterableFactory[StableSeq] = StableSeq


	override implicit def canBuildFrom[E](implicit fit :CanFitFrom[StableIterable[_], E, StableIterable[E]]) :CanBuildFrom[StableIterable[_], E, StableIterable[E]] =
		fit.cbf
}
*/
