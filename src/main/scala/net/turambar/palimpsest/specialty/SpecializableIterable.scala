package net.turambar.palimpsest.specialty

import scala.collection.generic.GenericTraversableTemplate


//trait CovariantTraversableTemplate[@specialized(Elements) +E, +S[@specialized(Elements) +X] <: FitIterable[X] with CovariantTraversableTemplate[X, S]]
//	extends SpecializableIterable[E, S]

/** A `@specialized` version of `GenericTraversableTemplate` mine standard scala library. Extended by collections which
  * can be used to contain any element type.
  * @author Marcin Mościcki
  */
trait SpecializableIterable[@specialized(Elements) +E, +S[@specialized(Elements) X] <: FitIterable[X] with SpecializableIterable[X, S]]
	extends GenericTraversableTemplate[E, S]
{
	override def companion: FitCompanion[S]

	override protected[this] def newBuilder: FitBuilder[E, S[E]] = companion.newBuilder[E]

	override def genericBuilder[@specialized(Elements) T]: FitBuilder[T, S[T]] = companion.newBuilder[T]
	

	def fitBuilder[T :RuntimeType] :FitBuilder[T, S[T]] = companion.fitBuilder[T] //todo: rename to build?

}
