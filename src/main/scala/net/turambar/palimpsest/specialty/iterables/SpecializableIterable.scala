package net.turambar.palimpsest.specialty.iterables

import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitCompanion, RuntimeType}

import scala.collection.generic.GenericTraversableTemplate


/** A `@specialized` version of `GenericTraversableTemplate` from the standard scala library. Extended by collections which
  * can be used to contain any element type.
  * @author Marcin Mościcki
  */
trait SpecializableIterable[@specialized(Elements) +E, +S[@specialized(Elements) X] <: FitIterable[X] with SpecializableIterable[X, S]]
	extends GenericTraversableTemplate[E, S]
{
	override def companion: FitCompanion[S]

	override protected[this] def newBuilder: FitBuilder[E, S[E]] = companion.newBuilder[E]

	/** By default delegates, as per the implementation in `GenericTraversableTemplate`, to `companion.newBuilder`.
	  * This method is specialized due to being public, but calls from non-specialized default scala `CanBuildFrom`
	  * will still unfortunately result in a non-specialized instance. We leave it be, as it will generally happen
	  * only when the collection is used through its standard scala interface, the target collection itself is
	  * not specialized, or type `T` is abstract at the point of calling. In each of the result likely all
	  * other parts of the operation (mapping function, appended collections, etc.) are not specialized either.
	  * This could be potentially resolved by [[net.turambar.palimpsest.specialty.FitBuilder.RetardedFitBuilder]]
	  * which can attempt to guess the runtime type representation based on added elements, but it would
	  * require an additional specialized method for the 'real' builder and be useful only in certain cases.
	  * It is probably better to optimise in final implementation classes on a case-by-case basis.
	  */
	override def genericBuilder[@specialized(Elements) T]: FitBuilder[T, S[T]] = companion.newBuilder[T]


	def fitBuilder[T :RuntimeType] :FitBuilder[T, S[T]] = companion.fitBuilder[T] //todo: rename to build?

}
