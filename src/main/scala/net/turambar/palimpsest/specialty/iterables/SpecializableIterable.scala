package net.turambar.palimpsest.specialty.iterables

import net.turambar.palimpsest.specialty.{ItemTypes, AptBuilder, RuntimeType}

import scala.collection.generic.GenericTraversableTemplate


/** A `@specialized` version of `GenericTraversableTemplate` from the standard scala library. Extended by collections
  * which can be used to contain any element type.
  * @author Marcin Mościcki
  */
trait SpecializableIterable[@specialized(ItemTypes) +E, +S[@specialized(ItemTypes) X] <: AptIterable[X] with GenericTraversableTemplate[X, S]]
	extends GenericTraversableTemplate[E, S]
{
	override def companion: AptCompanion[S]

	override protected[this] def newBuilder: AptBuilder[E, S[E]] = companion.newBuilder[E]

	/** By default delegates, as per the implementation in `GenericTraversableTemplate`, to `companion.newBuilder`.
	  * This method is specialized due to being public, but calls from non-specialized default scala `CanBuildFrom`
	  * will still unfortunately result in a non-specialized instance. We leave it be, as it will generally happen
	  * only when the collection is used through its standard scala interface, the target collection itself is
	  * not specialized, or type `T` is abstract at the point of calling. In each of the result likely all
	  * other parts of the operation (mapping function, appended collections, etc.) are not specialized either.
	  * This could be potentially resolved by [[net.turambar.palimpsest.specialty.AptBuilder.RetardedAptBuilder]]
	  * which can attempt to guess the runtime type representation based on added elements, but it would
	  * require an additional specialized method for the 'real' builder and be useful only in certain cases.
	  * It is probably better to optimise in final implementation classes on a case-by-case basis.
	  */
	override def genericBuilder[@specialized(ItemTypes) T]: AptBuilder[T, S[T]] = companion.newBuilder[T]

	/** An equivalent of [[net.turambar.palimpsest.specialty.iterables.SpecializableIterable#genericBuilder]] which
	  * is not specialized, but relies on implicit type information instead. Similarly to `genericBuilder`, by default
	  * delegates to `this.companion.builder`. It will generally have the same result as `genericBuilder`, but will
	  * additionally be able ro return a specialized instance in presence of a `ClassTag`, `TypeTag` and naturally
	  * `RuntimeType` - at the cost of an additional `invokevirtual` call.
	  */
	def builder[T :RuntimeType] :AptBuilder[T, S[T]] = companion.builder[T]

}

