package net.turambar.palimpsest.specialty

import scala.collection.generic.GenericTraversableTemplate




/**
  * @author Marcin Mościcki
  */
trait SpecializedTraversableTemplate[@specialized(Elements) +E, +S[@specialized(Elements) X] <: FitIterable[X] with SpecializedTraversableTemplate[X, S]]
	extends GenericTraversableTemplate[E, S] //with GenTraversable[E]
{
//	override def foreach[@specialized(Unit) U](f: (E) => U): Unit

//	override def head: E

	override def companion: FitCompanion[S]

	override protected[this] def newBuilder: FitBuilder[E, S[E]] = companion.newBuilder[E]

	override def genericBuilder[@specialized(Elements) T]: FitBuilder[T, S[T]] = companion.genericBuilder[T]
	
	/** A specialized variant of [[newBuilder]], which will pick the specialization of the builder and built sequence based
	  * on locally known specialization for `T`, but backed by an array with component type of the implicitly available class
	  * for `E`, making it possible to create 'mismatched' (but working) instances.
	  * Prefer [[fitBuilder]] whenever possible.
	  */
	@deprecated("use fitBuilder", "palimpsest")
	def specializedBuilder[@specialized(Elements) T :Specialized] :FitBuilder[T, S[T]] = companion.specializedBuilder[T]
	

	def fitBuilder[T :Specialized] :FitBuilder[T, S[T]] = companion.fitBuilder[T]

}
