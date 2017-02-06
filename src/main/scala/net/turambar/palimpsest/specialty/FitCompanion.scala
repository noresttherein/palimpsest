package net.turambar.palimpsest.specialty


import scala.annotation.tailrec

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import scala.collection.generic.{CanBuildFrom, GenTraversableFactory, GenericCompanion}
import scala.collection._




/** Base trait for companion objects to specialized collections.
  * It is a root of seeming blown up hierarchy of companion traits, but this one is needed as
  * it is covariant with regard to built collection type, which means that can be returned
  * by methods of those sequences, and overriden in more specific sequences
  * (invariance towards built type prevents that as part of non `private[this]` api).
  *
  * @author Marcin Mościcki
  */
trait FitCompanion[+S[@specialized(Elements) X] <: FitIterable[X]]
		extends GenericCompanion[S]
{ factory =>
	
	/** An empty, unspecialized instance of S[_] and a valid instance of `S[T]` for all immutable (variant) collections `S`. */
	val Empty :S[Nothing] //= empty[Nothing]

	
	/** An empty collection `S[E]` instance specialized in regard to `E`. */
	override def empty[@specialized(Elements) E]: S[E] =
		newBuilder[E].result()


	/** Create a new instance containing the given elements.
	  * '''Do not use it with empty argument list''' - not only [[net.turambar.palimpsest.specialty.FitCompanion#empty empty[E]] will be more efficient, but due to
	  * a bug in scala up to 2.11.8 such call won't be specialized if `apply` method is overloaded.
	  *
	  * @return a specialized subclass of `S[E]`
	  */
	override def apply[@specialized(Elements) E](elems: E*): S[E] =
		if (elems.isEmpty) empty
		else (newBuilder[E] ++= elems).result()


	def apply[E](elems :TraversableOnce[E])(implicit specializationHint :Specialized[E]) :S[E] = {
		val spec = elems match {
			case it :FitIterator[E] => it.specialization.asInstanceOf[Specialized[E]]
			case items :FitIterable[E] => items.specialization.asInstanceOf[Specialized[E]]
			case _ => specializationHint
		}
		(fitBuilder(spec) ++= elems).result()
	}
	
	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, S[E]] //= specializedBuilder[E]

	/** Builder specialized on `E` if any information about type `E` is available (see [[Specialized]]). */
	def fitBuilder[E :Specialized] :FitBuilder[E, S[E]] = NewBuilder()
	
	protected[this] type SpecializedBuilder[E] = FitBuilder[E, S[E]]
	protected[this] final val NewBuilder :Specialize[SpecializedBuilder] = new Specialize[SpecializedBuilder] {
		override def specialized[@specialized E : Specialized]: SpecializedBuilder[E] = specializedBuilder[E]
	}
//
	def specializedBuilder[@specialized(Elements) E :Specialized] :FitBuilder[E, S[E]]
	
	/** Generic, unspecialized builder of the associated sequence type.
	  * Always prefer [[newBuilder]] or [[fitBuilder]] whenether any information about element type is present.
	  * This method exists specifically as the target of unspecialized calls to [[SpecializedTraversableTemplate#genericBuilder]]
	  * from unspecialized `CanBuildFrom` factories when operating on specialized sequences through standard, unspecialized
	  * scala interfaces. For this reason [[FitBuilder]] provides two methods which can be used to obtain a specialized
	  * version. One is [[FitBuilder#typeHint]], which will try to provide an appropriate specialized version of itself,
	  * the other is [[FitBuilder#source]], which is a multipurpose identifier allowing collections to recognize their 'own'
	  * builders, if they so implement it.
      * @return an erased version of the builder for element type `E`.
	  */
	@deprecated("this is considered internal api. use newBuilder or fitBuilder", "palimpsest")
	def genericBuilder[@specialized(Elements) E] :FitBuilder[E, S[E]] = newBuilder[E]
	

}




/** Companion of all companions of specialized collections. Contains specialized, boring version
  * of various helper traits.
  * @author Marcin Moscicki
  */
object FitCompanion {
	import Specialized.{Fun1, Fun1Vals}
	
	
	
	
	
	/** A specialized version of the `CanBuildFrom` builder factory interface.
	  * Doesn't do any real work, but can be queried from collection methods to 'cheat'
	  * on the dynamic double dispatch in order to perform optimizations.
	  */
	trait CanFitFrom[-From, -E, +To] { this :CanBuildFrom[From, E, To] => //extends CanBuildFrom[From, E, To] {
		def cbf :CanBuildFrom[From, E, To] = this
//		override def apply(from: From): FitBuilder[E, To]
//
//		override def apply(): FitBuilder[E, To]
		def apply(from: From): FitBuilder[E, To]
		
		def apply(): FitBuilder[E, To]

		def copy(from :FitSeq[E]) :To = (apply() ++= from).result()
		
		/** If `true`, `this(from)` takes its builder as is customary from [[FitSeq#genericBuilder]] // [[FitSeq#fitBuilder]]. */
		def honorsBuilderFrom = true
		
		def elementType :Class[_] = specialization.runType
		
		def specialization :Specialized[_]

		def canEqual(that :Any) = that.isInstanceOf[CanFitFrom[_, _, _]]

		override def equals(that :Any) = that match {
			case cbf :CanFitFrom[_, _, _] =>
				(cbf eq this) || cbf.canEqual(this) && canEqual(cbf) && companion==cbf.companion && specialization == cbf.specialization
			case _ => false
		}

		override def hashCode = companion.hashCode * 31 + specialization.hashCode

		override def toString = s"CBF[$specialization]"


		private[specialty] def companion :Any = this
	}
	
	
	

	class CanBreakOut[-From, -E, +To]()(implicit cbf :CanFitFrom[_, E, To])
		extends CanFitFrom[From, E, To] with CanBuildFrom[From, E, To]
	{
		override def apply(from: From): FitBuilder[E, To] = cbf()
		
		override def apply(): FitBuilder[E, To] = cbf()
		
		override def specialization: Specialized[_] = cbf.specialization
		
		override private[specialty] def companion = cbf.companion
		
		override def honorsBuilderFrom: Boolean = false
		
	}
	
	

	

	
}
