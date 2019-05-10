package net.turambar.palimpsest.specialty


import scala.annotation.tailrec

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import scala.collection.generic.{CanBuildFrom, GenTraversableFactory, GenericCompanion}
import scala.collection._

import net.turambar.palimpsest.specialty.seqs.FitSeq




/** Base trait for companion objects to specialized collections.
  * It is a root of seeming blown up hierarchy of companion traits, but this one is needed as
  * it is covariant with regard to built collection type, which means that can be returned
  * by methods of immutable collections, and overridden in more specific collections
  * (invariance towards built type prevents that as part of non `private[this]` api).
  *
  * @author Marcin Mościcki
  */
trait FitCompanion[+S[@specialized(Elements) X] <: FitIterable[X]]
	extends GenericCompanion[S]
{ factory =>
	
	/** An empty, unspecialized instance of S[_] and a valid instance of `S[T]` for all immutable (variant) collections `S`. */
//	val Empty :S[Nothing] //= empty[Nothing]
	
	protected[this] final val NewEmpty :Specialize[S] = new Specialize[S] {
		override def specialized[@specialized E : RuntimeType]: S[E] = empty[E]
	}
	
	/** An empty collection `S[E]` instance specialized in regard to `E` as described by
	  * implicit specialization information.
	  */
	def emptyOf[E :RuntimeType] :S[E] = NewEmpty()

	/** An empty collection `S[E]` instance specialized in regard to `E`.
	  * Note that this method can rely on local specialization context only.
	  * Consider using [[emptyOf]] which takes an implicit parameter and can create
	  * a properly specialized instance in any context where one is available.
	  */
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


//	def apply[E](elems :TraversableOnce[E])(implicit specializationHint :Specialized[E]) :S[E] = {
//		val spec = elems match {
//			case it :FitIterator[E] => it.specialization.asInstanceOf[Specialized[E]]
//			case items :FitIterable[E] => items.specialization.asInstanceOf[Specialized[E]]
//			case _ => specializationHint
//		}
//		(fitBuilder(spec) ++= elems).result()
//	}
	
	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, S[E]] //= specializedBuilder[E]

	/** Builder specialized on `E` if any information about type `E` is available (see [[RuntimeType]]). */
	def fitBuilder[E :RuntimeType] :FitBuilder[E, S[E]] = NewBuilder() //todo: rename to builder
	
	protected[this] type SpecializedBuilder[E] = FitBuilder[E, S[E]]

	protected[this] final val NewBuilder :Specialize[SpecializedBuilder] = new Specialize[SpecializedBuilder] {
		override def specialized[@specialized E : RuntimeType]: SpecializedBuilder[E] = newBuilder[E] //specializedBuilder[E]
	}
//
//	def specializedBuilder[@specialized(Elements) E :Specialized] :FitBuilder[E, S[E]] = newBuilder[E]
	


}




/** Companion of all companions of specialized collections. Contains specialized, boring version
  * of various helper traits.
  * @author Marcin Moscicki
  */
object FitCompanion {
	import RuntimeType.{Fun1, Fun1Vals}
	
	
	
	
	
	/** A specialized version of the `CanBuildFrom` builder factory interface.
	  * Can be queried from collection methods to 'cheat' on the dynamic double dispatch in order to perform optimizations.
	  */
	trait CanFitFrom[-From, -E, +To] { outer :CanBuildFrom[From, E, To] => //extends CanBuildFrom[From, E, To] {
		private[this] type Builder[X] = FitBuilder[X, To]
		private[this] type MapFun[X] = X => E

		private[this] final val mapper = new Specialize.With2[Builder, MapFun, Specialize.Const[FitBuilder[E, To]]#T]{
			override def specialized[@specialized X :RuntimeType](f :X => E, builder :FitBuilder[E, To]) =
				builder.mapInput(f)
		}

		def cbf :CanBuildFrom[From, E, To] = this
//		override def apply(from: From): FitBuilder[E, To]
//
//		override def apply(): FitBuilder[E, To]
		def apply(from: From): FitBuilder[E, To]
		
		def apply(): FitBuilder[E, To]

		def copy(from :FitSeq[E]) :To = (apply() ++= from).result()

		def mapped[O](from :From, f :O => E) :FitBuilder[O, To]

		def mapped[O :RuntimeType](f :O => E) :FitBuilder[O, To]

		def mapping[O](from :From with FitIterable[O], f :O => E) :FitBuilder[O, To] =
			mapper[O](f, (this :CanFitFrom[From, E, To]).apply(from))(from.specialization.asInstanceOf[RuntimeType[O]])

		def mapping[O :RuntimeType](f :O => E) :FitBuilder[O, To] =
			mapper(f, (this :CanFitFrom[From, E, To]).apply())

		/** If `true`, `this(from)` takes its builder, as is customary, from
		  * [[SpecializableIterable#genericBuilder]] / [[SpecializableIterable#fitBuilder]].
		  */
		def honorsBuilderFrom = true
		
		def elementType :Class[_] = specialization.runType
		
		def specialization :RuntimeType[_]

		def canEqual(that :Any) :Boolean = that.isInstanceOf[CanFitFrom[_, _, _]]

		override def equals(that :Any) :Boolean = that match {
			case cbf :CanFitFrom[_, _, _] =>
				(cbf eq this) || cbf.canEqual(this) && canEqual(cbf) && companion==cbf.companion && specialization == cbf.specialization
			case _ => false
		}

		override def hashCode :Int = companion.hashCode * 31 + specialization.hashCode

		override def toString = s"CBF[$specialization]"


		private[specialty] def companion :Any = this
	}
	



	class CanBreakOut[-From, -E, +To]()(implicit cbf :CanFitFrom[_, E, To])
		extends CanFitFrom[From, E, To] with CanBuildFrom[From, E, To]
	{
		override def apply(from: From): FitBuilder[E, To] = cbf()
		
		override def apply(): FitBuilder[E, To] = cbf()

		override def mapped[O](from :From, f :O => E) :FitBuilder[O, To] = from match {
			case fit :FitTraversableOnce[_] => cbf.mapped(f)(fit.specialization.asInstanceOf[RuntimeType[O]])

			case _ => cbf().mapInput(f)
		}

		override def mapped[O :RuntimeType](f :O => E) :FitBuilder[O, To] = cbf.mapped(f)


		override def mapping[O](from :From with FitIterable[O], f :O => E) :FitBuilder[O, To] =
			cbf.mapped(f)(from.specialization.asInstanceOf[RuntimeType[O]])

		override def mapping[O :RuntimeType](f :O => E) :FitBuilder[O, To] =
			cbf.mapped(f)

		override def specialization: RuntimeType[_] = cbf.specialization
		
		override private[specialty] def companion = cbf.companion
		
		override def honorsBuilderFrom: Boolean = false
		
	}
	
	

	

	
}
