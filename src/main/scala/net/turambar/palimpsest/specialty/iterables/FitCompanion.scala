package net.turambar.palimpsest.specialty.iterables

import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitTraversableOnce, RuntimeType, Specialize, SpecializedGeneric}
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.Fun1Vals

import scala.collection.generic.{CanBuildFrom, GenericCompanion}
import scala.reflect.ClassTag




/** Base trait for companion objects to specialized collections.
  * It is a root of seeming blown up hierarchy of companion traits, but this one is needed as
  * it is covariant with regard to built collection type, which means that it can be returned
  * by methods of immutable collections, and overridden in more specific collections
  * (invariance towards built type prevents that as part of non `private[this]` api).
  *
  * @author Marcin Mościcki
  */
trait FitCompanion[+S[@specialized(Elements) X] <: FitIterable[X]]
	extends GenericCompanion[S]
{ factory =>


	/** An empty, erased instance of this collection. */
	def generic[E] :S[E] = empty[E]


	/** An empty collection `S[E]` instance specialized in regard to `E` as described by implicit specialization information. */
	def of[E :RuntimeType] :S[E]

	/** An empty collection `S[E]` instance specialized in regard to `E`. Note that this method can rely on local
	  * specialization context only. Consider using [[of]] if `E` is not known or an implementation dedicated to
	  * a given reference type is desired, as it takes an implicit parameter carrying type information and can create
	  * a properly specialized instance in any context where one is available.
	  */
	override def empty[@specialized(Elements) E]: S[E] //= newBuilder[E].result()

	/** A single element collection. By default delegates to `newBuilder`, but specific companions may decide to
	  * return a specialized singleton subclass.
	  */
	def one[@specialized(Elements) E](elem :E) :S[E] //= (newBuilder += elem).result()



	/** Create a new instance containing the given elements.
	  * '''Do not use it with empty argument list''' - not only [[FitCompanion#empty empty[E]] will be more efficient, but due to
	  * a bug in scala up to 2.11.8 such call won't be specialized if `apply` method is overloaded.
	  *
	  * @return a specialized subclass of `S[E]`
	  */
	override def apply[@specialized(Elements) E](elems: E*): S[E] //=
//		if (elems.isEmpty) empty
//		else (newBuilder[E] ++= elems).result()


//	def apply[E](elems :TraversableOnce[E])(implicit specializationHint :Specialized[E]) :S[E] = {
//		val spec = elems match {
//			case it :FitIterator[E] => it.specialization.asInstanceOf[Specialized[E]]
//			case items :FitIterable[E] => items.specialization.asInstanceOf[Specialized[E]]
//			case _ => specializationHint
//		}
//		(builder(spec) ++= elems).result()
//	}
	
	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, S[E]] //= specializedBuilder[E]

	/** Builder specialized on `E` if any information about type `E` is available (see [[RuntimeType]]). */
	def builder[E :RuntimeType] :FitBuilder[E, S[E]]




	/*  Methods lifted up from GenTraversableFactory. They must be overriden in SpecializableIterableFactory anway, so
	 *  let's leave the implementation to the subclass and keep this a pure interface.
	 */

	def fill[@specialized(Elements) E](n: Int)(elem: => E): S[E]

	def tabulate[@specialized(Fun1Vals) E](n: Int)(f: Int => E): S[E]


	def iterate[@specialized(Fun1Vals) E](start: E, len: Int)(f: E => E): S[E]

}




/** Companion of all companions of specialized collections. Contains specialized, boring version
  * of various helper traits.
  * @author Marcin Moscicki
  */
object FitCompanion {
	
	
	
	
	
	/** A variant of the `CanBuildFrom` builder factory interface adding methods allowing recognition of the target
	  * element type, specialization as well as originating factory. Used to permit collection classes to optimize
	  * the building of the same collection type and offload some common work from them. This is the public interface which
	  * '''does not''' extend `CanBuildFrom`, instead mandating every implementation subclass to mix it in separately.
	  * This is to avoid implicit resolution conflicts caused by having individual specialized `CanFitFrom` implicits
	  * declared in a common base class for collection companions (instead of directly in the companion).
	  * Can be queried from collection methods to 'cheat' on the dynamic double dispatch in order to perform optimizations.
	  */
	trait CanFitFrom[-From, -E, +To] extends SpecializedGeneric { outer :CanBuildFrom[From, E, To] =>
		private[this] type Builder[X] = FitBuilder[X, To]
		private[this] type MapFun[X] = X => E
		//todo: this is a trait, so it isn't really a field. Maybe move it down?
		private[this] final val mapper = new Specialize.With2[Builder, MapFun, Specialize.Const[FitBuilder[E, To]]#T]{
			override def specialized[@specialized X :RuntimeType](f :X => E, builder :FitBuilder[E, To]) =
				builder.mapInput(f)
		}

		def cbf :CanBuildFrom[From, E, To] = this

		def apply(from: From): FitBuilder[E, To]
		
		def apply(): FitBuilder[E, To]

//		def copy(from :FitSeq[E]) :To = (apply() ++= from).result()

		def mapped[O](from :From, f :O => E) :FitBuilder[O, To]

		def mapped[O :RuntimeType](f :O => E) :FitBuilder[O, To]

		def mapping[O](from :From with FitIterable[O], f :O => E) :FitBuilder[O, To] =
			mapper[O](f, (this :CanFitFrom[From, E, To]).apply(from))(from.runtimeType.asInstanceOf[RuntimeType[O]])

		def mapping[O :RuntimeType](f :O => E) :FitBuilder[O, To] =
			mapper(f, (this :CanFitFrom[From, E, To]).apply())

		/** If `true`, `this(from)` takes its builder, as is customary, from
		  * [[SpecializableIterable#genericBuilder]] / [[SpecializableIterable#builder]].
		  */
		def honorsBuilderFrom = true

		def targetClassTag :ClassTag[_] = runtimeType.classTag

		def elementType :Class[_] = runtimeType.runType
		

		def canEqual(that :Any) :Boolean = that.isInstanceOf[CanFitFrom[_, _, _]]

		override def equals(that :Any) :Boolean = that match {
			case cbf :CanFitFrom[_, _, _] =>
				(cbf eq this) || cbf.canEqual(this) && canEqual(cbf) &&
					companion == cbf.companion && runtimeType == cbf.runtimeType
			case _ => false
		}

		override def hashCode :Int = companion.hashCode * 31 + runtimeType.hashCode

		override def toString = s"CFF[$runtimeType]"


		private[specialty] def companion :Any = this
	}
	



/*
	class CanBreakOut[-From, -E, +To]()(implicit cbf :CanFitFrom[_, E, To])
		extends CanFitFrom[From, E, To] with CanBuildFrom[From, E, To]
	{
		override def apply(from: From): FitBuilder[E, To] = cbf()
		
		override def apply(): FitBuilder[E, To] = cbf()

		override def mapped[O](from :From, f :O => E) :FitBuilder[O, To] = from match {
			case fit :FitTraversableOnce[_] => cbf.mapped(f)(fit.runtimeType.asInstanceOf[RuntimeType[O]])

			case _ => cbf().mapInput(f)
		}

		override def mapped[O :RuntimeType](f :O => E) :FitBuilder[O, To] = cbf.mapped(f)


		override def mapping[O](from :From with FitIterable[O], f :O => E) :FitBuilder[O, To] =
			cbf.mapped(f)(from.runtimeType.asInstanceOf[RuntimeType[O]])

		override def mapping[O :RuntimeType](f :O => E) :FitBuilder[O, To] =
			cbf.mapped(f)

		override protected[this] def specialization: RuntimeType[_] = cbf.runtimeType
		
		override private[specialty] def companion = cbf.companion
		
		override def honorsBuilderFrom: Boolean = false
		
	}
*/

	

	

	
}