package net.turambar.palimpsest.specialty

import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericTraversableTemplate}
import scala.collection.mutable

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom



/**
  * @author Marcin Mościcki
  */
trait MutableSeq[@specialized(Elements) E]
	extends FitSeq[E] with MutableSeqLike[E, MutableSeq[E]]
	        with mutable.IndexedSeq[E]
	        with SpecializedTraversableTemplate[E, MutableSeq]
{

	override def seq: MutableSeq[E] = this

	override def companion: FitCompanion[MutableSeq] = MutableSeq

}


object MutableSeq extends InterfaceIterableFactory[MutableSeq] {
	protected[this] type RealType[@specialized(Elements) X] = SharedArray[X]

	override protected[this] final def default: FitIterableFactory[SharedArray] = SharedArray

	def of[E <: AnyVal :Specialized](size :Int) :MutableSeq[E] =
		SharedArray(Specialized.erasedArray[E](size))

	def of[E :Specialized](size :Int, value :E) :MutableSeq[E] =
		SharedArray(arrayFill(Specialized.erasedArray[E](size), value))
	

	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[MutableSeq[_], E, MutableSeq[E]]): CanBuildFrom[MutableSeq[_], E, MutableSeq[E]] =
		fit.cbf
}
