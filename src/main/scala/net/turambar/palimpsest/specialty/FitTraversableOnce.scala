package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.seqs.SharedArray

import scala.annotation.unspecialized
import scala.collection.{GenTraversableOnce, mutable}

/**
  * @author Marcin Mościcki
  */
/** Root trait for specialized collections and iterators.
  * It isn't specialized itself, so avoid using it to actually traverse the elements,
  * preferring [[FitIterable]] and [[FitIterator]] interfaces where specialization is possible.
  * It exists mainly to check
  * @tparam E
  */
trait FitTraversableOnce[@specialized(Elements) +E] extends TraversableOnce[E] { //with FilterMonadic[E, FitTraversableOnce[E]] {
//	protected[this] def mySpecialization :Specialized[E]
	def specialization :Specialized[_<:E] //= mySpecialization

	@unspecialized
	def traverse(f :E=>Unit) :Unit = foreach(f)

//	override def foreach[@specialized(Unit) U](f :E=>U) :Unit

	def hasFastSize :Boolean

	@unspecialized
	def fitIterator :FitIterator[E]

	def head :E

}

object FitTraversableOnce {
	def unapply[E](col :GenTraversableOnce[E]) :Option[FitTraversableOnce[E]] =
		col match {
			case it :FitTraversableOnce[E] => Some(it)
			case arr :mutable.WrappedArray[E] => Some(SharedArray(arr.array))
			case _ => None
		}


}


