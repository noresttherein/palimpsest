package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.seqs.SharedArray

import scala.annotation.unspecialized
import scala.collection.immutable.ListSet
import scala.collection.{BitSetLike, GenTraversableOnce, IndexedSeqLike, SetLike, mutable}

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
	def traverse(f :E=>Unit) :Unit //= foreach(f)

//	override def foreach[@specialized(Unit) U](f :E=>U) :Unit

	def hasFastSize :Boolean
	def ofAtLeast(size :Int) :Boolean //= this.size >= size
	override def nonEmpty = ofAtLeast(1)
	override def isEmpty = !ofAtLeast(1)


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

	trait OfKnownSize extends TraversableOnce[Any] { this :FitTraversableOnce[Any] =>
		override def hasFastSize = true
		override def hasDefiniteSize = true
		override def ofAtLeast(items :Int) = size >= items
		override def isEmpty = size==0
		override def nonEmpty = size > 0
	}

	@inline
	private[palimpsest] final def ofKnownSize[T](col :GenTraversableOnce[T]) =  col match {
		case fit :FitTraversableOnce[T] => fit.hasFastSize
		case _ :IndexedSeqLike[_, _] => true
		case _ :ListSet[_] => col.isEmpty
		case _ :BitSetLike[_] => false
		case _ :SetLike[_, _] => true
		case _ => col.isEmpty
	}

}


