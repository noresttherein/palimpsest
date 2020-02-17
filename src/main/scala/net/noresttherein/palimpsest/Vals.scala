package net.noresttherein.palimpsest

import net.noresttherein.palimpsest.seqs.SharedArray
import net.noresttherein.palimpsest.iterables.AptIterable
import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.seqs.SharedArray

import scala.annotation.unspecialized
import scala.collection.immutable.ListSet
import scala.collection.{BitSetLike, GenTraversableOnce, IndexedSeqLike, SetLike}
import scala.collection.mutable.WrappedArray


/** A base trait for everything that is specialized. It exists to avoid numerous overrides caused by inheriting
  * declarations from unrelated base types (such as `MutableSet` from `AptIterable` and AptBuilder), especially
  * unresolvable conflicts when inheriting a final definition. It should be considered an implementation detail
  * and possible to change without notice.
  */
trait SpecializedGeneric {

	/** The runtime type this object uses to reference the values it works on. This will in general be some super type
	  * of actual type parameter.
	  */
	def runtimeType :RuntimeType[_] = specialization

	/** Target of `runtimeType` intended to be specialized in the implementing classes. */
	protected[this] def specialization :RuntimeType[_]
}






/** Root trait for specialized collections and iterators.
  * It isn't specialized itself, so avoid using it to actually traverse the elements,
  * preferring [[AptIterable]] and [[AptIterator]] interfaces where specialization is possible.
  * It exists mainly to check
  * @tparam E
  */
//todo: rename to `Vals`
trait Vals[/*@specialized(Elements) */+E] extends TraversableOnce[E] with SpecializedGeneric {
//	protected[this] def specialization :Specialized[E]
	override def runtimeType :RuntimeType[_<:E] = specialization

	protected[this] override def specialization :RuntimeType[E]

	@unspecialized
	def traverse(f :E=>Unit) :Unit //= foreach(f)

//	override def foreach[@specialized(Unit) U](f :E=>U) :Unit

	/**  Declares if this collection is finite and provides reasonably fast (i.e. `o(n)`) `size` method, which additionally
	  *  doesn't affect the state of this object (i.e., doesn't advance an iterator).
	  *  Returning `true` will cause various modification methods of other collections and builders to pre-reserve
	  *  required space or use otherwise more efficient algorithms.
	  */
	def hasFastSize :Boolean

	/** Declares that this collection contains at least {{size}} items.
	  * This can be more efficient than simply comparing it to `this.size`, especially for infinite and stream-like collections.
	  * Note that in case of iterators and other `TraversableOnce` this will likely change the state of the collection.
	  */
	def ofAtLeast(size :Int) :Boolean //= this.size >= size

	/**  Returns `true` if this collection contains at least one element. By default implemented via
	  *  [[net.noresttherein.palimpsest.Vals#ofAtLeast]].
	  */
	override def nonEmpty :Boolean = ofAtLeast(1)

	/** Returns `true` if this is an empty collection. May be faster than calculating its size. */
	override def isEmpty :Boolean = !ofAtLeast(1)


	@unspecialized
	def toIterator :AptIterator[E]
	//todo: do we need it? its the only thing requireing specialization
//	def head :E

}





object Vals {
	def unapply[E](col :GenTraversableOnce[E]) :Option[Vals[E]] =
		col match {
			case it :Vals[E] => Some(it)
			case arr :WrappedArray[E] => Some(SharedArray(arr.array))
			case _ => None
		}

	/** Marker trait implemented by finite collections providing reasonably fast `size` method. */
	trait OfKnownSize extends TraversableOnce[Any] { this :Vals[Any] =>
		override def hasFastSize = true
		override def hasDefiniteSize = true
		override def ofAtLeast(items :Int) :Boolean = size >= items
		override def isEmpty :Boolean = size==0
		override def nonEmpty :Boolean = size > 0
	}

	@inline
	private[palimpsest] final def ofKnownSize[T](col :GenTraversableOnce[T]) =  col match {
		case fit :Vals[T] => fit.hasFastSize
		case _ :IndexedSeqLike[_, _] => true
		case _ :ListSet[_] => col.isEmpty
		case _ :BitSetLike[_] => false
		case _ :SetLike[_, _] => true
		case _ => col.isEmpty
	}

}


