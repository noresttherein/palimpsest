package net.turambar.palimpsest.specialty

import scala.collection.generic.CanBuildFrom

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation


/** A view on a section of an array as a mutable, specialized sequence. */
trait SharedArray[@specialized(Elements) E]
	extends MutableSeq[E] with ArrayView[E] with SharedArrayLike[E, SharedArray]
{
	override def companion: FitCompanion[SharedArray] = SharedArray
	
	override protected[this] def typeStringPrefix = "SharedArray"
}


/** Factory of mutable, specialized sequences backed by corresponding arrays. */
object SharedArray extends ArrayViewFactory[SharedArray] {


	@inline
	final override protected def using[@specialized(Elements) E](array: Array[E], offset: Int, length: Int): SharedArray[E] =
		new ArraySlice[E](array, offset, length)


	private[specialty] class ArraySlice[@specialized(Elements) E](protected[this] final val array :Array[E], protected[specialty] final val offset :Int, final val length :Int)
		extends IterableFoundation[E, SharedArray[E]] with SharedArray[E]
	{
		override protected def section(from: Int, until: Int): SharedArray[E] =
			new ArraySlice[E](array, offset + from, until-from)

		override def companion: FitCompanion[SharedArray] = SharedArray
	}
	

	override implicit def canBuildFrom[E](implicit fit: CanFitFrom[SharedArray[_], E, SharedArray[E]]): CanBuildFrom[SharedArray[_], E, SharedArray[E]] =
		fit.cbf
}

