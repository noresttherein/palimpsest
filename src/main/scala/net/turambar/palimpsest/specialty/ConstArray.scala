package net.turambar.palimpsest.specialty

import scala.collection.generic.CanBuildFrom

import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation


/** A view on a section of an array which contents - at least within this section -
  * can be changed by neither this instance or any other source.
  */
class ConstArray[@specialized(Elements) +E] protected[specialty]
		(final protected[this] val array :Array[E], final protected[specialty] val offset :Int, final val length :Int)
	extends IterableFoundation[E, ConstArray[E]] with ConstSeq[E] with ArrayView[E] with ArrayViewLike[E, ConstArray] //with ConstSeq[E] //with ConstSeqLike[E, ConstArray[E]]
	        with FitSeqLike[E, ConstArray[E]] //with SpecializedTraversableTemplate[E, ConstArray]
{
	
	
	override protected[this] def at(idx: Int): E = array(idx)
	
	@inline
	final override protected def section(from: Int, until: Int): ConstArray[E] =
		if (from==until) ConstArray.Empty //ConstArray.empty[E](storageType)
		else new ConstArray[E](array, offset+from, until-from)
	
	
	override def copy: ConstArray[E] =
		if (array.length>0 && array.length / length >= 2)
			ConstArray.view(toArray, 0, length)
		else this
	
	override protected[this] def typeStringPrefix = "ConstArray"
	
	override def companion: FitCompanion[ConstArray] = ConstArray
}


/** Factory for immutable, specialized sequences backed by any array.
  * All constructor methods here copy the contents for the created [[ConstArray]], including those
  * accepting an array themselves.
  */
object ConstArray extends ArrayViewFactory[ConstArray] { factory =>
	
	protected[specialty] override def apply[E](contents: ArrayBounds[E]): ConstArray[E] = shared(contents.copy)

	protected def using[@specialized(Elements) E](array: Array[E], offset: Int, length: Int): ConstArray[E] =
		new ConstArray[E](array, offset, length)
	
	
	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ConstArray[_], E, ConstArray[E]]): CanBuildFrom[ConstArray[_], E, ConstArray[E]] =
		fit.cbf
}





