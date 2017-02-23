package net.turambar.palimpsest.specialty.seqs

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.FitIterable.IterableFoundation
import net.turambar.palimpsest.specialty.seqs.StableSeq.MakeStableIndexed
import net.turambar.palimpsest.specialty.{ArrayBounds, Elements, FitCompanion, SpecializableIterable}

import scala.annotation.unspecialized


/** A view on a section of an array which contents - at least within this section -
  * can be changed by neither this instance or any other source.
  */
class StableArray[@specialized(Elements) +E] protected[seqs]
		(final protected[this] val array :Array[E], final protected[seqs] val headIdx :Int, final val length :Int)
	extends IterableFoundation[E, StableArray[E]] with StableSeq[E] with MakeStableIndexed[E]
			with ArrayView[E] with ArrayViewLike[E, StableArray[E]]
			with SpecializableIterable[E, StableArray]
{
	@unspecialized
	override def seq = this
	
	override protected[this] def at(idx: Int): E = array(idx)
	
	@inline
	final override protected def section(from: Int, until: Int): StableArray[E] =
		if (from==until) StableArray.Empty //ConstArray.empty[E](storageType)
		else new StableArray[E](array, headIdx+from, until-from)
	
	
	override def clone(): StableArray[E] =
		if (array.length>0 && array.length / length >= 2)
			StableArray.view(toArray, 0, length)
		else this
	
	override protected[this] def typeStringPrefix = "StableArray"
	
	override def companion: FitCompanion[StableArray] = StableArray
}


/** Factory for immutable, specialized sequences backed by any array.
  * All constructor methods here copy the contents for the created [[StableArray]], including those
  * accepting an array themselves.
  */
object StableArray extends ArrayViewFactory[StableArray] { factory =>
	
	protected[seqs] override def apply[E](contents: ArrayBounds[E]): StableArray[E] = shared(contents.copy)

	protected def using[@specialized(Elements) E](array: Array[E], offset: Int, length: Int): StableArray[E] =
		new StableArray[E](array, offset, length)
	
	
	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[StableArray[_], E, StableArray[E]]): CanBuildFrom[StableArray[_], E, StableArray[E]] =
		fit.cbf
}





