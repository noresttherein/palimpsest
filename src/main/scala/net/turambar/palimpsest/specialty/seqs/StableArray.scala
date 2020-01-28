package net.turambar.palimpsest.specialty.seqs

import java.lang.System.arraycopy

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable
import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.{CloneableIterable, FitCompanion, IterableFoundation, SpecializableIterable, StableIterableTemplate}
import net.turambar.palimpsest.specialty.{newArray, ItemTypes}

import scala.annotation.unspecialized


/** A view on a section of an array which contents - at least within this section -
  * can be changed by neither this instance nor any other source.
  */
class StableArray[@specialized(ItemTypes) +E] protected[seqs]
		(final protected[this] val array :Array[E], final protected[palimpsest] val headIdx :Int, final val length :Int)
	extends IterableFoundation[E, StableArray[E]] with CloneableIterable[E, StableArray[E]] //with FitIndexedSeq[E] //enforce the desired linearization
//	   with StableSeq[E] with StableIndexedOverrides[E] with StableIterableTemplate[E, StableArray[E]]
	   with ArrayView[E] with ArrayViewLike[E, StableArray[E]] with SpecializableIterable[E, StableArray]
	   with StableSeq[E] with StableIndexedSeq[E] with StableIterableTemplate[E, StableArray[E]]
{
	@unspecialized
	override def seq :StableArray[E] = this

	@unspecialized
	override def toStableArray :StableArray[E] = this

	@unspecialized
	override def toArrayPlus :ArrayPlus[E] = ArrayPlus.shared(new ArrayBounds[E](array, headIdx, length)(specialization))

	override protected[this] def at(idx: Int): E = array(idx)
	
	@inline
	final override protected def section(from: Int, until: Int): StableArray[E] =
		if (from==until) StableArray.Empty //ConstArray.empty[E](storageType)
		else new StableArray[E](array, headIdx+from, until-from)


//	override def carbon :StableArray[E] = this

	override def clone() :StableArray[E] =
		if (array.length == length)
			this
		else {
			val copy = newArray[E](array.getClass.getComponentType, length)
			arraycopy(array, headIdx, copy, 0, length)
			new StableArray[E](copy, 0, length)
		}
	
	override protected[this] def typeStringPrefix = "StableArray"

	override protected[this] def debugPrefix = "StableArray"

	override def companion: FitCompanion[StableArray] = StableArray
}


/** Factory for immutable, specialized sequences backed by any array.
  * All constructor methods here copy the contents for the created [[StableArray]], including those
  * accepting an array themselves.
  */
object StableArray extends ArrayViewFactory[StableArray] { factory =>

	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[StableArray[_], E, StableArray[E]]): CanBuildFrom[StableArray[_], E, StableArray[E]] =
		fit.cbf



	protected[seqs] override def apply[E](contents: ArrayBounds[E]): StableArray[E] = shared(contents.copy)


	protected def using[@specialized(ItemTypes) E](array: Array[E], offset: Int, length: Int): StableArray[E] =
		new StableArray[E](array, offset, length)
	
}





