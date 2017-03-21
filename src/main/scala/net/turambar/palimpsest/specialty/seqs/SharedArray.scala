package net.turambar.palimpsest.specialty.seqs

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.IterableFoundation
import net.turambar.palimpsest.specialty.{Elements, FitCompanion, FitTraversableOnce, SpecializableIterable, arrayFill, ofKnownSize}

import scala.collection.mutable


/** A view on a section of an array as a mutable, specialized sequence. */
trait SharedArray[@specialized(Elements) E]
//	extends mutable.IndexedSeq[E] with mutable.IndexedSeqLike[E, SharedArray[E]]
//			with MutableSeq[E] with MutableSliceLike[E, SharedArray[E]] with ArrayView[E] with ArrayViewLike[E, SharedArray[E]]
//			with SpecializedTraversableTemplate[E, SharedArray]
	extends mutable.IndexedSeq[E] with MutableSeq[E] with ValSeqLike[E, SharedArray[E]]
			with ArrayView[E] with SharedArrayLike[E, SharedArray]
{
	override protected[seqs] def arr :Array[E] = array
	
	override def overwrite: FitBuffer[E] = LentArrayBuffer.upon(array, headIdx, length)
	
	override def overwrite(start: Int, length: Int): FitBuffer[E] =
		if (start<0 || length<0 || start+length>this.length)
			throw new IndexOutOfBoundsException(s"$stringPrefix{$size}.overwrite($start, $length)")
		else LentArrayBuffer.upon(array, headIdx+start, length)
	
	

	
	@inline override protected[this] def set(idx: Int, elem: E): Unit =
		array(idx + headIdx) = elem
	
	
	
	override def update(idx: Int, elems: TraversableOnce[E]): Unit =
		if (idx<0) throw new IndexOutOfBoundsException(s"$stringPrefix<$length>.update($idx, ...)")
		else if (idx<length)
			elems.copyToArray(array, headIdx+idx, length-idx)


	override def update(fromIndex: Int, value: E, count: Int): Unit =
		if (count > 0)
			if (fromIndex<0 || fromIndex>=length)
				throw new IndexOutOfBoundsException(s"$stringPrefix[$length].update($fromIndex, $value, $count)")
			else
				arrayFill(array, value, headIdx+fromIndex, headIdx+fromIndex + (array.length-fromIndex min count))
	
	
	override def toArray[B >: E](implicit ev: ClassTag[B]): Array[B] =
		if (headIdx==0 && length==array.length && ev.runtimeClass.isAssignableFrom(storageClass))
			array.asInstanceOf[Array[B]]
		else {
			val res = new Array[B](length)
			Array.copy(array, headIdx, res, 0, length)
			res
		}
	
	
	
	
	override def companion: FitCompanion[SharedArray] = SharedArray
	
	override protected[this] def typeStringPrefix = "SharedArray"
}




/** Factory of mutable, specialized sequences backed by corresponding arrays. */
object SharedArray extends ArrayViewFactory[SharedArray] {


	
	
	@inline
	final override protected def using[@specialized(Elements) E](array: Array[E], offset: Int, length: Int): SharedArray[E] =
		new DopeArray[E](array, offset, length)


	private[seqs] class DopeArray[@specialized(Elements) E](protected[this] final val array :Array[E], protected[seqs] final val headIdx :Int, final val length :Int)
		extends IterableFoundation[E, SharedArray[E]] with SharedArray[E]
	{
		override protected def section(from: Int, until: Int): SharedArray[E] =
			new DopeArray[E](array, headIdx + from, until-from)

		override def companion: FitCompanion[SharedArray] = SharedArray
	}
	

	override implicit def canBuildFrom[E](implicit fit: CanFitFrom[SharedArray[_], E, SharedArray[E]]): CanBuildFrom[SharedArray[_], E, SharedArray[E]] =
		fit.cbf
}

