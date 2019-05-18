package net.turambar.palimpsest.specialty.seqs

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.{CloneableIterable, FitCompanion, FitIterable, IterableFoundation, MutableIterable, SpecializableIterable}
import net.turambar.palimpsest.specialty.{arrayFill, ofKnownSize, Elements, FitTraversableOnce}

import scala.collection.mutable


/** A view on a section of an array as a mutable, specialized sequence. */
trait SharedArray[@specialized(Elements) E]
//	extends mutable.IndexedSeq[E] with mutable.IndexedSeqLike[E, SharedArray[E]]
//			with MutableSeq[E] with MutableSliceLike[E, SharedArray[E]] with ArrayView[E] with ArrayViewLike[E, SharedArray[E]]
//			with SpecializedTraversableTemplate[E, SharedArray]
	extends mutable.IndexedSeq[E] with mutable.IndexedSeqLike[E, SharedArray[E]]//with MutableSeq[E] with ValSeqLike[E, SharedArray[E]]
	   with FitIterable[E] with SpecializableIterable[E, SharedArray] with MutableSeq[E]
	   with ArrayView[E] with ArrayViewLike[E, SharedArray[E]] with ValSeqLike[E, SharedArray[E]] //with SpecializableIterable[E, SharedArray]
//	   with ArrayView[E] with SharedArrayLike[E, SharedArray] with ValSeqLike[E, SharedArray[E]]
	   with CloneableIterable[E, SharedArray[E]]
{
	override def seq :SharedArray[E] = this //todo: introduce MutableIndexedSeq
	override def thisCollection :SharedArray[E] = this


	override protected[palimpsest] def arr :Array[E] = array
	
	override def overwrite: FitBuffer[E] = LentArrayBuffer.upon(array, headIdx, length)
	
	override def overwrite(start: Int, length: Int): FitBuffer[E] =
		if (start<0 || length<0 || start > this.length - length)
			throw new IndexOutOfBoundsException(s"$stringPrefix{$size}.overwrite($start, $length)")
		else new LentArrayBuffer[E](array, headIdx + start, headIdx + length)
	
	

	
	@inline override protected[this] def set(idx: Int, elem: E): Unit =
		array(idx + headIdx) = elem
	
	
	
	override def update(idx: Int, elems: TraversableOnce[E]): Unit =
		if (idx<0) throw new IndexOutOfBoundsException(s"$stringPrefix<$length>.update($idx, ...)")
		else if (idx < length)
			elems.copyToArray(array, headIdx + idx, length-idx)


	override def update(fromIndex: Int, value: E, count: Int): Unit =
		if (count > 0)
			if (fromIndex < 0 || fromIndex >= length - count)
				throw new IndexOutOfBoundsException(s"$stringPrefix[$length].update($fromIndex, ???, $count)")
			else
				arrayFill(array, value, headIdx + fromIndex, headIdx + fromIndex + count)
	
	
	override def toArray[B >: E](implicit ev: ClassTag[B]): Array[B] =
		if (headIdx==0 && length==array.length && (ev.runtimeClass eq storageClass))
			array.asInstanceOf[Array[B]]
		else {
			val res = new Array[B](length)
			Array.copy(array, headIdx, res, 0, length)
			res
		}




//	override protected[this] def empty :Repr[E] = companion.empty[E]

	override def transform(f: E => E) :this.type = {
		var i = headIdx; val lim = i + length; val a = array
		while(i<lim) {
			a(i) = f(a(i)); i+=1
		}
		this
	}

	//	override def offsetOf(elem: E, from: Int) = super.offsetOf(elem, from)
	//
	//	override def lastOffsetOf(elem: E, end: Int) = super.lastOffsetOf(elem, end)

	//todo: these are straight copy&paste from ArrayViewLike, but we need to make them public; think of a place to extract them to.
	//todo: remember that super is broken!
	override def offsetOf(elem: E, from: Int): Int =
		if (from>=length) -1 //also guards against arithmetic overflow on indices
		else {
			var i = headIdx + Math.max(from, 0); val e = headIdx+length
			val a = array
			while(i<e && a(i) != elem) i+=1
			if (i==e) -1 else i-headIdx
		}

	override def lastOffsetOf(elem: E, end: Int): Int = {
		var i = Math.min(end, length-1) + headIdx
		val a = array
		while(i>=headIdx && a(i) != elem) i-=1
		if (i<headIdx) -1 else i-headIdx
	}


	override def companion: FitCompanion[SharedArray] = SharedArray
	
	override protected[this] def typeStringPrefix = "SharedArray"
}




/** Factory of mutable, specialized sequences backed by corresponding arrays. */
object SharedArray extends ArrayViewFactory[SharedArray] {


	override implicit def canBuildFrom[E](implicit fit: CanFitFrom[SharedArray[_], E, SharedArray[E]]): CanBuildFrom[SharedArray[_], E, SharedArray[E]] =
		fit.cbf

	object implicits {
		@inline final def wrapArrayInView[@specialized E](array :Array[E]) :SharedArray[E] =
			new MutableArrayView[E](array, 0, array.length)
	}


	@inline
	final override protected def using[@specialized(Elements) E](array: Array[E], offset: Int, length: Int): SharedArray[E] =
		new MutableArrayView[E](array, offset, length)


	private[seqs] class MutableArrayView[@specialized(Elements) E](protected[this] final val array :Array[E], protected[palimpsest] final val headIdx :Int, final val length :Int)
		extends IterableFoundation[E, SharedArray[E]] with SharedArray[E]
	{
		override protected def section(from: Int, until: Int): SharedArray[E] =
			new MutableArrayView[E](array, headIdx + from, until-from)

		override def companion: FitCompanion[SharedArray] = SharedArray
	}
	
}

