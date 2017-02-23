package net.turambar.palimpsest.specialty.seqs

import scala.reflect.ClassTag
import net.turambar.palimpsest.specialty.{Elements, IterableSpecialization, SpecializableIterable, arrayFill, ofKnownSize}

import scala.collection.mutable



trait SharedArrayLike[@specialized(Elements) E, +Repr[X]<:SharedArrayLike[X, Repr] with SharedArray[X]]
	extends mutable.SeqLike[E, Repr[E]] with mutable.IndexedSeqLike[E, Repr[E]]
			with ValSeqLike[E, Repr[E]] with ArrayViewLike[E, Repr[E]]
			with SpecializableIterable[E, Repr]
{
	protected[this] override def toCollection(repr: Repr[E]) = repr
	protected[this] override def thisCollection = this.asInstanceOf[Repr[E]]
	override def seq = this.asInstanceOf[Repr[E]]

	override protected[this] def empty = companion.empty[E]

	override def transform(f: (E) => E) = {
		var i = headIdx; val lim = i + length; val a = array
		while(i<lim) {
			a(i) = f(a(i)); i+=1
		}
		this
	}

//	override def positionOf(elem: E, from: Int) = super.positionOf(elem, from)
//
//	override def lastPositionOf(elem: E, end: Int) = super.lastPositionOf(elem, end)

	//todo: these are straight copy&paste from ArrayViewLike, but we need to make them public; think of a place to extract them to.

	override def positionOf(elem: E, from: Int): Int =
		if (from>=length) -1 //also guards against arithmetic overflow on indices
		else {
			var i = headIdx + math.max(from, 0); val e = headIdx+length
			val a = array
			while(i<e && a(i) != elem) i+=1
			if (i==e) -1 else i-headIdx
		}

	override def lastPositionOf(elem: E, end: Int): Int = {
		var i = math.min(end, length-1) + headIdx
		val a = array
		while(i>=headIdx && a(i) != elem) i-=1
		if (i<headIdx) -1 else i-headIdx
	}

}

/**
  * @author Marcin Mościcki
  */
/*
trait SharedArrayLike[@specialized(Elements) E, +Repr[X]<:SharedArrayLike[X, Repr] with SharedArray[X]]
	extends MutableSeqLike[E, Repr[E]] with ArrayViewLike[E, Repr]
{
	override protected[seqs] def arr :Array[E] = array

	override def overwrite: FitBuffer[E] = LentArrayBuffer.upon(array, offset, length)

	override def overwrite(start: Int, length: Int): FitBuffer[E] =
		if (start<0 || length<0 || start+length>this.length)
			throw new IndexOutOfBoundsException(s"$stringPrefix{$size}.overwrite($start, $length)")
		else LentArrayBuffer.upon(array, offset+start, length)




	@inline override protected[this] def set(idx: Int, elem: E): Unit =
		array(idx + offset) = elem



	override def update(idx: Int, elems: TraversableOnce[E]): Unit = elems match {
		case _ if idx < 0 || idx >= length =>
			throw new IndexOutOfBoundsException(idx.toString)
		case _ if ofKnownSize(elems) =>
			if (elems.size > length-idx)
				throw new IndexOutOfBoundsException((idx+elems.size).toString)
			elems.copyToArray(array, offset+idx, length-idx)
		case _ =>
			val e = elems.toIterator //todo :specialization
			var i = offset+idx; val end = offset+length
			while (e.hasNext && i<end) {
				set(i, e.next()); i+=1
			}
			if (e.hasNext)
				throw new IndexOutOfBoundsException(length.toString)
	}



	override def update(fromIndex: Int, value: E, count: Int): Unit =
		if (count > 0)
			if (fromIndex<0 || fromIndex>=length)
				throw new IndexOutOfBoundsException(s"$stringPrefix[$length].update($fromIndex, $value, $count)")
			else
				arrayFill(array, value, offset+fromIndex, offset+fromIndex + (array.length-fromIndex min count))

	
	override def toArray[B >: E](implicit ev: ClassTag[B]): Array[B] =
		if (offset==0 && length==array.length && ev.runtimeClass.isAssignableFrom(storageClass))
			array.asInstanceOf[Array[B]]
		else {
			val res = new Array[B](length)
			Array.copy(array, offset, res, 0, length)
			res
		}

}
*/
