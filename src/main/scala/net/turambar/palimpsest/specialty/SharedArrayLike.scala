package net.turambar.palimpsest.specialty

import scala.reflect.ClassTag


/**
  * @author Marcin Mościcki
  */
trait SharedArrayLike[@specialized(Elements) E, +Repr[X]<:SharedArrayLike[X, Repr] with SharedArray[X]]
	extends MutableSeqLike[E, Repr[E]] with ArrayViewLike[E, Repr]
{
	override protected[specialty] def arr :Array[E] = array

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
			val e = elems.toIterator
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
