package net.turambar.collection.specnaz

import java.util

import net.turambar.collection.specnaz.SpecCompanion.{SpecBuilder, SpecCanBuildFrom}

import scala.collection.{GenTraversableOnce, mutable}
import scala.collection.generic.{GenericCompanion, GenericTraversableTemplate, Subtractable}
import scala.reflect.ClassTag


/**
  * @author Marcin Mościcki
  */
trait ArraySliceLike[@specialized(Reified) E, +Repr[X]<:ArraySliceLike[X, Repr] with ArraySlice[X]]
	extends MutableSeqLike[E, Repr[E]] with SharedArrayLike[E, Repr]
//	extends SharedArray[E] //with SpecSeqLike[E, Repr[E]]
//	with MutableSeq[E] with MutableSeqLike[E, Repr[E]]
//	with GenericSpecializedTraversable[E, Repr]
{


	override def overwrite: SpecBuffer[E] = FixedArrayBuffer.upon(array, offset, length)

	override def overwrite(start: Int, length: Int): SpecBuffer[E] =
		if (start<0 || length<0 || start+length>this.length)
			throw new IndexOutOfBoundsException(s"$stringPrefix{$size}.asFixedBuffer($start, $length)")
		else FixedArrayBuffer.upon(array, offset+start, length)




	@inline override protected[this] def set(idx: Int, elem: E): Unit =
		array(idx + offset) = elem



	override def update(idx: Int, elems: TraversableOnce[E]): Unit = elems match {
		case _ if idx < 0 || idx >= length =>
			throw new IndexOutOfBoundsException(idx.toString)
		case _ if hasFastSize(elems) =>
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



//	override def update(idx: Int, elems: SpecSeq[E]): Unit = ???

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



//	override protected[this] def newBuilder: SpecBuilder[E, Repr[E]] = companion.newBuilder[E] //companion.forClass[E](storageType)

//	override def companion: SpecCompanion[Repr] =
//		throw new NotImplementedError(s"companion method not overriden by $getClass")

}
