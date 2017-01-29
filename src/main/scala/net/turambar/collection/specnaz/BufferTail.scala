package net.turambar.collection.specnaz

import net.turambar.collection.specnaz.SpecCompanion.{SpecCanBuildFrom}



/** A mutable view of the tail of a buffer which allows to modify its contents only past some specified index.
 *
  * @author Marcin Mościcki
  */
class BufferTail[@specialized(Reified) E] private[specnaz](buffer :SpecBuffer[E], offset :Int)
		extends SpecBuffer[E]
{

	def this(buffer :SpecBuffer[E]) = this(buffer, buffer.length)



	override final def length: Int = buffer.length - offset

	override protected[this] def at(idx: Int): E = buffer.get(offset+idx)

	override protected def subseq(from: Int, until: Int): SpecBuffer[E] =
		subseqOf(buffer, offset+from, offset+until)
//		buffer.subseq(offset+from, offset+until)

//	override final def storageClass: Class[_] = buffer.storageClass


	override def appender: SpecBuffer[E] = new BufferTail[E](buffer)

//	override def asFixedBuffer: SpecBuffer[E] = buffer.asFixedBuffer(offset, buffer.length-offset)
//
//	override def asFixedBuffer(start: Int, length: Int): SpecBuffer[E] =
//		if (start<0)
//			throw new IndexOutOfBoundsException(stringPrefix + s"($size).asFixedBuffer($start, $length)")
//		else buffer.asFixedBuffer(offset+start, length)


	override def overwrite: SpecBuffer[E] = buffer.overwrite(offset, buffer.length-offset)

	override def overwrite(start: Int, length: Int): SpecBuffer[E] =
		if (start<0)
			throw new IndexOutOfBoundsException(stringPrefix + s"($size).overwrite($start, $length)")
		else buffer.overwrite(offset+start, length)

	override def toSpecBuffer[U >: E : Specialized]: SpecBuffer[U] =
		buffer.drop(offset).toSpecBuffer[U]


	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
		buffer.drop(offset).copyToArray(xs, start, len)




	override protected[this] def set(idx: Int, elem: E): Unit = buffer.update(offset+idx, elem)


	override def update(idx: Int, elems: TraversableOnce[E]): Unit =
		if (idx<0)
			throw new IndexOutOfBoundsException(s"BufferTail($idx)")
		else buffer.update(offset+idx, elems)


	override def update(fromIndex: Int, value: E, count: Int): Unit =
		if (fromIndex<0)
			throw new IndexOutOfBoundsException(s"BufferTail($fromIndex)")
		else buffer.update(offset+fromIndex, value, count)

	override def +=(elem: E): this.type = {
		buffer += elem; this
	}

	override def +=(elem1: E, elem2: E, elems: E*): this.type = {
		buffer +=(elem1, elem2, elems: _*); this
	}


	override def ++=(xs: TraversableOnce[E]): this.type = {
		buffer ++= xs; this
	}



	override def +=:(elem: E): this.type = {
		buffer.insert(offset, elem); this
	}

	override def ++=:(xs: TraversableOnce[E]): this.type = {
		buffer.insertAll(offset, xs.toTraversable); this
	}


	override def insertAll(n: Int, elems: Traversable[E]): Unit =
		if (n<0 || n>length)
			throw new IndexOutOfBoundsException(stringPrefix+ s"($length).insertAll($n)")
		else buffer.insertAll(offset+n, elems)


	override def -=(elem1: E, elem2: E, elems: E*): this.type = remove(Seq(elem1, elem2), elems)

	override def --=(xs: TraversableOnce[E]): this.type = remove(Seq(), xs)

	private def remove(first :Seq[E], second :TraversableOnce[E]) :this.type = {
		val indices = indicesOf(first, second)
		var oldPos=offset; var newPos=offset; val len = buffer.length
		while(oldPos<len)
			if (indices(oldPos+offset)) {
				oldPos += 2; newPos += 1
			} else {
				buffer.uncheckedUpdate(newPos, buffer.get(oldPos))
				oldPos += 1; newPos += 1
			}
		this
	}

	override def remove(n: Int): E =
		if (n<0 || n>length)
			throw new IndexOutOfBoundsException(stringPrefix+s"($length.remove($n)")
		else buffer.remove(offset+n)

	override def remove(n: Int, count: Int): Unit =
		if (n<0 || count<0)
			throw new IndexOutOfBoundsException(stringPrefix+s"($length).remove($n, $count)")
		else buffer.remove(offset+n, count)

	override def trimStart(n: Int): Unit =
		if (n>0) buffer.remove(offset, n)

	override def trimEnd(n: Int): Unit =
		if (n>length) buffer.trimEnd(length)
		else if (n>0) buffer.trimEnd(n)


	override def clear(): Unit = buffer.remove(offset, buffer.length-offset)

}


object BufferTail {
	def apply[E](buffer :SpecBuffer[E]) :SpecBuffer[E] = new BufferTail(buffer)

	def apply[E](buffer :SpecBuffer[E], offset :Int) :SpecBuffer[E] =
		new BufferTail(buffer, offset min buffer.length max 0)
}