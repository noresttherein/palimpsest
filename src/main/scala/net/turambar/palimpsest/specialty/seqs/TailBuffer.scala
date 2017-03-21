package net.turambar.palimpsest.specialty.seqs

import scala.annotation.unspecialized

import net.turambar.palimpsest.specialty.iterables.IterableFoundation
import net.turambar.palimpsest.specialty.{Elements, FitTraversableOnce, Specialized}


/** A mutable view of the tail of a buffer which allows to modify its contents only past some specified index.
 *
  * @author Marcin Mościcki
  */
class TailBuffer[@specialized(Elements) E] private[seqs](buffer :FitBuffer[E], offset :Int)
		extends IterableFoundation[E, FitBuffer[E]] with FitBuffer[E] with SliceLike[E, FitBuffer[E]]
{

	def this(buffer :FitBuffer[E]) = this(buffer, buffer.length)



	override final def length: Int = buffer.length - offset
	override def hasFastSize = buffer.hasFastSize
	override def ofAtLeast(elems :Int) = elems<=0 || buffer.ofAtLeast(offset+elems)

	override protected[this] def at(idx: Int): E = buffer.get(offset+idx)

	@unspecialized
	override protected def section(from: Int, until: Int): FitBuffer[E] =
		sectionOf(buffer, offset+from, offset+until)


	override def appender: FitBuffer[E] = new TailBuffer[E](buffer)

	@unspecialized
	override def overwrite: FitBuffer[E] = buffer.overwrite(offset, buffer.length-offset)
	
	@unspecialized
	override def overwrite(start: Int, length: Int): FitBuffer[E] =
		if (start<0)
			throw new IndexOutOfBoundsException(stringPrefix + s"($size).overwrite($start, $length)")
		else buffer.overwrite(offset+start, length)

	@unspecialized
	override def toFitBuffer[U >: E : Specialized]: FitBuffer[U] =
		buffer.drop(offset).toFitBuffer[U]


	@unspecialized
	override def copyToArray[U >: E](xs: Array[U], start: Int, len: Int): Unit =
		buffer.drop(offset).copyToArray(xs, start, len)




	override protected[this] def set(idx: Int, elem: E): Unit = buffer.update(offset+idx, elem)


	override def update(idx: Int, elems: TraversableOnce[E]): Unit =
		if (idx<0)
			throw new IndexOutOfBoundsException(s"TailBuffer($idx)")
		else buffer.update(offset+idx, elems)


	override def update(fromIndex: Int, value: E, count: Int): Unit =
		if (fromIndex<0)
			throw new IndexOutOfBoundsException(s"TailBuffer($fromIndex)")
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
	
	@unspecialized
	override def ++=(elems: FitTraversableOnce[E]): this.type = { buffer ++= elems; this }
	
	
	
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

	@unspecialized
	override def iterator = buffer.iterator.drop(offset)

	@unspecialized
	override def reverseIterator = buffer.reverseIterator.take(buffer.length-offset)

	override protected[this] def typeStringPrefix: String = "TailBuffer"
}


object TailBuffer {
	def apply[E](buffer :FitBuffer[E]) :FitBuffer[E] = new TailBuffer(buffer)

	def apply[E](buffer :FitBuffer[E], offset :Int) :FitBuffer[E] =
		new TailBuffer(buffer, offset min buffer.length max 0)
}