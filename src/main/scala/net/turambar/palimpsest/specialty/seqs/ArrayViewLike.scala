package net.turambar.palimpsest.specialty.seqs

import scala.annotation.unspecialized
import scala.reflect.ClassTag
import scala.collection.{IndexedSeqLike, IndexedSeqOptimized, LinearSeqLike, immutable}
import net.turambar.palimpsest.specialty.Specialized.Fun2Vals
import net.turambar.palimpsest.specialty.{ArrayIterator, Elements, FitBuilder, IterableSpecialization, ReverseArrayIterator, Specialized, SpecializableIterable, arrayCopy}

/**
  * @author Marcin Mościcki
  */
trait ArrayViewLike[@specialized(Elements) +E, +Repr]
	extends IndexedSeqOptimized[E, Repr] with SliceLike[E, Repr] with IterableSpecialization[E, Repr] with Serializable//extend specialized version of iterable
{

	protected[this] def array :Array[E]

	protected[seqs] def arr :Array[_] = array

	protected[seqs] def headIdx :Int
	protected[this] def endIdx :Int = headIdx + length


	@inline
	final def storageClass: Class[_] = array.getClass.getComponentType

	@inline
	final protected[this] def storageType :Class[E] = array.getClass.getComponentType.asInstanceOf[Class[E]]

	def boxClass: Class[_] = Specialized.BoxClass(storageClass)

	@inline
	implicit final protected[this] def storageClassTag :ClassTag[E] = ClassTag(storageClass.asInstanceOf[Class[E]])




	override def head =
		if (nonEmpty) array(headIdx)
		else throw new NoSuchElementException(s"$this.head")

	override def headOption =
		if (nonEmpty) Some(array(headIdx))
		else None

	override def last =
		if (nonEmpty) array(length-1)
		else throw new NoSuchElementException(s"$this.last")

	override def lastOption =
		if (nonEmpty) Some(array(array.length-1))
		else None

	/************** Searching for an element methods  ***************/

	override def segmentLength(p: (E) => Boolean, from: Int): Int = {
		val start = headIdx + math.max(from, 0); val end = headIdx+length; val a=array
		var i = start
		while(i<end && p(a(i))) i+=1
		i-start
	}

	override def indexWhere(p: (E) => Boolean, from: Int): Int = {
		val start = headIdx + math.max(from, 0); val end = headIdx+length; val a=array
		var i = start
		while(i>=end && !p(a(i))) i+=1
		i-start
	}


	override def lastIndexWhere(p: (E) => Boolean, from: Int): Int = {
		var i = headIdx + math.min(from, length-1); val end = headIdx; val a = array
		while(i>=end && !p(a(i))) i-=1
		i - end
	}

	override protected[this] def positionOf(elem: E, from: Int): Int =
		if (from>=length) -1 //also guards against arithmetic overflow on indices
		else {
			var i = headIdx + math.max(from, 0); val e = headIdx+length
			val a = array
			while(i<e && a(i) != elem) i+=1
			if (i==e) -1 else i-headIdx
		}

	override protected[this] def lastPositionOf(elem: E, end: Int): Int = {
		var i = math.min(end, length-1) + headIdx
		val a = array
		while(i>=headIdx && a(i) != elem) i-=1
		if (i<headIdx) -1 else i-headIdx
	}







	/* Predicate testing and traversing methods */



	@inline final override def foreach[@specialized(Unit) O](f: (E) => O): Unit = {
		val a = array; var i = headIdx; val e = i + length
		while(i<e) { f(a(i)); i+=1 }
	}


	@inline final override def reverseForeach(f: (E) => Unit): Unit = {
		val a = array; val e = headIdx; var i = headIdx+length-1
		while(i>=e) { f(a(i)); i-=1 }
	}



	@inline final override def foldLeft[@specialized(Fun2Vals) U](z: U)(op: (U, E) => U): U = {
		var i=headIdx; val e = headIdx+length; val a = array
		var res = z
		while(i<length) { res = op(res, a(i)); i+=1 }
		res
	}

	@inline final override def foldRight[@specialized(Fun2Vals) O](z: O)(op: (E, O) => O): O = {
		val e = headIdx; var i = e+length; val a = array
		var acc = z
		while(i>=e) { acc = op(a(i), acc); i-=1 }
		acc
	}


	/* ********** filtering and other self-typed collections ************** */


	override protected[this] def filter(p: (E) => Boolean, value: Boolean): Repr = {
		val b = newBuilder
		b.sizeHint(length)
		var i = headIdx; val e = i+length; val a = array
		while(i<e) {
			val e = a(i)
			if (p(e)) b += e
			i+=1
		}
		b.result()
	}


	//todo: optimistic specialization
//	override def toBuffer[U >: E]: FitBuffer[U] =

	@unspecialized
	override def toFitBuffer[U >: E: Specialized]: SharedArrayBuffer[U] =
		if (storageClass isAssignableFrom Specialized[U].runType)
			new GrowingArrayBuffer[E](array, headIdx, length, true).asInstanceOf[SharedArrayBuffer[U]]
		else SharedArrayBuffer.like[U] ++= this


//	override def toIndexedSeq: collection.immutable.IndexedSeq[E] =
//		StableArray(array, headIdx, length)


	@inline @unspecialized
	final override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit =
		if (start < xs.length && len > 0)
			if (start < 0)
				Array.copy(array, headIdx, xs, 0, len min length)
			else
				Array.copy(array, headIdx, xs, start, len min length min (xs.length - start))



	override def iterator: ArrayIterator[E] = new ArrayIterator[E](array, headIdx, headIdx+length)

	override def reverseIterator: ReverseArrayIterator[E] = new ReverseArrayIterator[E](array, headIdx+length-1, headIdx)

	@unspecialized
	protected[this] def newBuffer :SharedArrayBuffer[E] = SharedArrayBuffer.like[E](mySpecialization)


	/** Includes the component type of the underlying array instead of specialization. */
	override def stringPrefix: String = typeStringPrefix + "[" + storageClass.getSimpleName + "]"

	override protected[this] def typeStringPrefix = "ArrayView"
}
