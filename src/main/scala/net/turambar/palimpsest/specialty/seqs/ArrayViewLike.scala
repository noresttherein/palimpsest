package net.turambar.palimpsest.specialty.seqs

import java.lang.Math

import scala.annotation.unspecialized
import scala.reflect.ClassTag
import scala.collection.{immutable, IndexedSeqLike, IndexedSeqOptimized, LinearSeqLike}
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.Fun2Vals
import net.turambar.palimpsest.specialty._
import net.turambar.palimpsest.specialty.iterables.{IterableFoundation, IterableSpecialization}
import net.turambar.palimpsest.specialty.iterators.{ArrayIterator, ReverseArrayIterator}

import scala.compat.Platform




//class ArrayViewFoundation[E, +Repr](protected[this] final var array :Array[E], protected[this] final var begin :Int, protected[this] final var end :Int)
//	extends IterableFoundation[E, Repr] with IndexedSeqOptimized[E, Repr] with SliceLike[E, Repr] with OfKnownSize with Serializable



/**
  * @author Marcin Mościcki
  */
trait ArrayViewLike[@specialized(Elements) +E, +Repr]
	extends IndexedSeqOptimized[E, Repr] with SliceLike[E, Repr] with IterableSpecialization[E, Repr]
	   with OfKnownSize with Serializable
{

	protected[this] def array :Array[E]

	protected[palimpsest] def arr :Array[_ <: E] = array

	protected[palimpsest] def headIdx :Int
	protected[palimpsest] def endIdx :Int = headIdx + length


	@inline
	final def storageClass: Class[_] = array.getClass.getComponentType

//	@inline
//	final protected[this] def storageType :Class[E] = array.getClass.getComponentType.asInstanceOf[Class[E]]

	def boxClass: Class[_] = RuntimeType.BoxedClass(storageClass)

	@inline
	implicit final protected[this] def storageClassTag :ClassTag[E] = ClassTag(storageClass.asInstanceOf[Class[E]])




	override def head :E =
		if (nonEmpty) array(headIdx)
		else throw new NoSuchElementException(s"$this.head")

	override def head_? : ?[E] =
		if (nonEmpty) Sure(array(headIdx))
		else Blank
	
	override def headOption :Option[E] =
		if (nonEmpty) Some(array(headIdx))
		else None

	override def last :E =
		if (nonEmpty) array(length-1)
		else throw new NoSuchElementException(s"$this.last")

	override def last_? : ?[E] =
		if (nonEmpty) Sure(array(length-1))
		else Blank
	
	override def lastOption :Option[E] =
		if (nonEmpty) Some(array(array.length-1))
		else None

	
	
	/************************** Sub-arrays ****************************/

	override def filter(p: E => Boolean, value: Boolean): Repr = {
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


	@inline final override def foreach[@specialized(Unit) O](f: E => O): Unit = {
		val a = array; var i = headIdx; val e = i + length
		while (i<e) { f(a(i)); i+=1 }
	}


	@inline final override def reverseForeach(f: E => Unit): Unit = {
		val a = array; val e = headIdx; var i = headIdx+length-1
		while(i>=e) { f(a(i)); i-=1 }
	}




	/************** Searching for an element methods  ***************/

	override def find_?(f :E => Boolean, where :Boolean): ?[E] = {
		var i = headIdx; val end = endIdx
		val a = array
		while (i < end && f(a(i)) != where) i += 1
		if (i < end) Sure(a(i)) else Blank
	}
	
	
	override def segmentLength(p: E => Boolean, from: Int): Int = {
		val start = headIdx + Math.max(from, 0); val end = headIdx+length; val a=array
		var i = start
		while(i<end && p(a(i))) i+=1
		i-start
	}

	override def indexWhere(p: E => Boolean, from: Int): Int = {
		val start = headIdx + Math.max(from, 0); val end = headIdx+length; val a=array
		var i = start
		while(i>=end && !p(a(i))) i+=1
		i-start
	}


	override def lastIndexWhere(p: E => Boolean, from: Int): Int = {
		var i = headIdx + Math.min(from, length-1); val end = headIdx; val a = array
		while(i>=end && !p(a(i))) i-=1
		i - end
	}

	override protected[this] def offsetOf(elem: E, from: Int): Int =
		if (from>=length) -1 //also guards against arithmetic overflow on indices
		else {
			var i = headIdx + Math.max(from, 0); val e = headIdx+length
			val a = array
			while(i<e && a(i) != elem) i+=1
			if (i==e) -1 else i-headIdx
		}

	override protected[this] def lastOffsetOf(elem: E, end: Int): Int = {
		var i = Math.min(end, length-1) + headIdx; val hd = headIdx
		val a = array
		while (i >= hd && a(i) != elem) i -= 1
		if (i < hd) -1 else i - headIdx
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




	@inline @unspecialized
	override final protected[this] def trustedCopyTo(xs: Array[E], start: Int, total: Int) :Int = {
		val count = Math.min(total, length)
		Platform.arraycopy(array, headIdx, xs, start, count)
		count
	}


	override protected def manualCopyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Unit =
		if (start < 0)
			throw new IndexOutOfBoundsException(s"$stringPrefix.copyToArray([${xs.length}], $start, $len)")
		else if (start < xs.length && len > 0)
			     if (xs.getClass isAssignableFrom array.getClass)
				     Platform.arraycopy(array, headIdx, xs, start, Math.min(xs.length - start, Math.min(len, length)))
			     else {
				     val end = Math.min(xs.length, start + Math.min(len, length))
				     var into = start; var from = headIdx
				     val a = array
				     while (into < end) {
					     xs(into) = a(from)
					     into += 1; from += 1
				     }
			     }



	@unspecialized
	override def stable :StableSeq[E] = (ArrayPlus.builder[E](specialization) ++= this).result()

	//todo: optimistic specialization
//	override def toBuffer[U >: E]: FitBuffer[U] =

	@unspecialized
	override def toFitBuffer[U >: E: RuntimeType]: SharedArrayBuffer[U] =
		if (storageClass isAssignableFrom RuntimeType[U].runType)
			new GrowingArrayBuffer[E](array, headIdx, length, true).asInstanceOf[SharedArrayBuffer[U]]
		else SharedArrayBuffer.of[U] ++= this


	@unspecialized
	def toStableArray :StableArray[E] = StableArray.copy(array, headIdx, length)

	@unspecialized
	def toArrayPlus :ArrayPlus[E] = ArrayPlus.copy(array, headIdx, length)


//	override def toIndexedSeq: collection.immutable.IndexedSeq[E] =
//		StableArray(array, headIdx, length)

//	@unspecialized override def seq :ArrayView[E] = repr

	override def iterator: ArrayIterator[E] = new ArrayIterator[E](array, headIdx, headIdx+length)

	override def reverseIterator: ReverseArrayIterator[E] = new ReverseArrayIterator[E](array, headIdx+length-1, headIdx)

	
	@unspecialized
	protected[this] def newBuffer :SharedArrayBuffer[E] = SharedArrayBuffer.of[E](specialization)


	/** Includes the component type of the underlying array instead of specialization. */
	override def stringPrefix: String = typeStringPrefix + "[" + storageClass.getSimpleName + "]"

	protected[this] override def typeStringPrefix = "ArrayView"

	protected[this] override def debugString :String =
		debugPrefix + "[" + specialization + ":" + storageClass.getSimpleName + "]<" + length + ">"
}
