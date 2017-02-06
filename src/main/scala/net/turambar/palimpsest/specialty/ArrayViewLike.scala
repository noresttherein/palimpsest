package net.turambar.palimpsest.specialty

import java.util

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

import Specialized.{Fun1Vals, Fun2, Fun2Vals}

trait ArrayViewLike[@specialized(Elements) +E, +Repr[X] <: ArrayViewLike[X, Repr] with ArrayView[X]]
	extends FitSeqLike[E, ArrayView[E]] with SpecializedTraversableTemplate[E, Repr]
{
	
	override protected[this] def emptyCollection: Repr[E] = companion.empty[E]
	
	protected[this] def array :Array[E]

	protected[specialty] def arr :Array[_] = array

	protected[specialty] def offset :Int

	
	
	override protected[this] def at(idx: Int): E = array(offset + idx)

	@inline
	final def storageClass: Class[_] = array.getClass.getComponentType

	@inline
	final protected[this] def storageType :Class[E] = array.getClass.getComponentType.asInstanceOf[Class[E]]

	def boxClass: Class[_] = Specialized.BoxClass(storageClass)

	@inline
	implicit final protected[this] def storageClassTag :ClassTag[E] = ClassTag(storageClass.asInstanceOf[Class[E]])
	
	
	
	override def head =
		if (length>0) array(offset)
		else throw new NoSuchElementException(s"$this.head")
	
	override def headOption =
		if (length>0) Some(array(offset))
		else None
	
	
	
	/************** Searching for an element methods  ***************/
	
	override def segmentLength(p: (E) => Boolean, from: Int): Int = {
		val start = offset + math.max(from, 0); val end = offset+length; val a=array
		var i = start
		while(i<end && p(a(i))) i+=1
		i-start
	}
	
	override def indexWhere(p: (E) => Boolean, from: Int): Int = {
		val start = offset + math.max(from, 0); val end = offset+length; val a=array
		var i = start
		while(i>=end && !p(a(i))) i+=1
		i-start
	}
	
	
	override def lastIndexWhere(p: (E) => Boolean, from: Int): Int = {
		var i = offset + math.min(from, length-1); val end = offset; val a = array
		while(i>=end && !p(a(i))) i-=1
		i - end
	}
	
	override protected[this] def fitIndexOf(elem: E, from: Int): Int =
		if (from>=length) -1
		else {
			var i = offset + math.max(from, 0); val e = offset+length
			val a = array
			while(i<e && a(i) != elem) i+=1
			if (i==e) -1 else i-offset
		}
	
	override protected[this] def fitLastIndexOf(elem: E, end: Int): Int =
		if (end<=0) -1
		else {
			var i = math.min(end, length-1) + offset
			val a = array
			while(i>=offset && a(i) != elem) i-=1
			if (i<offset) -1 else i-offset
		}
	
	
	
	
	override protected[this] def startsWithUnchecked(that :FitSeqLike[E, _], offset :Int) :Boolean = that match {
		case a :ArrayView[_] =>
			var me = offset; var she = a.offset; val myEnd = me + a.length
			val my = array; val her = a.arr.asInstanceOf[Array[E]]
			while(me<myEnd && my(me)==her(she)) { me+=1; she += 1 }
			me==myEnd
		case _ =>
			super.startsWithUnchecked(that, offset)
	}
	
	
	
	
	
	/* Predicate testing and traversing methods */
	
	
//	override final def foreach[@specialized(Unit) U](f: (E) => U): Unit = {
//		val a = array; var i = offset; val e = i + length
//		while(i<e) {
//			val x :E = a(i)
//			f.apply(x :E); i+=1
//		}
//	}

	//temporarily commented out for super class test
/*
	override def foreach[@specialized(Unit) O](f: (E) => O): Unit = {
		val a = array; var i = offset; val e = i + length
		while(i<e) {
//			val x :E = a(i)
			f(a(i)); i+=1
		}
	}
*/
	
	
	override def reverseForeach(f: (E) => Unit): Unit = {
		val a = array; val e = offset; var i = offset+length-1
		while(i>=e) { f(a(i)); i-=1 }
	}
	
	
	
	override final def foldLeft[@specialized(Fun2Vals) U](z: U)(op: (U, E) => U): U = {
		var i=offset; val e = offset+length; val a = array
		var res = z
		while(i<length) { res = op(res, a(i)); i+=1 }
		res
	}
	
	override def foldRight[@specialized(Fun2Vals) O](z: O)(op: (E, O) => O): O = {
		val e = offset; var i = e+length; val a = array
		var acc = z
		while(i>=e) { acc = op(a(i), acc); i-=1 }
		acc
	}
	
	
	/* ********** filtering and other self-typed collections ************** */
	
	//temporarily commented out for tests of super impl
/*	override protected[this] def filter(p: (E) => Boolean, value: Boolean): Repr[E] = {
		val b = newBuilder
		b.sizeHint(length)
		var i = offset; val e = i+length; val a = array
		while(i<e) {
			val e = a(i)
			if (p(e)) b += e
			i+=1
		}
		b.result()
	}*/
	
	
	
	
	override def toBuffer[U >: E]: FitBuffer[U] = {
		if (storageClass isAssignableFrom Specialized[U].runType)
			new GrowingArrayBuffer[E](array, offset, length, true).asInstanceOf[FitBuffer[U]]
		else new GrowingArrayBuffer[U]() ++= this
	}


	override def toFitBuffer[U >: E: Specialized]: SharedArrayBuffer[U] =
		if (storageClass isAssignableFrom Specialized[U].runType)
			new GrowingArrayBuffer[E](array, offset, length, true).asInstanceOf[SharedArrayBuffer[U]]
		else new GrowingArrayBuffer[U]() ++= this
	
	
	override def toIndexedSeq: ConstSeq[E] =
		new ConstArray[E](arrayCopy(array, offset, length), 0, length)
	
	@inline final override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit =
		if (start < xs.length && len > 0)
			if (start < 0)
				Array.copy(array, offset, xs, 0, len min length)
			else
				Array.copy(array, offset, xs, start, len min length min (xs.length - start))



	override def iterator: ArrayIterator[E] = new ArrayIterator[E](array, offset, offset+length)
	
	override def reverseIterator: ReverseArrayIterator[E] = new ReverseArrayIterator[E](array, offset+length-1, offset)
	
//	override def companion :FitCompanion[Repr]
	
	
	
	protected[this] def newBuffer :SharedArrayBuffer[E] = SharedArrayBuffer.empty[E]
	
	override def stringPrefix: String = typeStringPrefix + "[" + storageClass.getSimpleName + "]"

	override protected[this] def typeStringPrefix = "ArrayView"
}

