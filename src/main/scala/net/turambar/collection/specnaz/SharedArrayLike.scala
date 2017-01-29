package net.turambar.collection.specnaz

import net.turambar.collection.specnaz.SpecCompanion.SpecBuilder

import scala.reflect.ClassTag


trait SharedArrayLike[@specialized(Reified) +E, +Repr[X] <: SharedArrayLike[X, Repr] with SharedArray[X]]
	extends SpecSeqLike[E, SharedArray[E]] with GenericSpecializedTraversable[E, Repr]
{
	protected[this] def array :Array[E]

	@inline final protected[specnaz] def arr :Array[_] = array

	protected[specnaz] def offset :Int

	@inline
	final override protected[this] def at(idx: Int): E = array(offset + idx)

	@inline
	final def storageClass: Class[_] = array.getClass.getComponentType

	@inline
	final protected[this] def storageType :Class[E] = array.getClass.getComponentType.asInstanceOf[Class[E]]

	def boxClass: Class[_] = Specialized.BoxClass(storageClass)

	@inline
	implicit final protected[this] def storageClassTag :ClassTag[E] = ClassTag(storageClass.asInstanceOf[Class[E]])


//	override protected def subseq(from: Int, until: Int): SharedArray[E] = factory.subseq(array, from, until-from)

	override def indexOf[U >: E](elem: U, from: Int): Int =
		if (boxClass==elem.getClass) specIndexOf(elem.asInstanceOf[E], from)
		else super.indexOf(elem, from)


	override def lastIndexOf[U >: E](elem: U, end: Int): Int =
		if (boxClass==elem.getClass) specLastIndexOf(elem.asInstanceOf[E], end)
		else super.lastIndexOf(elem, end)


	override def toBuffer[U >: E]: SpecBuffer[U] = {
		if (storageClass isAssignableFrom Specialized[U].runType)
			new ResizableArrayBuffer[E](array, offset, length, true).asInstanceOf[SpecBuffer[U]]
		else new ResizableArrayBuffer[U]() ++= this
	}


	override def toSpecBuffer[U >: E: Specialized]: SharedArrayBuffer[U] =
		if (storageClass isAssignableFrom Specialized[U].runType)
			new ResizableArrayBuffer[E](array, offset, length, true).asInstanceOf[SharedArrayBuffer[U]]
		else new ResizableArrayBuffer[U]() ++= this



	@inline final override def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit =
		if (start < xs.length && len > 0)
			if (start < 0)
				Array.copy(array, offset, xs, 0, len min length)
			else
				Array.copy(array, offset, xs, start, len min length min (xs.length - start))



	override def iterator: ArrayIterator[E] = new ArrayIterator[E](array, offset, offset+length)
	
	override def reverseIterator: ReverseArrayIterator[E] = new ReverseArrayIterator[E](array, offset+length-1, offset)
	
	protected[this] def factory :SharedArrayFactory[Repr]

	override def companion :SpecCompanion[Repr] = factory

	def dedicatedBuilder[@specialized(Reified) T :ClassTag] :SpecBuilder[T, Repr[T]] = factory.dedicatedBuilder[T]

	override protected[this] def newBuilder: SpecBuilder[E, Repr[E]] = factory.dedicatedBuilder[E]


	override def stringPrefix: String = typeStringPrefix + "[" + storageClass.getSimpleName + "]"

	override protected[this] def typeStringPrefix = "SharedArray"
}

