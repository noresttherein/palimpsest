package net.turambar.collection.specnaz

import scala.collection.{breakOut, GenTraversableOnce, TraversableLike}
import scala.collection.generic.CanBuildFrom

import net.turambar.collection.specnaz.SpecCompanion.{ArraySection, SpecCanBuildFrom}

/**
  * @author Marcin Mościcki
  */
class GrowableArray[@specialized(Reified) E] protected[specnaz](
		final protected[this] val array :Array[E],
		final protected[specnaz] val offset :Int,
		final val length :Int,
		private[this] var ownsPrefix :Boolean=false,
		private[this] var ownsSuffix :Boolean=false)
	extends ConstSeq[E] with SharedArray[E] with SharedArrayLike[E, GrowableArray] with SpecSeqLike[E, GrowableArray[E]]
{
	override protected[this] def factory: SharedArrayFactory[GrowableArray] = GrowableArray
	
	override protected def subseq(from: Int, until: Int): GrowableArray[E] =
		new GrowableArray[E](array, offset+from, until-from, false, false)
	
	
	
	@inline final protected[this] def shouldShareArray = synchronized {
		if (ownsSuffix && ownsPrefix) {
			ownsSuffix = false; ownsPrefix = false
			true
		} else false
	}
	
	@inline final protected[this] def shouldShareSuffix = synchronized {
		if (ownsSuffix) { ownsSuffix = false; true } else false
	}
	
	@inline final protected[this] def needsSuffix =
		(array.length - offset - length > 0) && synchronized { ownsSuffix }
	
	
	@inline final protected[this] def shouldSharePrefix = synchronized {
		if (ownsPrefix) { ownsPrefix = false; true } else false
	}
	
	@inline final protected[this] def needsPrefix =
		(offset > 0) && synchronized { ownsPrefix }
	
	@inline final protected[this] def reclaimSuffix() :Unit = synchronized {
		ownsSuffix = true
	}
	
	@inline final protected[this] def reclaimPrefix() :Unit = synchronized {
		ownsPrefix = true
	}
	
	@inline private[this] def suffixCapacity = array.length - offset - length

	
	
	
	
	override def +:[B >: E, That](elem: B)(implicit bf: CanBuildFrom[GrowableArray[E], B, That]): That =
		bf match {
			case spec :SpecCanBuildFrom[GrowableArray[E], B, That]
						if spec.honorsBuilderFrom && storageClass.isAssignableFrom(spec.elementType) =>
			{
				if (length == 0 && array.length > 0 && shouldShareArray) {
					array(length - 1) = elem.asInstanceOf[E]
					new GrowableArray[E](array, length-1, 1, true, true)
				} else if (offset > 0 && shouldSharePrefix) {
					val start = offset - 1
					array(start) = elem.asInstanceOf[E]
					new GrowableArray[E](array, start, length + 1, true, shouldShareSuffix)
				} else {
					val prefixReserve = (length+1) * 2
					val suffixReserve = if (needsSuffix) length else 0
					val copy = storageClassTag.newArray(prefixReserve + length + suffixReserve)
					System.arraycopy(array, offset, copy, prefixReserve, length)
					copy(prefixReserve-1) = elem.asInstanceOf[E]
					new GrowableArray[E](copy, prefixReserve-1, length+1, true, true)
				}
			}.asInstanceOf[That]
				
			case _ => super.+:(elem)
		}
	
	override def :+[B >: E, That](elem: B)(implicit bf: CanBuildFrom[GrowableArray[E], B, That]): That =
		bf match {
			case spec :SpecCanBuildFrom[GrowableArray[E], B, That]
						if spec.honorsBuilderFrom && storageClass.isAssignableFrom(spec.elementType) =>
			{
				if (length==0 && array.length>0 && shouldShareArray) {
					array(0) = elem.asInstanceOf[E]
					new GrowableArray[E](array, 0, 1, true, true)
				} else if (suffixCapacity > 0 && shouldShareSuffix) {
					array(offset+length) = elem.asInstanceOf[E]
					new GrowableArray[E](array, offset, length+1, shouldSharePrefix, true)
				} else {
					val prefixReserve = if (needsPrefix) length else 0
					val suffixReserve = (length+1) * 2
					val copy = storageClassTag.newArray(prefixReserve + length + suffixReserve)
					System.arraycopy(array, offset, copy, prefixReserve, length)
					copy(prefixReserve+length) = elem.asInstanceOf[E]
					new GrowableArray[E](copy, prefixReserve, length+1, true, true)
				}
			}.asInstanceOf[That]
			
			case _ => super.:+(elem)
		}
	
	override def ++[B >: E, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[GrowableArray[E], B, That]): That =
		bf match {
			case spec :SpecCanBuildFrom[GrowableArray[E], B, That]
				if hasFastSize(that) && spec.honorsBuilderFrom && storageClass.isAssignableFrom(spec.elementType) =>
					append(that).asInstanceOf[That]
			case _ => super.++(that)
		}
	
	
	override def ++:[B >: E, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[GrowableArray[E], B, That]): That =
		bf match {
			case spec :SpecCanBuildFrom[GrowableArray[E], B, That]
						if hasFastSize(that) && spec.honorsBuilderFrom && storageClass.isAssignableFrom(spec.elementType) =>
				prepend(that).asInstanceOf[That]
			case _ => super.++:(that)
		}
	
	override def ++:[B >: E, That](that: Traversable[B])(implicit bf: CanBuildFrom[GrowableArray[E], B, That]): That =
		bf match {
			case spec :SpecCanBuildFrom[GrowableArray[E], B, That]
						if hasFastSize(that) && spec.honorsBuilderFrom && storageClass.isAssignableFrom(spec.elementType) =>
				prepend(that).asInstanceOf[That]
			case _ =>
				(that ++ seq)(breakOut)
		}
	
	private def append[B >: E](that: GenTraversableOnce[B]) = {
		val extras = that.size
		if (extras == 0)
			this
		else if (length == 0 && array.length >= extras && shouldShareArray) {
			that.copyToArray(array.asInstanceOf[Array[B]])
			new GrowableArray[E](array, 0, extras, true, true)
		} else if (suffixCapacity > extras && shouldShareSuffix) {
			that.copyToArray(array.asInstanceOf[Array[B]], offset + length)
			new GrowableArray[E](array, offset, length + extras, shouldSharePrefix, true)
		} else {
			val prefixReserve = if (needsPrefix) length else 0
			val size = length + extras
			val capacity = newCapacity(size)
			val copy = storageClassTag.newArray(prefixReserve + capacity)
			System.arraycopy(array, offset, copy, prefixReserve, length)
			that.copyToArray(copy.asInstanceOf[Array[B]], prefixReserve + length)
			new GrowableArray[E](copy, prefixReserve, size, true, true)
		}
	}
	
	private def prepend[B >: E](that :GenTraversableOnce[B]) = {
		val extras = that.size
		if (extras == 0)
			this
		else if (length==0 && array.length >= extras && shouldShareArray) {
			val offset = array.length - extras
			that.copyToArray(array.asInstanceOf[Array[B]], offset)
			new GrowableArray[E](array, offset, extras, true, true)
		} else if (offset > extras && shouldSharePrefix) {
			val start = offset-extras
			that.copyToArray(array.asInstanceOf[Array[B]], start)
			new GrowableArray[E](array, start, length+extras, true, shouldShareSuffix)
		} else {
			val suffixReserve = if (needsSuffix) length else 0
			val size = length + extras
			val capacity = newCapacity(size)
			val start = capacity - size
			val copy = storageClassTag.newArray(capacity + suffixReserve)
			that.copyToArray(copy.asInstanceOf[Array[B]], start)
			System.arraycopy(array, offset, copy, capacity-length, length)
			new GrowableArray[E](array, start, size, true, true)
		}
	}
	
	
	private def newCapacity(required :Int) = {
		import java.lang.Integer.highestOneBit
		var capacity = (length + 1) * 2
		if (capacity < required)
			capacity = capacity << highestOneBit(required) - highestOneBit(capacity)
		if (capacity < size)
			capacity = capacity << 1
		capacity
	}
}


object GrowableArray extends SharedArrayFactory[GrowableArray] { factory =>
	@inline def Acc[E :Specialized] :GrowableArray[E] = using(Specialized.erasedArray[E], 0, 0)
	
	
	
	protected[specnaz] override def apply[E](contents: ArraySection[E]): GrowableArray[E] = shared(contents.copy)
	
	protected def using[@specialized(Reified) E](array: Array[E], offset: Int, length: Int): GrowableArray[E] =
		new GrowableArray[E](array, offset, length)
	
}






