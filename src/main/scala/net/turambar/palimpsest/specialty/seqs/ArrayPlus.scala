package net.turambar.palimpsest.specialty.seqs

import scala.collection.generic.CanBuildFrom
import scala.collection.{GenTraversableOnce, breakOut, immutable}
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.iterables.IterableFoundation
import net.turambar.palimpsest.specialty.seqs.StableSeq.MakeStableIndexed
import net.turambar.palimpsest.specialty.{ArrayBounds, Elements, FitCompanion, Specialized, SpecializableIterable, ofKnownSize}

import scala.annotation.unspecialized

/** An immutable, specialized view on a section of an array with O(n) recursive concatenation/extension.
  * This holds only for one selected, but very common scenario, where the final sequence is built by
  * recursively growing an accumulator sequence, such as in:
  * {{{
  * (ArrayPlus.empty /: seqs){ (acc, elems) => acc :++ elems }
  * }}}
  * As such, its mainly a means for more convenient building of the final sequence without an intermediate
  * builder / linked list, rather than an implementation suited towards common concatenation,
  * as only the result of the concatenation is guaranteed to be expandable in amortized constant time per element.
  * All intermediate values of the accumulator in the above case will cause buffer copying on subsequent expansion.
  *
  *
  * While neither the range, nor the array or its contents within the range of this sequence can be mutated
  * by either this instance or any external source, the contents of the array outside of the  given section
  * are not immutable. Whenever an element is appended/prepended or this instance is concatenated
  * with another sequence, if the backing array has enough space in the corresponding fragment
  * (preceding or succeeding this section) '''and''' this instance ''owns'' the corresponding section
  * of the array (that is, either its whole suffix or whole prefix), new content is simply copied to the
  * underlying array and a view over the whole, extended section is returned as the result sequence, now
  * becoming the whole owner of the underlying section.
  *
  * This class is both effectively immutable and thread safe; the only mutable state are ownership
  * flags which can only be changed from `true` to `false`.
  *
  * @author Marcin Mościcki
  */
class ArrayPlus[@specialized(Elements) E] protected[seqs](
		final protected[this] val array :Array[E],
		final protected[seqs] val headIdx :Int,
		final val length :Int,
		private[this] var ownsPrefix :Boolean=false,
		private[this] var ownsSuffix :Boolean=false
	)
	extends IterableFoundation[E, ArrayPlus[E]] with MakeStableIndexed[E] with StableSeq[E]
			with ArrayView[E] with ArrayViewLike[E, ArrayPlus[E]]
			with SpecializableIterable[E, ArrayPlus]
{

	@unspecialized
	override def seq = this

	override def companion: FitCompanion[ArrayPlus] = ArrayPlus
	
	override protected def section(from: Int, until: Int): ArrayPlus[E] =
		new ArrayPlus[E](array, headIdx+from, until-from, false, false)
	
	override def clone(): ArrayPlus[E] =
		if (array.length>0 && array.length / length >= 2)
			ArrayPlus.view(toArray, 0, length)
		else this
	
	
	final protected[this] def shouldShareArray = synchronized {
		if (ownsSuffix && ownsPrefix) {
			ownsSuffix = false; ownsPrefix = false
			true
		} else false
	}
	
	final protected[this] def shouldShareSuffix = synchronized {
		if (ownsSuffix) { ownsSuffix = false; true } else false
	}
	
	@inline final protected[this] def needsSuffix =
		(array.length - headIdx - length > 0) && synchronized { ownsSuffix }
	
	
	final protected[this] def shouldSharePrefix = synchronized {
		if (ownsPrefix) { ownsPrefix = false; true } else false
	}
	
	final protected[this] def needsPrefix =
		(headIdx > 0) && synchronized { ownsPrefix }
	
	final protected[this] def reclaimSuffix() :Unit = synchronized {
		ownsSuffix = true
	}
	
	final protected[this] def reclaimPrefix() :Unit = synchronized {
		ownsPrefix = true
	}
	
	@inline private[this] def suffixCapacity = array.length - headIdx - length

	
	
	
	
	override def +:[B >: E, That](elem: B)(implicit bf: CanBuildFrom[ArrayPlus[E], B, That]): That =
		bf match {
			case spec :CanFitFrom[ArrayPlus[E], B, That]
						if spec.honorsBuilderFrom && storageClass.isAssignableFrom(spec.elementType) =>
			{
				if (length == 0 && array.length > 0 && shouldShareArray) {
					array(length - 1) = elem.asInstanceOf[E]
					new ArrayPlus[E](array, length-1, 1, true, true)
				} else if (headIdx > 0 && shouldSharePrefix) {
					val start = headIdx - 1
					array(start) = elem.asInstanceOf[E]
					new ArrayPlus[E](array, start, length + 1, true, shouldShareSuffix)
				} else {
					val prefixReserve = (length+1) * 2
					val suffixReserve = if (needsSuffix) length else 0
					val copy = storageClassTag.newArray(prefixReserve + length + suffixReserve)
					System.arraycopy(array, headIdx, copy, prefixReserve, length)
					copy(prefixReserve-1) = elem.asInstanceOf[E]
					new ArrayPlus[E](copy, prefixReserve-1, length+1, true, true)
				}
			}.asInstanceOf[That]
				
			case _ => super.+:(elem)(bf)
		}
	
	override def :+[B >: E, That](elem: B)(implicit bf: CanBuildFrom[ArrayPlus[E], B, That]): That =
		bf match {
			case spec :CanFitFrom[ArrayPlus[E], B, That]
						if spec.honorsBuilderFrom && storageClass.isAssignableFrom(spec.elementType) =>
			{
				if (length==0 && array.length>0 && shouldShareArray) {
					array(0) = elem.asInstanceOf[E]
					new ArrayPlus[E](array, 0, 1, true, true)
				} else if (suffixCapacity > 0 && shouldShareSuffix) {
					array(headIdx+length) = elem.asInstanceOf[E]
					new ArrayPlus[E](array, headIdx, length+1, shouldSharePrefix, true)
				} else {
					val prefixReserve = if (needsPrefix) length else 0
					val suffixReserve = (length+1) * 2
					val copy = storageClassTag.newArray(prefixReserve + length + suffixReserve)
					System.arraycopy(array, headIdx, copy, prefixReserve, length)
					copy(prefixReserve+length) = elem.asInstanceOf[E]
					new ArrayPlus[E](copy, prefixReserve, length+1, true, true)
				}
			}.asInstanceOf[That]
			
			case _ => super.:+(elem)
		}
	
	override def ++[B >: E, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[ArrayPlus[E], B, That]): That =
		bf match {
			case spec :CanFitFrom[ArrayPlus[E], B, That]
				if ofKnownSize(that) && spec.honorsBuilderFrom && storageClass.isAssignableFrom(spec.elementType) =>
					append(that).asInstanceOf[That]
			case _ => super.++(that)
		}
	
	
	override def ++:[B >: E, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[ArrayPlus[E], B, That]): That =
		bf match {
			case spec :CanFitFrom[ArrayPlus[E], B, That]
						if ofKnownSize(that) && spec.honorsBuilderFrom && storageClass.isAssignableFrom(spec.elementType) =>
				prepend(that).asInstanceOf[That]
			case _ => super.++:(that)
		}
	
	override def ++:[B >: E, That](that: Traversable[B])(implicit bf: CanBuildFrom[ArrayPlus[E], B, That]): That =
		bf match {
			case spec :CanFitFrom[ArrayPlus[E], B, That]
						if ofKnownSize(that) && spec.honorsBuilderFrom && storageClass.isAssignableFrom(spec.elementType) =>
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
			new ArrayPlus[E](array, 0, extras, true, true)
		} else if (suffixCapacity > extras && shouldShareSuffix) {
			that.copyToArray(array.asInstanceOf[Array[B]], headIdx + length)
			new ArrayPlus[E](array, headIdx, length + extras, shouldSharePrefix, true)
		} else {
			val prefixReserve = if (needsPrefix) length else 0
			val size = length + extras
			val capacity = newCapacity(size)
			val copy = storageClassTag.newArray(prefixReserve + capacity)
			System.arraycopy(array, headIdx, copy, prefixReserve, length)
			that.copyToArray(copy.asInstanceOf[Array[B]], prefixReserve + length)
			new ArrayPlus[E](copy, prefixReserve, size, true, true)
		}
	}
	
	private def prepend[B >: E](that :GenTraversableOnce[B]) = {
		val extras = that.size
		if (extras == 0)
			this
		else if (length==0 && array.length >= extras && shouldShareArray) {
			val offset = array.length - extras
			that.copyToArray(array.asInstanceOf[Array[B]], offset)
			new ArrayPlus[E](array, offset, extras, true, true)
		} else if (headIdx > extras && shouldSharePrefix) {
			val start = headIdx-extras
			that.copyToArray(array.asInstanceOf[Array[B]], start)
			new ArrayPlus[E](array, start, length+extras, true, shouldShareSuffix)
		} else {
			val suffixReserve = if (needsSuffix) length else 0
			val size = length + extras
			val capacity = newCapacity(size)
			val start = capacity - size
			val copy = storageClassTag.newArray(capacity + suffixReserve)
			that.copyToArray(copy.asInstanceOf[Array[B]], start)
			System.arraycopy(array, headIdx, copy, capacity-length, length)
			new ArrayPlus[E](array, start, size, true, true)
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
	
	
	override def typeStringPrefix: String = "Array+"
}


/** Factory for specialized sequences implemented as views on an a section of an array which pass
  * the ownership of the backing array on concatenation, making
  */
object ArrayPlus extends ArrayViewFactory[ArrayPlus] { factory =>
	@inline def Acc[E :Specialized] :ArrayPlus[E] = using(Specialized.arrayFor[E], 0, 0)
	
	
	
	protected[seqs] override def apply[E](contents: ArrayBounds[E]): ArrayPlus[E] = shared(contents.copy)
	
	protected def using[@specialized(Elements) E](array: Array[E], offset: Int, length: Int): ArrayPlus[E] =
		new ArrayPlus[E](array, offset, length, true, true)
	

	@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ArrayPlus[_], E, ArrayPlus[E]]): CanBuildFrom[ArrayPlus[_], E, ArrayPlus[E]] =
		fit.cbf
}






