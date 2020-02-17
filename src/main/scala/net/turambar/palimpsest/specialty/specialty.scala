package net.turambar.palimpsest

import java.util

import scala.collection.{mutable, BitSetLike, GenTraversableOnce, IndexedSeqLike, SetLike}
import scala.reflect.ClassTag
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.{Fun2, Primitives}
import net.turambar.palimpsest.specialty.iterables.{AptIterable, IterableTemplate}
import net.turambar.palimpsest.specialty.iterators.AptIterator

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.ListSet
import scala.collection.mutable.Builder




/**
  * @author Marcin Mościcki
  */
package object specialty {

	final val ItemTypes = Primitives

	/** A specialized version of `scala.Option`.
	  * This is a type alias for [[Unsure]] used as a shorthand for passing uncertain values.
	  * @see [[Sure]]
	  * @see [[Blank]]
	  * @see [[specialty.null_?]]
	  * @see [[specialty.some_?]]
	  */
	type ?[@specialized(Primitives) +T] = Unsure[T]

	val ? = Unsure


	/** Tests whether a value is null, creating an uncertain value `?[T]` in the result the same way `Option(value)` would.
	  * @return [[Sure]]`(value)` ''iff'' `value != null` or [[Blank]] otherwise
	  */
	@inline def null_?[T](value :T): ?[T] = if (value == null) Blank else Sure(value)

	/** Converts a scala option to an uncertain value `?[T]`.
	  * @return [[Sure]]`(value.get)` if `value :Some` or [[Blank]] otherwise.
	  */
	@inline def some_?[@specialized(Primitives) T](value :Option[T]): ?[T] = value match {
		case Some(v) => Sure(v)
		case None => Blank
	}


//todo: adapters/converters from scala classes


	
	@inline
	private[palimpsest] final def ofKnownSize[T](col :GenTraversableOnce[T]) =  col match {
		//todo: growing/shrinking collections' size can change during append; should they count as having fast size?
		case fit :Vals[T] => fit.hasFastSize
		case _ :IndexedSeqLike[_, _] => true // ListSet is primarily used for hash collisions
		case list :ListSet[_] => list.isEmpty || list.tail.isEmpty || list.tail.tail.isEmpty || list.tail.tail.tail.isEmpty
		case _ :BitSetLike[_] => false
		case _ :SetLike[_, _] => true
		case _ => col.isEmpty
	}



	private[specialty] def concat[E, R](first :Vals[E], second :TraversableOnce[E])(builder :Builder[E, R]) :R = {
		if (first.hasFastSize && ofKnownSize(second))
			builder sizeHint first.size + second.size
		builder ++= first ++= second
		builder.result()
	}

	private[specialty] def concat[E, R](first :TraversableOnce[E], second :Vals[E])(builder :Builder[E, R]) :R = {
		if (second.hasFastSize && ofKnownSize(first))
			builder sizeHint first.size + second.size
		builder ++= first ++= second
		builder.result()
	}



	private[specialty] def span[@specialized(ItemTypes) E, R]
	(src :AptIterator[E], prefix :AptBuilder[E, R], suffix :AptBuilder[E, R])(p :E => Boolean) :(R, R) =
	{
		while (src.hasNext && p(src.head)) prefix += src.next()
		while (src.hasNext) suffix += src.next()
		(prefix.result(), suffix.result())
	}

	private[specialty] def filter[@specialized(ItemTypes) E, R](src :AptIterator[E], builder :AptBuilder[E, R])
	                                                           (p :E => Boolean, neoTruth :Boolean = true) :R =
	{
		while (src.hasNext) {
			val e = src.next()
			if (p(e) == neoTruth)
				builder += e
		}
		builder.result()
	}

	private[specialty] def partition[@specialized(ItemTypes) E, R]
	                                (src :AptIterator[E], whereTrue :AptBuilder[E, R], whereFalse :AptBuilder[E, R])
	                                (p :E => Boolean) :(R, R) =
	{
		while (src.hasNext) {
			val e = src.next()
			if (p(e)) whereTrue += e
			else whereFalse += e
		}
		(whereTrue.result(), whereFalse.result())
	}







	private[palimpsest] def arrayString(array :Array[_]) :String = s"${arrayString(array.getClass.getComponentType)}[${array.length}]"
	
	private[palimpsest] def arrayString(any :Class[_]) :String=
		if (any.isArray) arrayString(any.getComponentType) + "[]"
		else any.getClass.getName




/*
	@inline private[specialty] final def emptyArray[E](elementType :Class[E]) :Array[E] = {
		if (elementType.isPrimitive)
			(if (elementType eq classOf[Int]) EmptyIntArray
			else if (elementType eq classOf[Double]) EmptyDoubleArray
			else if (elementType eq classOf[Byte]) EmptyByteArray
			else if (elementType eq classOf[Char]) EmptyCharArray
			else if (elementType eq classOf[Long]) EmptyLongArray
			else if (elementType eq classOf[Boolean]) EmptyBooleanArray
			else if (elementType eq classOf[Float]) EmptyFloatArray
			else if (elementType eq classOf[Short]) EmptyShortArray
			else if (elementType eq classOf[Unit]) EmptyUnitArray
			else Array.empty[E](ClassTag(elementType))
			).asInstanceOf[Array[E]]
		else {
			implicit val tag = ClassTag[E](elementType)
			new Array[E](0)
		}
	}

	private[this] final val EmptyIntArray = new Array[Int](0)
	private[this] final val EmptyLongArray = new Array[Long](0)
	private[this] final val EmptyShortArray = new Array[Short](0)
	private[this] final val EmptyByteArray = new Array[Byte](0)
	private[this] final val EmptyDoubleArray = new Array[Double](0)
	private[this] final val EmptyFloatArray = new Array[Float](0)
	private[this] final val EmptyCharArray = new Array[Char](0)
	private[this] final val EmptyBooleanArray = new Array[Boolean](0)
	private[this] final val EmptyUnitArray = new Array[Unit](0)
*/


	@inline private[specialty] final def newArray[E](elements :Class[_], length :Int) :Array[E] =
		if (elements eq java.lang.Void.TYPE)
			new Array[Unit](length).asInstanceOf[Array[E]]
		else
	        java.lang.reflect.Array.newInstance(elements, length).asInstanceOf[Array[E]]


	private[specialty] final def arrayCopy[E](src :Array[E], srcOffset :Int, dst :Array[E], dstOffset :Int, length :Int) :Unit =
		if (src.getClass.getComponentType.isPrimitive == dst.getClass.getComponentType.isPrimitive)
			System.arraycopy(src, srcOffset, dst, dstOffset, length)
		else {
			slowcopy(src, srcOffset, dst, dstOffset, length)
		}

	private[specialty] final def slowcopy[E](src :Array[E], srcOffset :Int, dst :Array[E], dstOffset :Int, length :Int) :Unit = {
		var from = srcOffset; var to = dstOffset; val end = from + length
		while (from < end) {
			dst(to) = src(from)
			from += 1; to += 1
		}
	}


	@inline private[specialty] final def arrayCopy[E](array :Array[E]) :Array[E] = {
		val res = newArray[E](array.getClass.getComponentType, array.length)
		System.arraycopy(array, 0, res, 0, array.length)
		res
	}

	@inline private[specialty] final def arrayCopy[E](array :Array[E], from :Int, length :Int) :Array[E] = {
		val res = newArray[E](array.getClass.getComponentType, length)
		System.arraycopy(array, from, res, 0, length)
		res
	}

	@inline private[specialty] final def arraySlice[E](from :Int, array :Array[E], until :Int) :Array[E] = {
		val length = until-from
		val res = newArray[E](array.getClass.getComponentType, length)
		System.arraycopy(array, from, res, 0, length)
		res
	}

	
	
	private[specialty] final def arrayFill[E](array :Array[E], value :E, from :Int=0, until :Int=Int.MaxValue) :Array[E] = {
		val upto = Math.min(until, array.length)
		val tpe = array.getClass.getComponentType
		if (!tpe.isPrimitive)
			util.Arrays.fill(array.asInstanceOf[Array[AnyRef]], from, upto, value.asInstanceOf[AnyRef])
		else if (tpe==classOf[Int])
			util.Arrays.fill(array.asInstanceOf[Array[Int]], from, upto, value.asInstanceOf[Int])
		else if (tpe==classOf[Byte])
	        util.Arrays.fill(array.asInstanceOf[Array[Byte]], from, upto, value.asInstanceOf[Byte])
		else if (tpe==classOf[Char])
			util.Arrays.fill(array.asInstanceOf[Array[Char]], from, upto, value.asInstanceOf[Char])
		else if (tpe==classOf[Double])
	        util.Arrays.fill(array.asInstanceOf[Array[Double]], from, upto, value.asInstanceOf[Double])
		else if (tpe==classOf[Long])
			util.Arrays.fill(array.asInstanceOf[Array[Long]], from, upto, value.asInstanceOf[Long])
		else if (tpe==classOf[Boolean])
			util.Arrays.fill(array.asInstanceOf[Array[Boolean]], from, upto, value.asInstanceOf[Boolean])
		else if (tpe==classOf[Float])
	        util.Arrays.fill(array.asInstanceOf[Array[Float]], from, upto, value.asInstanceOf[Float])
		else if (tpe==classOf[Short])
			util.Arrays.fill(array.asInstanceOf[Array[Short]], from, upto, value.asInstanceOf[Short])
		else
			util.Arrays.fill(array.asInstanceOf[Array[AnyRef]], from, upto, value.asInstanceOf[AnyRef])
		array.asInstanceOf[Array[E]]

	}
	
	

}
