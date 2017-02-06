package net.turambar.palimpsest

import java.util

import scala.Specializable.SpecializedGroup
import scala.collection.{GenTraversableOnce, IndexedSeqLike, mutable}
import scala.reflect.ClassTag

import net.turambar.palimpsest.specialty.FitCompanion.{CanBreakOut, CanFitFrom}


/**
  * @author Marcin Mościcki
  */
package object specialty {

	final val Elements = Specialized.Values
	
	/** Same as `scala.collection.breakOut`, but for [[CanFitFrom]] instances.
	  * Named differently to avoid conflicts when both are imported.
	  * Adapts a builder factory tied to a specific source type to be used for any input collection types.
	  * Used to escape default resolution of static and dynamic result type of collection operations
	  * such as `map` and enforce the usage of a builder for a desired return type expressed as type
	  * constraint on the result of the operation.
	  */
	def forceFit[F, E, T](implicit anyFactory :CanFitFrom[_, E, T]) :CanFitFrom[F, E, T] =
		new CanBreakOut[F, E, T]
	
//	def breakOut[F, E, T](implicit anyFactory :FitCanBuildFrom[_, E, T]) :FitCanBuildFrom[F, E, T] =
//		new CanBreakOut[F, E, T]
	
	
	def specializeFrom[E](col :TraversableOnce[E]) :Specialized[E] = (col match {
		case i :FitIterable[E] => i.specialization
		case i :FitIterator[E] => i.specialization
		case a :mutable.WrappedArray[E] => Specialized.ofClass(a.array.getClass.getComponentType)
		case _ => Specialized.SpecializedAnyRef
	}).asInstanceOf[Specialized[E]]
	
	
	@inline
	private[palimpsest] final def ofKnownSize[T](col :GenTraversableOnce[T]) =  col match {
		case fit :FitItems[T] => fit.hasFastSize
		case _ :IndexedSeqLike[_, _] => true
		case _ => col.isEmpty
	}
		
	
	private[palimpsest] def arrayString(array :Array[_]) :String = s"${arrayString(array.getClass.getComponentType)}[${array.length}]"
	
	private[palimpsest] def arrayString(any :Class[_]) :String=
		if (any.isArray) arrayString(any.getComponentType) + "[]"
		else any.getClass.getName
	
	@inline private[specialty] final def newArray[E](elementType :Class[E], size :Int=0) = Array.ofDim[E](size)(ClassTag(elementType))
	
	
	@inline private[specialty] final def emptyArray[E](array :Array[E]) :Array[E] =
		Array.empty[E](ClassTag(array.getClass.getComponentType.asInstanceOf[Class[E]]))
	
	@inline private[specialty] final def emptyArray[E](elementType :Class[E]) :Array[E] =
		Array.empty[E](ClassTag(elementType))
	
	@inline private[specialty] final def arrayCopy[E](array :Array[E]) :Array[E] = {
		val res = Array.ofDim[E](array.length)(ClassTag(array.getClass.getComponentType.asInstanceOf[Class[E]]))
		System.arraycopy(array, 0, res, 0, array.length)
		res
	}
	
	@inline private[specialty] final def arrayCopy[E](array :Array[E], from :Int, length :Int) :Array[E] = {
		val res = Array.ofDim[E](length)(ClassTag(array.getClass.getComponentType.asInstanceOf[Class[E]]))
		System.arraycopy(array, from, res, 0, length)
		res
	}
	
	@inline private[specialty] final def arraySlice[E](from :Int, array :Array[E], until :Int) :Array[E] = {
		val length = until-from
		val res = Array.ofDim[E](length)(ClassTag(array.getClass.getComponentType.asInstanceOf[Class[E]]))
		System.arraycopy(array, from, res, 0, length)
		res
	}
	
	
	
	private[specialty] final def arrayFill[E](array :Array[E], value :E, from :Int=0, until :Int=Int.MaxValue) :Array[E] = {
		val upto = until min array.length
		val tpe = array.getClass.getComponentType
		if (tpe==classOf[Int])
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
	
	
	
	
	
	
	
	/** A simple wrapper over an array and validated index range, used as a common parameter for many factory methods of specialized collections. */
	private[specialty] class ArrayBounds[E] private[specialty](val array :Array[E], val start :Int, val length :Int) {
		def this(array :Array[E]) = this(array, 0, array.length)
		
		
		def end = start+length
		
		def copy = new ArrayBounds[E](arrayCopy(array, start, length), 0, length)
		
		implicit def specialization = Specialized.ofClass(array.getClass.getComponentType).asInstanceOf[Specialized[E]]
		
		
		override def toString = s"${array.getClass.getName}[${array.length}]($start..$end)"
	}
	
	/** Validates and creates [[ArrayBounds]] instances defining the contents of created specialized sequences. */
	private[specialty] object ArrayBounds {
		def share[E](array :Array[E]) :ArrayBounds[E] = new ArrayBounds(array, 0, array.length)
		
		def copy[E](array :Array[E]) :ArrayBounds[E] = new ArrayBounds(arrayCopy(array), 0, array.length)
		
		
		
		def share[E](array :Array[E], start :Int) :ArrayBounds[E] =
			if (start<0)
				throw new IndexOutOfBoundsException(s"ArrayBounds.share(${arrayString(array)}, $start")
			else if (start>=array.length)
				     new ArrayBounds(array, array.length, 0)
			else
				new ArrayBounds(array, start, array.length-start)
		
		def copy[E](array :Array[E], start :Int) :ArrayBounds[E] =
			if (start<0)
				throw new IndexOutOfBoundsException(s"ArrayBounds.copy(${arrayString(array)}, $start")
			else if (start>=array.length)
				     new ArrayBounds(emptyArray(array), 0, 0)
			else
				new ArrayBounds(arrayCopy(array, start, array.length-start))
		
		
		
		def share[E](array :Array[E], start :Int, length :Int) :ArrayBounds[E] =
			if (start<0)
				throw new IndexOutOfBoundsException(s"ArrayBounds.share(${arrayString(array)}, $start, $length")
			else if (start >= array.length)
				     new ArrayBounds(array, array.length, 0)
			else {
				val available = array.length-start
				new ArrayBounds(array, start,
					if (length>=available) available
					else if (length<0) 0
					else length
				)
			}
		
		
		def copy[E](array :Array[E], start :Int, length :Int) :ArrayBounds[E] =
			if (start<0)
				throw new IndexOutOfBoundsException(s"ArrayContents.copy(${arrayString(array)}, $start, $length")
			else if (start >= array.length || length <=0)
				     new ArrayBounds(emptyArray(array), 0, 0)
			else {
				val available = array.length-start
				val total = if (length>available) available else length
				new ArrayBounds(arrayCopy(array, start, total), 0, total)
			}
		
		
		def share[E](from :Int, array :Array[E], until :Int) :ArrayBounds[E] =
			if (from<0)
				throw new IndexOutOfBoundsException(s"ArrayContents.share($from, ${arrayString(array)}, $until")
			else if (from >= array.length || until<=from)
				     new ArrayBounds(array, array.length, 0)
			else if (until>=array.length)
				     new ArrayBounds(array, from, array.length-from)
			else
				new ArrayBounds(array, from, until-from)
		
		def copy[E](from :Int, array :Array[E], until :Int) :ArrayBounds[E] =
			if (from<0)
				throw new IndexOutOfBoundsException(s"ArrayContents.copy($from, ${arrayString(array)}, $until")
			else if (from >= array.length || until<=from)
				     new ArrayBounds(emptyArray(array), 0, 0)
			else if (until>=array.length)
				     new ArrayBounds(arrayCopy(array, from, array.length-from))
			else
				new ArrayBounds(arrayCopy(array, from, until-from))
		
		
		
	}
	
}
