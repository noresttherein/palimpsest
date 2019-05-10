package net.turambar.palimpsest

import java.util
import java.lang.Math

import scala.Specializable.SpecializedGroup
import scala.collection.{mutable, BitSet, BitSetLike, GenTraversableOnce, IndexedSeqLike, SetLike}
import scala.reflect.ClassTag
import net.turambar.palimpsest.specialty.FitCompanion.{CanBreakOut, CanFitFrom}
import net.turambar.palimpsest.specialty.RuntimeType.Primitives

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.ListSet


/**
  * @author Marcin Mościcki
  */
package object specialty {

	final val Elements = Primitives
	final val SetElements = Elements //we might want to exclude Boolean

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

	/** Same as `scala.collection.breakOut`, but for [[CanFitFrom]] instances.
	  * Named differently to avoid conflicts when both are imported.
	  * Adapts a builder factory tied to a specific source type to be used for any input collection types.
	  * Used to escape default resolution of static and dynamic result type of collection operations
	  * such as `map` and enforce the usage of a builder for a desired return type expressed as type
	  * constraint on the result of the operation.
	  */
	def forceFit[F, E, T](implicit anyFactory :CanFitFrom[_, E, T]) :CanBuildFrom[F, E, T] =
		new CanBreakOut[F, E, T].cbf
	
//	def breakOut[F, E, T](implicit anyFactory :FitCanBuildFrom[_, E, T]) :FitCanBuildFrom[F, E, T] =
//		new CanBreakOut[F, E, T]
	
	
	def specializeFrom[E](col :TraversableOnce[E]) :RuntimeType[E] = (col match {
		case i :FitIterable[E] => i.specialization
		case i :FitIterator[E] => i.specialization
		case a :mutable.WrappedArray[E] => RuntimeType.ofClass(a.array.getClass.getComponentType)
		case _ => RuntimeType.OfAnyRef
	}).asInstanceOf[RuntimeType[E]]
	
	
	@inline
	private[palimpsest] final def ofKnownSize[T](col :GenTraversableOnce[T]) =  col match {
		case fit :FitTraversableOnce[T] => fit.hasFastSize
		case _ :IndexedSeqLike[_, _] => true
		case _ :ListSet[_] => col.isEmpty
		case _ :BitSetLike[_] => false
		case _ :SetLike[_, _] => true
		case _ => col.isEmpty
	}
		
	
	private[palimpsest] def arrayString(array :Array[_]) :String = s"${arrayString(array.getClass.getComponentType)}[${array.length}]"
	
	private[palimpsest] def arrayString(any :Class[_]) :String=
		if (any.isArray) arrayString(any.getComponentType) + "[]"
		else any.getClass.getName
	
	@inline private[specialty] final def newArray[E](elementType :Class[E], size :Int=0) = Array.ofDim[E](size)(ClassTag(elementType))
	
	
	@inline private[specialty] final def emptyArray[E](array :Array[E]) :Array[E] =
		emptyArray(array.getClass.getComponentType.asInstanceOf[Class[E]])

	
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

	
	@inline private[specialty] final def arrayCopy[E](array :Array[E]) :Array[E] = {
		implicit val tag = ClassTag[E](array.getClass.getComponentType)
		val res = new Array[E](array.length)
		System.arraycopy(array, 0, res, 0, array.length)
		res
	}
	
	@inline private[specialty] final def arrayCopy[E](array :Array[E], from :Int, length :Int) :Array[E] = {
		implicit val tag = ClassTag[E](array.getClass.getComponentType)
		val res = new Array[E](length)
		System.arraycopy(array, from, res, 0, length)
		res
	}
	
	@inline private[specialty] final def arraySlice[E](from :Int, array :Array[E], until :Int) :Array[E] = {
		implicit val tag = ClassTag[E](array.getClass.getComponentType)
		val length = until-from
		val res = new Array[E](length)
		System.arraycopy(array, from, res, 0, length)
		res
	}
	
	
	
	private[specialty] final def arrayFill[E](array :Array[E], value :E, from :Int=0, until :Int=Int.MaxValue) :Array[E] = {
		val upto = Math.min(until, array.length)
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
	private[specialty] class ArrayBounds[E] private[specialty](val array :Array[E], val start :Int, val length :Int)
	                                                          (implicit val specialization :RuntimeType[E])
	{
		def this(array :Array[E]) =
			this(array, 0, array.length)(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))

		def this()(implicit specialization :RuntimeType[E]) =
			this(specialization.emptyArray.asInstanceOf[Array[E]], 0, 0)
		
		def end :Int = start+length
		
		def copy = new ArrayBounds[E](arrayCopy(array, start, length), 0, length)
		
//		implicit def specialization :Specialized[E] = Specialized.ofClass(array.getClass.getComponentType).asInstanceOf[Specialized[E]]
		
		
		override def toString = s"${array.getClass.getName}[${array.length}]($start..$end)"
	}



	/** Validates and creates [[ArrayBounds]] instances defining the contents of created specialized sequences. */
	private[specialty] object ArrayBounds {
		@inline final def share[E](array :Array[E]) :ArrayBounds[E] =
			new ArrayBounds(array, 0, array.length)(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))
		
		@inline final def copy[E](array :Array[E]) :ArrayBounds[E] =
			new ArrayBounds(arrayCopy(array), 0, array.length)(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))
		
		
		
		final def share[E](array :Array[E], start :Int) :ArrayBounds[E] =
			if (start < 0)
				throw new IndexOutOfBoundsException(s"ArrayBounds.copy(${arrayString(array)}, $start")
			else if (start >= array.length)
				new ArrayBounds(array, array.length, 0)(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))
			else
				new ArrayBounds(array, start, array.length-start)(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))


		def copy[E](array :Array[E], start :Int) :ArrayBounds[E] =
			if (start < 0)
				throw new IndexOutOfBoundsException(s"ArrayBounds.copy(${arrayString(array)}, $start")
			else if (start >= array.length)
				new ArrayBounds[E]()(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))
			else
				new ArrayBounds(arrayCopy(array, start, array.length - start))
		
		
		
		def share[E](array :Array[E], start :Int, length :Int) :ArrayBounds[E] =
			if (start<0)
				throw new IndexOutOfBoundsException(s"ArrayBounds.copy(${arrayString(array)}, $start, $length")
			else if (start >= array.length)
				new ArrayBounds(array, array.length, 0)(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))
			else {
				val available = array.length-start
				new ArrayBounds(array, start,
					if (length >= available) available
					else if (length < 0) 0
					else length
				)(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))
			}
		
		
		def copy[E](array :Array[E], start :Int, length :Int) :ArrayBounds[E] =
			if (start < 0)
				throw new IndexOutOfBoundsException(s"ArrayBounds.copy(${arrayString(array)}, $start, $length")
			else if (start >= array.length || length <= 0)
				new ArrayBounds[E]()(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))
			else {
				val available = array.length-start
				val total = if (length > available) available else length
				new ArrayBounds(arrayCopy(array, start, total), 0, total)(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))
			}
		


		def share[E](from :Int, array :Array[E], until :Int) :ArrayBounds[E] =
			if (from < 0)
				throw new IndexOutOfBoundsException(s"ArrayBounds.copy($from, ${arrayString(array)}, $until")
			else if (from >= array.length || until <= from)
				new ArrayBounds(array, array.length, 0)(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))
			else if (until >= array.length)
				new ArrayBounds(array, from, array.length - from)(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))
			else
				new ArrayBounds(array, from, until - from)(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))


		def copy[E](from :Int, array :Array[E], until :Int) :ArrayBounds[E] =
			if (from<0)
				throw new IndexOutOfBoundsException(s"ArrayBounds.copy($from, ${arrayString(array)}, $until")
			else if (from >= array.length || until <= from)
				new ArrayBounds[E]()(RuntimeType.ofClass(array.getClass.getComponentType.asInstanceOf[Class[E]]))
			else if (until >= array.length)
				new ArrayBounds(arrayCopy(array, from, array.length - from))
			else
				new ArrayBounds(arrayCopy(array, from, until - from))
		
		
		
	}
	
}
