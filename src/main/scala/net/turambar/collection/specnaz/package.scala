package net.turambar.collection

import java.util

import scala.Specializable.SpecializedGroup
import scala.collection.{GenTraversableOnce, IndexedSeqLike}
import scala.reflect.ClassTag


/**
  * @author Marcin Mościcki
  */
package object specnaz {
//	class elementType(group :SpecializedGroup=SpecializationOf.ElementType) extends specialized(group) {
////		def this() = this(SpecializationOf.AllButUnit)
//	}

	val Reified = Specialized.Primitives
	
	
	
	
	
	@inline
	private[collection] final def hasFastSize[T](col :GenTraversableOnce[T]) = col match {
		case _ :IndexedSeqLike[_, _] => true
		case i :SpecIterator[_] => i.hasFastSize
		case _ => col.isEmpty
	}
		
	
	private[collection] def arrayString(array :Array[_]) :String = s"${arrayString(array.getClass.getComponentType)}[${array.length}]"
	
	private[collection] def arrayString(any :Class[_]) :String=
		if (any.isArray) arrayString(any.getComponentType) + "[]"
		else any.getClass.getName
	
	@inline private[specnaz] final def newArray[E](elementType :Class[E], size :Int=0) = Array.ofDim[E](size)(ClassTag(elementType))
	
	
	@inline private[specnaz] final def emptyArray[E](array :Array[E]) :Array[E] =
		Array.empty[E](ClassTag(array.getClass.getComponentType.asInstanceOf[Class[E]]))
	
	@inline private[specnaz] final def emptyArray[E](elementType :Class[E]) :Array[E] =
		Array.empty[E](ClassTag(elementType))
	
	@inline private[specnaz] final def arrayCopy[E](array :Array[E]) :Array[E] = {
		val res = Array.ofDim[E](array.length)(ClassTag(array.getClass.getComponentType.asInstanceOf[Class[E]]))
		System.arraycopy(array, 0, res, 0, array.length)
		res
	}
	
	@inline private[specnaz] final def arrayCopy[E](array :Array[E], from :Int, length :Int) :Array[E] = {
		val res = Array.ofDim[E](length)(ClassTag(array.getClass.getComponentType.asInstanceOf[Class[E]]))
		System.arraycopy(array, from, res, 0, length)
		res
	}
	
	@inline private[specnaz] final def arraySlice[E](array :Array[E], from :Int, until :Int) :Array[E] = {
		val length = until-from
		val res = Array.ofDim[E](length)(ClassTag(array.getClass.getComponentType.asInstanceOf[Class[E]]))
		System.arraycopy(array, from, res, 0, length)
		res
	}
	
	
	
	private[specnaz] final def arrayFill[E](array :Array[E], value :E, from :Int=0, until :Int=Int.MaxValue) :Array[E] = {
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
		else if (tpe==classOf[Float])
	        util.Arrays.fill(array.asInstanceOf[Array[Float]], from, upto, value.asInstanceOf[Float])
		else if (tpe==classOf[Boolean])
			util.Arrays.fill(array.asInstanceOf[Array[Boolean]], from, upto, value.asInstanceOf[Boolean])
		else if (tpe==classOf[Short])
			util.Arrays.fill(array.asInstanceOf[Array[Short]], from, upto, value.asInstanceOf[Short])
		else
			util.Arrays.fill(array.asInstanceOf[Array[AnyRef]], from, upto, value.asInstanceOf[AnyRef])
		array.asInstanceOf[Array[E]]
	}
	
}
