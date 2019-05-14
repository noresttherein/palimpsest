package net.turambar.palimpsest.specialty.seqs

import net.turambar.palimpsest.specialty.RuntimeType
import net.turambar.palimpsest.specialty.{arrayCopy, arrayString}

import scala.reflect.ClassTag


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


}
