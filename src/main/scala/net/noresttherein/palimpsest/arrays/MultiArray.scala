package net.noresttherein.palimpsest.arrays

import net.noresttherein.palimpsest.RuntimeType.Specialized.MultiValue
import net.noresttherein.palimpsest.arrays.MultiArray.{ArrayAccessor, ArrayDimension}
import net.noresttherein.palimpsest.arrays.MultiArray.ArrayDimension.{_1, _2, _3, Inc}



/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
class MultiArray[@specialized(MultiValue) E, D <: ArrayDimension] private (array :Array[E], dimensions :Array[Int]) extends Serializable {

	def size :Int = {
		var i = dimensions.length - 1; var res = 1
		while (i >= 0) {
			res *= dimensions(i)
			i -= 1
		}
		res
	}

	def rank :Int = dimensions.length

	def dim(n :Int) :Int = dimensions(n)


	def apply(i0 :Int) :ArrayAccessor[E, D] = new ArrayAccessor(this, new Array[Int](dimensions.length))

	private def apply(indices :Array[Int]) :E = {
		var idx = indices(0); var i = 1
		while (i < dimensions.length) {
			idx *= dimensions(i-1)
			idx += indices(i)
			i += 1
		}
		array(idx)
	}

	private def update(indices :Array[Int], value :E) :Unit = {
		var idx = indices(0); var i = 1
		while (i < dimensions.length) {
			idx *= dimensions(i-1)
			idx += indices(i)
			i += 1
		}
		array(idx) = value
	}



	def apply(i0 :Int, i1 :Int)(implicit ev :D <:< _2) :E =
		if (i0 < 0 || i0 >= dimensions(0) || i1 < 0 || i1 >= dimensions(1))
			throw new IndexOutOfBoundsException(s"$prefixString($i0, $i1)")
		else
            array(i0 * dimensions(0) + i1)

	def update(i0 :Int, i1 :Int, value :E)(implicit ev :D <:< _2) :Unit =
		if (i0 < 0 || i0 >= dimensions(0) || i1 < 0 || i1 >= dimensions(1))
			throw new IndexOutOfBoundsException(s"$prefixString($i0, $i1)")
		else
			array(i0 * dimensions(0) + i1) = value



	def apply(i0 :Int, i1 :Int, i2 :Int)(implicit ev :D <:< _3) :E =
		if (i0 < 0 || i0 >= dimensions(0) || i1 < 0 || i1 >= dimensions(1) || i2 < 0 || i2 >= dimensions(2))
			throw new IndexOutOfBoundsException(s"$prefixString($i0, $i1, $i2)")
		else
			array(((i0 * dimensions(0)) + i1) * dimensions(1) + i2)

	def update(i0 :Int, i1 :Int, i2 :Int, value :E)(implicit ev :D <:< _3) :Unit =
		if (i0 < 0 || i0 >= dimensions(0) || i1 < 0 || i1 >= dimensions(1) || i2 < 0 || i2 >= dimensions(2))
			throw new IndexOutOfBoundsException(s"$prefixString($i0, $i1, $i2)")
		else
			array(((i0 * dimensions(0)) + i1) * dimensions(1) + i2) = value



	def prefixString :String = {
		val s = new StringBuilder(dimensions.length * 3 + 11, "MultiArray[")
		s append dimensions(0)
		var i = 1
		while (i < dimensions.length) {
			s append ", " append dimensions(i)
			i += 1
		}
		s append "]"
		s.toString
	}
}




object MultiArray {

	sealed abstract class ArrayDimension protected (final val toInt :Int)

	object ArrayDimension {
		final class _1 extends ArrayDimension(1) {
			def next :_2 = _2
		}

		final class Inc[D <: ArrayDimension] private[ArrayDimension] (dimension :Int) extends ArrayDimension(dimension) {
			def next :Inc[Inc[D]] = new Inc(toInt + 1)
		}

		type _2 = Inc[_1]
		final val _2 = new _2(2)

		type _3 = Inc[_2]
		final val _3 = new _3(3)

	}

	class ArrayAccessor[E, D <: ArrayDimension](array :MultiArray[E, _ <: ArrayDimension], indices :Array[Int]) {
		def update(i :Int, value :E)(implicit ev :D <:< _1) :Unit = {
			indices(0) = i
			array.update(indices, value)
		}

		def apply(i :Int)(implicit ev :D <:< _1) :E = {
			indices(0) = i
			array(indices)
		}

		def apply[N <: ArrayDimension](i :Int)(implicit ev :D <:< Inc[N], dim :N) :ArrayAccessor[E, N] = {
			indices(dim.toInt) = i
			new ArrayAccessor(array, indices)
		}
	}

//	val arr :MultiArray[Long, _3] = ???
//	arr(0)(1)(3) = 4L
//	val e = arr(0)(1)(3)


}
