package net.turambar.palimpsest.specialty.ordered

import net.turambar.palimpsest.specialty.{Elements, FitIterable}
import net.turambar.palimpsest.specialty.Specialized.Fun1
import net.turambar.palimpsest.specialty.ordered.ValOrdering.GenericOrdering


trait ValOrdering[@specialized(Elements) K] extends Ordering[K] { ord =>
	override def lteq(x: K, y: K): Boolean = compare(x, y) <= 0
	override def gteq(x: K, y: K): Boolean = compare(x, y) >= 0
	override def lt(x: K, y: K): Boolean = compare(x, y) < 0
	override def gt(x: K, y: K): Boolean = compare(x, y) > 0
	override def equiv(x: K, y: K): Boolean = compare(x, y) == 0

	override def max(x: K, y: K): K = if (compare(x, y) <= 0) y else x
	override def min(x: K, y: K): K = if (compare(x, y) > 0) y else x

	override def reverse: ValOrdering[K] = new ValOrdering[K] {
		override def compare(x: K, y: K): Int = ord.compare(y, x)
		override def reverse :ValOrdering[K] = ord
	}

	override def on[@specialized(Fun1) U](f: U => K): ValOrdering[U] = (x: U, y: U) => ord.compare(f(x), f(y))

//	override implicit def mkOrderingOps(lhs: K): Ops = super.mkOrderingOps(lhs)
}

sealed class FallbackOrderingImplicits {
	implicit def apply[E](implicit gen :Ordering[E]) :ValOrdering[E] = new GenericOrdering[E](gen)
}

object ValOrdering extends FallbackOrderingImplicits  {

	/** Marker trait for natural orderings of built-in value types. Sealed, so can be used to quickly determine
	  * if an implicit parameter is one of the defaults provided here and perform some optimizations.
	  */
	sealed trait NaturalValOrdering[T] { this :ValOrdering[T] => }


	implicit case object BooleanOrdering extends ValOrdering[Boolean]  with NaturalValOrdering[Boolean] {
		override def compare(x: Boolean, y: Boolean): Int =
			if (x)
				if (y) 0 else 1
			else
				if (y) -1 else 0
	}

	implicit case object ByteOrdering extends ValOrdering[Byte] with NaturalValOrdering[Byte] {
		override def compare(x: Byte, y: Byte): Int = x.toInt - y.toInt
	}

	implicit case object ShortOrdering extends ValOrdering[Short] with NaturalValOrdering[Short] {
		override def compare(x :Short, y :Short) :Int = x.toInt - y.toInt
	}

	implicit case object CharOrdering extends ValOrdering[Char] with NaturalValOrdering[Char] {
		override def compare(x: Char, y: Char): Int = x.toInt - y.toInt
	}

	implicit case object IntOrdering extends ValOrdering[Int] with NaturalValOrdering[Int] {
		override def compare(x :Int, y :Int) :Int = if (x < y) -1 else if (x > y) 1 else 0
	}

	trait LongOrdering extends ValOrdering[Long] {

		override def compare(x: Long, y: Long): Int = if (x < y) -1 else if (x > y) 1 else 0

		override def lteq(x: Long, y: Long): Boolean = x <= y

		override def gteq(x: Long, y: Long): Boolean = x >= y

		override def lt(x: Long, y: Long): Boolean = x < y

		override def gt(x: Long, y: Long): Boolean = x > y

		override def equiv(x: Long, y: Long): Boolean = x == y

		override def max(x: Long, y: Long): Long = if (x >= y) x else y

		override def min(x: Long, y: Long): Long = if (x <= y) x else y


	}
	
	implicit case object LongOrdering extends LongOrdering with NaturalValOrdering[Long]

	implicit case object FloatOrdering extends ValOrdering[Float] with NaturalValOrdering[Float] {
		override def compare(x: Float, y: Float): Int = if (x < y) -1 else if (x > y) 1 else 0
	}

	implicit case object DoubleOrdering extends ValOrdering[Double] with NaturalValOrdering[Double] {
		override def compare(x :Double, y :Double) :Int = if (x < y) -1 else if (x > y) 1 else 0
	}

	//todo: should those below be also NaturalValOrdering?

	implicit case object BigIntOrdering extends ValOrdering[BigInt] {
		override def compare(x: BigInt, y: BigInt): Int = x compare y
	}

	implicit case object BigDecimalOrdering extends ValOrdering[BigDecimal] {
		override def compare(x: BigDecimal, y: BigDecimal): Int = x compare y
	}

	implicit case object StringOrdering extends ValOrdering[String] {
		override def compare(x: String, y: String): Int = x compareTo y
	}


	implicit class IterableOrdering[@specialized(Elements) E](elems :ValOrdering[E]) extends ValOrdering[FitIterable[E]] {
		override def compare(x: FitIterable[E], y: FitIterable[E]): Int = {
			val xi = x.iterator; val yi = y.iterator
			while (xi.hasNext && yi.hasNext) {
				val diff = elems.compare(xi.next(), yi.next())
				if (diff!=0)
					return diff //breaking out from the whole method!!!
			}
			0
		}
	}

	class GenericOrdering[E](gen :Ordering[E]) extends ValOrdering[E] {
		override def compare(x: E, y: E): Int = gen.compare(x, y)
		override def toString = s"ValOrdering($gen)"
	}
}