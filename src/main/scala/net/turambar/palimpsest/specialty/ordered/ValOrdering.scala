package net.turambar.palimpsest.specialty.ordered

import net.turambar.palimpsest.specialty.{ItemTypes, RuntimeType, Specialize}
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.Fun1
import net.turambar.palimpsest.specialty.iterables.AptIterable
import net.turambar.palimpsest.specialty.ordered.ValOrdering.GenericOrdering
import net.turambar.palimpsest.specialty.RuntimeType.Specialized
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.Numbers
import net.turambar.palimpsest.specialty.Specialize.SpecializeSome



/** Specialization of the standard scala `Ordering` trait. Ordered collections in this library check if the passed
  * ordering is one of the implicit values defined here and use dedicated implementations for natural orderings of
  * standard value types. In addition to natural orderings, their reverse variants, returned by the `reverse` method,
  * for the similar purpose of checking for equality when comparing two collections.
  * @tparam K type of ordered elements.
  */
trait ValOrdering[@specialized(ItemTypes) K] extends Ordering[K] { ord =>

	def specialization :RuntimeType[K] = RuntimeType.specialized[K]

	override def compare(x :K, y :K) :Int

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
	def isNatural :Boolean = false
}







sealed class FallbackOrderingImplicits {
	implicit def genericOrdering[E](implicit gen :Ordering[E]) :ValOrdering[E] = new GenericOrdering[E](gen)
}



object ValOrdering extends FallbackOrderingImplicits {

	/** Summons implicit value for `ValOrdering[E]`. */
	def apply[E](implicit ordering :ValOrdering[E]) :ValOrdering[E] = ordering

	def natural[E :Specialized] :ValOrdering[E] = Natural()

	private[this] final val Natural :Specialize[ValOrdering] = new SpecializeSome[ValOrdering] {
		override def forByte = ByteOrdering
		override def forShort = ShortOrdering
		override def forChar = CharOrdering
		override def forInt = IntOrdering
		override def forLong = LongOrdering
		override def forFloat = FloatOrdering
		override def forDouble = DoubleOrdering
		override def forBoolean = BooleanOrdering
//		override def forUnit = UnitOrdering
//		override def forNothing = ???
//		override def forRef[E :RuntimeType] = ???
		override protected[this] def generic[E :RuntimeType] =
			throw new UnsupportedOperationException("ValOrdering.natural[" + RuntimeType[E] + "]")
	}


	/** Marker trait for natural orderings of built-in value types. Sealed, so can be used to quickly determine
	  * if an implicit parameter is one of the defaults provided here and perform some optimizations.
	  */
	sealed trait NaturalValOrdering[T] { this :ValOrdering[T] =>
		def ordering :ValOrdering[T] = this

		override def isNatural = true
	}


	implicit case object BooleanOrdering extends ValOrdering[Boolean] with NaturalValOrdering[Boolean] {
		override def specialization :RuntimeType[Boolean] = RuntimeType.OfBoolean

		override def compare(x: Boolean, y: Boolean): Int =
			if (x)
				if (y) 0 else 1
			else
				if (y) -1 else 0

		override def reverse :ValOrdering[Boolean] = ReverseBooleanOrdering
	}

	implicit case object ByteOrdering extends ValOrdering[Byte] with NaturalValOrdering[Byte] {
		override def specialization :RuntimeType[Byte] = RuntimeType.OfByte

		override def compare(x: Byte, y: Byte): Int = x.toInt - y.toInt

		override def reverse :ValOrdering[Byte] = ReverseByteOrdering
	}

	implicit case object ShortOrdering extends ValOrdering[Short] with NaturalValOrdering[Short] {
		override def specialization :RuntimeType[Short] = RuntimeType.OfShort

		override def compare(x :Short, y :Short) :Int = x.toInt - y.toInt

		override def reverse :ValOrdering[Short] = ReverseShortOrdering
	}

	implicit case object CharOrdering extends ValOrdering[Char] with NaturalValOrdering[Char] {
		override def specialization :RuntimeType[Char] = RuntimeType.OfChar

		override def compare(x: Char, y: Char): Int = x.toInt - y.toInt

		override def reverse :ValOrdering[Char] = ReverseCharOrdering
	}

	implicit case object IntOrdering extends ValOrdering[Int] with NaturalValOrdering[Int] {
		override def specialization :RuntimeType[Int] = RuntimeType.OfInt

		override def compare(x :Int, y :Int) :Int = (x.toLong - y.toLong).signum

		override def reverse :ValOrdering[Int] = ReverseIntOrdering
	}


	implicit case object LongOrdering extends ValOrdering[Long] with NaturalValOrdering[Long] {
		override def specialization :RuntimeType[Long] = RuntimeType.OfLong

		override def compare(x: Long, y: Long): Int = if (x < y) -1 else if (x > y) 1 else 0

		override def lteq(x: Long, y: Long): Boolean = x <= y

		override def gteq(x: Long, y: Long): Boolean = x >= y

		override def lt(x: Long, y: Long): Boolean = x < y

		override def gt(x: Long, y: Long): Boolean = x > y

		override def equiv(x: Long, y: Long): Boolean = x == y

		override def max(x: Long, y: Long): Long = if (x >= y) x else y

		override def min(x: Long, y: Long): Long = if (x <= y) x else y

		override def reverse :ValOrdering[Long] = ReverseLongOrdering
	}

	implicit case object FloatOrdering extends ValOrdering[Float] with NaturalValOrdering[Float] {
		override def specialization :RuntimeType[Float] = RuntimeType.OfFloat

		override def compare(x: Float, y: Float): Int = if (x < y) -1 else if (x > y) 1 else 0

		override def reverse :ValOrdering[Float] = ReverseFloatOrdering
	}

	implicit case object DoubleOrdering extends ValOrdering[Double] with NaturalValOrdering[Double] {
		override def specialization :RuntimeType[Double] = RuntimeType.OfDouble

		override def compare(x :Double, y :Double) :Int = if (x < y) -1 else if (x > y) 1 else 0

		override def reverse :ValOrdering[Double] = ReverseDoubleOrdering
	}


	implicit case object BigIntOrdering extends ValOrdering[BigInt] with NaturalValOrdering[BigInt] {
		override val specialization :RuntimeType[BigInt] = RuntimeType.of[BigInt]

		override def compare(x: BigInt, y: BigInt): Int = x compare y

		override def reverse :ValOrdering[BigInt] = ReverseBigIntOrdering
	}

	implicit case object BigDecimalOrdering extends ValOrdering[BigDecimal] with NaturalValOrdering[BigDecimal] {
		override val specialization :RuntimeType[BigDecimal] = RuntimeType.of[BigDecimal]

		override def compare(x: BigDecimal, y: BigDecimal): Int = x compare y

		override def reverse :ValOrdering[BigDecimal] = ReverseBigDecimalOrdering
	}

	implicit case object StringOrdering extends ValOrdering[String] {
		override val specialization :RuntimeType[String] = RuntimeType.of[String]

		override def compare(x: String, y: String): Int = x compareTo y

		override def reverse :ValOrdering[String] = ReverseStringOrdering
	}



	implicit class IterableOrdering[@specialized(ItemTypes) E](elems :ValOrdering[E]) extends ValOrdering[AptIterable[E]] {
		override def compare(x: AptIterable[E], y: AptIterable[E]): Int = {
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
	
	
	
	sealed trait ReverseValOrdering[T] extends ValOrdering[T]


	case object ReverseBooleanOrdering extends ValOrdering[Boolean] with ReverseValOrdering[Boolean] {
		override def compare(x: Boolean, y: Boolean): Int =
			if (x)
				if (y) 1 else 0
			else
				if (y) 0 else -1

		override def reverse :ValOrdering[Boolean] = BooleanOrdering
	}

	case object ReverseByteOrdering extends ValOrdering[Byte] with ReverseValOrdering[Byte] {
		override def compare(x: Byte, y: Byte): Int = y.toInt - x.toInt

		override def reverse :ValOrdering[Byte] = ByteOrdering
	}

	case object ReverseShortOrdering extends ValOrdering[Short] with ReverseValOrdering[Short] {
		override def compare(x :Short, y :Short) :Int = y.toInt - x.toInt

		override def reverse :ValOrdering[Short] = ShortOrdering
	}

	case object ReverseCharOrdering extends ValOrdering[Char] with ReverseValOrdering[Char] {
		override def compare(x: Char, y: Char): Int = y.toInt - x.toInt

		override def reverse :ValOrdering[Char] = CharOrdering
	}

	case object ReverseIntOrdering extends ValOrdering[Int] with ReverseValOrdering[Int] {
		override def compare(x :Int, y :Int) :Int = (y.toLong - x.toLong).signum

		override def reverse :ValOrdering[Int] = IntOrdering
	}


	case object ReverseLongOrdering extends ValOrdering[Long] with ReverseValOrdering[Long] {

		override def compare(x: Long, y: Long): Int = if (x < y) 1 else if (x > y) -1 else 0

		override def lteq(x: Long, y: Long): Boolean = x >= y

		override def gteq(x: Long, y: Long): Boolean = x <= y

		override def lt(x: Long, y: Long): Boolean = x > y

		override def gt(x: Long, y: Long): Boolean = x < y

		override def equiv(x: Long, y: Long): Boolean = x == y

		override def max(x: Long, y: Long): Long = if (x < y) x else y

		override def min(x: Long, y: Long): Long = if (x > y) x else y

		override def reverse :ValOrdering[Long] = LongOrdering
	}

	case object ReverseFloatOrdering extends ValOrdering[Float] with ReverseValOrdering[Float] {
		override def compare(x: Float, y: Float): Int = if (x < y) 1 else if (x > y) -1 else 0

		override def reverse :ValOrdering[Float] = FloatOrdering
	}

	case object ReverseDoubleOrdering extends ValOrdering[Double] with ReverseValOrdering[Double] {
		override def compare(x :Double, y :Double) :Int = if (x < y) 1 else if (x > y) -1 else 0

		override def reverse :ValOrdering[Double] = DoubleOrdering
	}


	case object ReverseBigIntOrdering extends ValOrdering[BigInt] with ReverseValOrdering[BigInt] {
		override def compare(x: BigInt, y: BigInt): Int = y compare x

		override def reverse :ValOrdering[BigInt] = BigIntOrdering
	}

	case object ReverseBigDecimalOrdering extends ValOrdering[BigDecimal] with ReverseValOrdering[BigDecimal] {
		override def compare(x: BigDecimal, y: BigDecimal): Int = y compare x

		override def reverse :ValOrdering[BigDecimal] =  BigDecimalOrdering
	}

	case object ReverseStringOrdering extends ValOrdering[String] {
		override def compare(x: String, y: String): Int = y compareTo x

		override def reverse :ValOrdering[String] = StringOrdering
	}


}