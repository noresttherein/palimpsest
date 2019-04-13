package net.turambar.palimpsest.specialty.arithmetic

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait Additive[N] {
	def plus(x :N, y :N) :N
}

object Additive {
	implicit val ShortIsAdditive :Additive[Short] = (x :Short, y :Short) => (x + y).toShort

	implicit val IntIsAdditive :Additive[Int] = (x :Int, y :Int) => x + y

	implicit val LongIsAdditive :Additive[Long] = (x :Long, y :Long) => x + y

	implicit val FloatIsAdditive :Additive[Float] = (x :Float, y :Float) => x + y

	implicit val DoubleIsAdditive :Additive[Double] = (x :Double, y :Double) => x + y

	implicit val BigIntIsAdditive :Additive[BigInt] = (x :BigInt, y :BigInt) => x + y

	implicit val BigDecimalIsAdditive :Additive[BigDecimal] = (x :BigDecimal, y :BigDecimal) => x + y
}
