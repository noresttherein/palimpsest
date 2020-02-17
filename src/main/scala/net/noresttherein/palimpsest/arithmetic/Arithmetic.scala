package net.noresttherein.palimpsest.arithmetic

import net.noresttherein.palimpsest.RuntimeType.Specialized.Numbers

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait Arithmetic[@specialized(Numbers) N] {
	def plus(x :N, y :N) :N
	def minus(x :N, y :N) :N
	def multiply(x :N, y :N) :N
	def divide(x :N, y :N) :N

	def neg(x :N) :N

	def zero :N
	def one :N

	def eq(x :N, y :N) :Boolean
	def lt(x :N, y :N) :Boolean
	def lte(x :N, y :N) :Boolean
	def gt(x :N, y :N) :Boolean
	def gte(x :N, y :N) :Boolean
}


object Arithmetic {
	
	implicit val ShortArithmetic :Arithmetic[Short] = new Arithmetic[Short] {
		override def plus(x: Short, y: Short)  = (x + y).toShort
		override def minus(x: Short, y: Short)  = (x - y).toShort
		override def multiply(x: Short, y: Short)  = (x * y).toShort
		override def divide(x: Short, y: Short)  = (x / y).toShort
		
		override def neg(x: Short)  = (-x).toShort
		
		override def zero  :Short = 0
		override def one :Short  = 1

		override def eq(x: Short, y: Short)  = x == y
		override def lt(x: Short, y: Short)  = x < y
		override def lte(x: Short, y: Short)  = x <= y
		override def gt(x: Short, y: Short)  = x > y
		override def gte(x: Short, y: Short)  = x >= y
	}

	implicit val IntArithmetic :Arithmetic[Int] = new Arithmetic[Int] {
		override def plus(x: Int, y: Int)  = x + y
		override def minus(x: Int, y: Int)  = x - y
		override def multiply(x: Int, y: Int)  = x * y
		override def divide(x: Int, y: Int)  = x / y

		override def neg(x: Int)  = -x

		override def zero  = 0
		override def one  = 1

		override def eq(x: Int, y: Int)  = x == y
		override def lt(x: Int, y: Int)  = x < y
		override def lte(x: Int, y: Int)  = x <= y
		override def gt(x: Int, y: Int)  = x > y
		override def gte(x: Int, y: Int)  = x >= y	
	}
	
	implicit val LongArithmetic :Arithmetic[Long] = new Arithmetic[Long] {
		override def plus(x: Long, y: Long)  = x + y
		override def minus(x: Long, y: Long)  = x - y
		override def multiply(x: Long, y: Long)  = x * y
		override def divide(x: Long, y: Long)  = x / y

		override def neg(x: Long)  = -x

		override def zero  = 0L
		override def one  = 1L
		
		override def eq(x: Long, y: Long)  = x == y
		override def lt(x: Long, y: Long)  = x < y
		override def lte(x: Long, y: Long)  = x <= y
		override def gt(x: Long, y: Long)  = x > y
		override def gte(x: Long, y: Long)  = x >= y		
	}
	
	implicit val FloatArithmetic :Arithmetic[Float] = new Arithmetic[Float] {
		override def plus(x: Float, y: Float)  = x + y
		override def minus(x: Float, y: Float)  = x - y
		override def multiply(x: Float, y: Float)  = x * y
		override def divide(x: Float, y: Float)  = x / y

		override def neg(x: Float)  = -x

		override def zero  = 0F
		override def one  = 1F

		override def eq(x: Float, y: Float)  = x == y
		override def lt(x: Float, y: Float)  = x < y
		override def lte(x: Float, y: Float)  = x <= y
		override def gt(x: Float, y: Float)  = x > y
		override def gte(x: Float, y: Float)  = x >= y
	}
	
	implicit val DoubleArithmetic :Arithmetic[Double] = new Arithmetic[Double] {
		override def plus(x: Double, y: Double)  = x + y
		override def minus(x: Double, y: Double)  = x - y
		override def multiply(x: Double, y: Double)  = x * y
		override def divide(x: Double, y: Double)  = x / y
		
		override def neg(x: Double)  = -x
		
		override def zero  = 0.0
		override def one  = 1.0

		override def eq(x: Double, y: Double)  = x == y
		override def lt(x: Double, y: Double)  = x < y
		override def lte(x: Double, y: Double)  = x <= y
		override def gt(x: Double, y: Double)  = x > y
		override def gte(x: Double, y: Double)  = x >= y	
	}
	
	implicit val BigIntArithmetic :Arithmetic[BigInt] = new Arithmetic[BigInt] {
		override def plus(x: BigInt, y: BigInt)  = x + y
		override def minus(x: BigInt, y: BigInt)  = x - y
		override def multiply(x: BigInt, y: BigInt)  = x * y
		override def divide(x: BigInt, y: BigInt)  = x / y
		
		override def neg(x: BigInt)  = -x
		
		override def zero  = BigInt(0)
		override def one  = BigInt(1)
		
		override def eq(x: BigInt, y: BigInt)  = x == y
		override def lt(x: BigInt, y: BigInt)  = x < y
		override def lte(x: BigInt, y: BigInt)  = x <= y
		override def gt(x: BigInt, y: BigInt)  = x > y
		override def gte(x: BigInt, y: BigInt)  = x >= y		
	}
	
	implicit val BigDecimalArithmetic :Arithmetic[BigDecimal] = new Arithmetic[BigDecimal] {
		override def plus(x: BigDecimal, y: BigDecimal)  = x + y
		override def minus(x: BigDecimal, y: BigDecimal)  = x - y
		override def multiply(x: BigDecimal, y: BigDecimal)  = x * y
		override def divide(x: BigDecimal, y: BigDecimal)  = x / y

		override def neg(x: BigDecimal)  = -x

		override def zero  = BigDecimal(0)
		override def one  = BigDecimal(1)

		override def eq(x: BigDecimal, y: BigDecimal)  = x == y
		override def lt(x: BigDecimal, y: BigDecimal)  = x < y
		override def lte(x: BigDecimal, y: BigDecimal)  = x <= y
		override def gt(x: BigDecimal, y: BigDecimal)  = x > y
		override def gte(x: BigDecimal, y: BigDecimal)  = x >= y
	}
}
