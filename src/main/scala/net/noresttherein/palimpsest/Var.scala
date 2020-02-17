package net.noresttherein.palimpsest

/** A box/reference for a mutable value. Allows for in/out parameters to functions.
  * Implicit conversions exist providing arithmetic suitable to the type of the boxed value, so for example
  * you can write `param += 1` for `param :Var[Int]`.
  * @param x initial value of this variable
  * @tparam T type of the boxed value
  */
sealed class Var[@specialized(Var.SpecializedTypes) T](private[this] var x :T) extends InOut[T] with Serializable {

	/** Current value of this variable. */
	@inline final override def get :T = x

	/** Current value of this variable. */
	@inline final override def value :T = x

	/** Assign a new value to this variable. */
	@inline final override def value_=(value :T) :Unit = this.x = value


	/** Assigns a new value to this variable. */
	@inline final override def :=(newValue :T) :Unit = x = newValue


	/** Assigns a new value returning the previous value at the same time. */
	@inline final override def ?=(newValue :T) :T = { val res = x; x = newValue; res }


	override def equals(that :Any) :Boolean = that match {
		case v :Var[_] => (v eq this) || v.get == x
		case v :InOut[_] => (v eq this) || (v canEqual this) && v.get == x
		case _ => false
	}

	override def hashCode :Int = x.hashCode

	override def toString :String = x.toString
}



/** Factory of specialized boxed mutable values. */
object Var {

	/** Types for which [[Var]] is specialized. */
	final val SpecializedTypes = new Specializable.Group(Byte, Short, Char, Int, Long, Float, Double, Boolean)

	/** Unbox the value hold by a `Var`. */
	@inline final implicit def unboxVar[@specialized(SpecializedTypes) T](variable :Var[T]) :T = variable.get

	/** Create a wrapper over a '''`var`''' of type `T` which can be passed as an in/out method parameter. */
	@inline def apply[@specialized(SpecializedTypes) T](value :T) :Var[T] = new Var[T](value)



	/** Extra implicits which might be helpful but can also lead to tricky bugs. */
	object implicits {

		/** Implicitly creates a `Var` with a given value. This implicit is optional as the main use of `Var[T]`
		  * is to be used as in/out method parameters. In that scenario, using a value identifier instead of a `Var[T]`
		  * makes no sense and would likely be an error.
		  */
		@inline implicit def boxVar[@specialized(SpecializedTypes) T](value :T) :Var[T] = new Var[T](value)

		type VarMultiAssignment[T] = InOut.implicits.InOutMultiAssignment[T, Var[T]]

		@inline implicit def VarMultiAssignment[@specialized(SpecializedTypes) T](value :T) :VarMultiAssignment[T] =
			new VarMultiAssignment(value)

//		val x, y, z = Var(0)
//		(x, y, z) =: 1
//		x =: y =: z =: 1
	}



	/** Implicit conversion of `Var[Boolean]` values providing logical operators. */
	implicit class BooleanVarLogic(private val x :Var[Boolean]) extends AnyVal {
		@inline def &=(other :Boolean) :Unit = x := x.get && other
		@inline def &&=(other: =>Boolean) :Unit = x := x.get && other
		@inline def |=(other :Boolean) :Unit = x := x.get || other
		@inline def ||=(other: =>Boolean) :Unit = x := x.get || other
		@inline def ^=(other :Boolean) :Unit = x := x.get ^ other
		@inline def neg() :Unit = x := !x.get
	}


	/** Implicit conversion of `Var[Int]` values providing arithmetic modifications. */
	implicit class IntVarOps(private val x :Var[Int]) extends AnyVal {
		@inline def +=(n :Int) :Unit = x := x.get + n
		@inline def -=(n :Int) :Unit = x := x.get - n
		@inline def *=(n :Int) :Unit = x := x.get * n
		@inline def /=(n :Int) :Unit = x := x.get / n
		@inline def %=(n :Int) :Unit = x := x.get % n

		@inline def ++ :Unit = x := x.get + 1
		@inline def -- :Unit = x := x.get - 1

		@inline def |=(n :Int) :Unit = x := x.get | n
		@inline def &=(n :Int) :Unit = x := x.get & n
		@inline def ^=(n :Int) :Unit = x := x.get ^ n

		@inline def >>=(n :Int) :Unit = x := x.get >> n
		@inline def >>>=(n :Int) :Unit = x := x.get >>> n
		@inline def <<=(n :Int) :Unit = x := x.get << n
		@inline def flip() :Unit = x := ~x.get
	}


	/** Implicit conversion of `Var[Long]` values providing arithmetic modifications. */
	implicit class LongVarOps(private val x :Var[Long]) extends AnyVal {
		@inline def +=(n :Long) :Unit = x := x.get + n
		@inline def -=(n :Long) :Unit = x := x.get - n
		@inline def *=(n :Long) :Unit = x := x.get * n
		@inline def /=(n :Long) :Unit = x := x.get / n
		@inline def %=(n :Long) :Unit = x := x.get % n

		//		@inline def neg() :Unit = x := -x.get

		@inline def ++ :Unit = x := x.get + 1L
		@inline def -- :Unit = x := x.get - 1L

		@inline def |=(n :Long) :Unit = x := x.get | n
		@inline def &=(n :Long) :Unit = x := x.get & n
		@inline def ^=(n :Long) :Unit = x := x.get ^ n

		@inline def >>=(n :Int) :Unit = x := x.get >> n
		@inline def >>>=(n :Int) :Unit = x := x.get >>> n
		@inline def <<=(n :Int) :Unit = x := x.get << n
		@inline def flip() :Unit = x := ~x.get
	}


	/** Implicit conversion of `Var[Float]` values providing arithmetic modifications. */
	implicit class FloatVarOps(private val x :Var[Float]) extends AnyVal {
		@inline def +=(n :Float) :Unit = x := x.get + n
		@inline def -=(n :Float) :Unit = x := x.get - n
		@inline def *=(n :Float) :Unit = x := x.get * n
		@inline def /=(n :Float) :Unit = x := x.get / n
	}


	/** Implicit conversion of `Var[Double]` values providing arithmetic modifications. */
	implicit class DoubleVarOps(private val x :Var[Double]) extends AnyVal {
		@inline def +=(n :Double) :Unit = x := x.get + n
		@inline def -=(n :Double) :Unit = x := x.get - n
		@inline def *=(n :Double) :Unit = x := x.get * n
		@inline def /=(n :Double) :Unit = x := x.get / n
	}




}

