package net.turambar.collection.specnaz

/**
  * @author Marcin Mościcki
  */
object SpecFun {
	import Specialized.Primitives
//	class SpecFun0[@specialized R] extends Function0
	type SpecFun0[+R] = Function0[R]


	trait SpecFun1[@specialized -X, @specialized +Y] {
		def apply(x :X) :Y
	}

	object SpecFun1 {
		implicit def unspecializeToFunction[X, Y](f :SpecFun1[X, Y]) :X => Y = { x => f(x) }
	}

	trait SpecFun2[@specialized(Primitives) -X1, @specialized(Primitives) -X2, @specialized(Primitives) +Y] {
		def apply(x1 :X1, x2 :X2) :Y
	}

	object SpecFun2 {
		implicit def unspecializeToFunction2[X1, X2, Y](f :SpecFun2[X1, X2, Y]) : (X1, X2) => Y =
			{ (x1, x2) => f(x1, x2) }
	}
	
/*
	trait Fun1[@specialized -X, @specialized +Y] {
		def apply(x :X) :Y
//		@annotation.unspecialized def compose[A](g: A => X): A => Y = { x => apply(g(x)) }
//
//		@annotation.unspecialized def andThen[A](g: Y => A): X => A = { x => g(apply(x)) }

	}

	trait Sub[@specialized -X, @specialized +Y] extends Fun1[X, Y]
*/

//	@inline final def spec[X, Y](f :X=>Y) :f.type = f

	@inline final def spec[@specialized X, @specialized Y](f :SpecFun1[X, Y]) :SpecFun1[X, Y] = f

//	@inline final def spec[@specialized(Primitives) X1, @specialized(Primitives) X2, @specialized(Primitives)]
//	@inline final def spec[@specialized(Primitives) X1, @specialized(Primitives) X2, @specialized Y](f :SpecFun2[X1, X2, Y]) :SpecFun2[X1, X2, Y] = f

	
//	def fun[X, Y](f :Fun1[X, Y]) :Fun1[X, Y] = f
//	def sub[X, Y](f :Sub[X, Y]) :Sub[X, Y] = f
//	fun{ x :Int => x }
//	sub{ x :Int => x }
//	spec{ x :Int => x }
//	val s0 :SpecFun1[Byte, Int] = { x :Byte => 1 }// :SpecFun1[Byte, Int]
//	val s = spec[Byte, Int]({ x :Byte => x*x } :SpecFun1[Byte, Int])
//	val s = spec({ x :Int => x*x })
}
