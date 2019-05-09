package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.Specialized.Fun1Vals

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait FunctorLike[+E, +F[+X]] {
	private[this] type MapFun[+O] = E => O

	def fitMap[O :Specialized](f :E => O) :F[O] = {
		val cons = new Specialize.With[F, MapFun] {
			override def specialized[@specialized Y :Specialized](fun :E => Y) :F[Y] = map(fun)
		}
		cons(f)
	}

	def map[@specialized(Fun1Vals) O](f :E => O) :F[O]
}


