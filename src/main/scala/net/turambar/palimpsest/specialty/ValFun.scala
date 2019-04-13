package net.turambar.palimpsest.specialty

import Specialized.{All, Primitives}



trait ValFun[@specialized(Primitives) -X, @specialized(All) +Y] extends Function1[X, Y] {
	override def apply(x :X) :Y

	def accepts :Specialized[_] = accepts

	def returns :Specialized[_] = range

	protected[this] def domain :Specialized[X] = Specialized[X]

	protected[this] def range :Specialized[Y] = Specialized[Y]

	override def toString = s"$domain => $range"

}


