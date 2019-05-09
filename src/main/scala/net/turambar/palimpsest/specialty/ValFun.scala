package net.turambar.palimpsest.specialty

import Specialized.{All, Primitives, Fun2, Fun2Vals}
import ValFun.{ArgTypes, ResTypes, =>:}

trait FunctionArgumentSpecialization[-X, +Y] extends (X => Y) {

}


trait ValFun[@specialized(Primitives) -X, @specialized(All) +Y] extends (X => Y) {
	override def apply(x :X) :Y

	def accepts :Specialized[_] = domain

	def returns :Specialized[_] = range

	def *:[W](g :W => X) :W => Y

	def /[Z](g :Y => Z) :X => Z

//	override def compose[A](g :A => X) :A => Y
//
//	override def andThen[A](g :Y => A) :X => A = super.andThen(g)


	protected[this] def domain :Specialized[X] = Specialized[X]

	protected[this] def range :Specialized[Y] = Specialized[Y]

	override def toString = s"${domain.typeName} => ${range.typeName}"

}


object ValFun {
	val ArgTypes = Specialized.Primitives
	val ResTypes = Specialized.All

//	type =>:[@specialized(ArgTypes) -X, @specialized(ResTypes) +Y] = ValFun[X, Y]

	type =>:[-X, +Y] = X => Y

}