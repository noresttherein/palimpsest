package net.turambar.palimpsest.specialty

package object maps {
	final val KeyTypes = new Specializable.Group((/*Byte, Short, */Int, Long, Char, Float, Double))
	final val ValueTypes = new Specializable.Group((/*Byte, Short, */Int, Long, Char, Float, Double))

//	type MapTypeConstructor[@specialized(KeyTypes) K, @specialized(ValueTypes) V] =
//		FitMap[K, V] with MapSpecialization[K, V, ({ type M[Y] = })]
}