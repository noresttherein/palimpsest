package net.turambar.palimpsest.specialty

package object maps {

	/** Key types for which `FitMap` is specialized. */
	final val KeyTypes = new Specializable.Group((/*Byte, Short, */Int, Long, Char, Float, Double))

	/** Value types for which `FitMap` is specialized. */
	final val ValueTypes = new Specializable.Group((/*Byte, Short, */Int, Long, Char, Float, Double))

	/** Value types which are also specialized `Tuple2` types. */
	private[maps] final val EntryTypes = new Specializable.Group((Int, Long, Char, Double))

}