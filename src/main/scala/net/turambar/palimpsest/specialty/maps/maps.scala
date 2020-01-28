package net.turambar.palimpsest.specialty

package object maps {

	/** Key types for which `FitMap` is specialized. */
	final val KeyTypes = new Specializable.Group((/*Byte, Short, */Int, Long, Char, Float, Double))

	/** Value types for which `FitMap` is specialized. */
	final val ValueTypes = new Specializable.Group((/*Byte, Short, */Int, Long, Char, Float, Double))

	private[maps] final val UnitAndValueTypes = new Specializable.Group((Int, Long, Char, Float, Double, Unit))

	private[maps] final val RawKeyTypes = new Specializable.Group((Int, Long))
	private[maps] final val RawValueTypes = new Specializable.Group((Int, Long, Unit))

	/** Value types which are also specialized `Tuple2` types. */
	private[maps] final val EntryTypes = new Specializable.Group((Int, Long, Char, Double))

}