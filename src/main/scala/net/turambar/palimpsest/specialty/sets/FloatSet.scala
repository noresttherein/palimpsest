package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.AptBuilder

/*
/**
  * @author Marcin Mościcki
  */
object FloatSet {
	import java.lang.Float.{intBitsToFloat, floatToRawIntBits}

	private[this] final val FloatToInt :Float => Int = floatToRawIntBits
	private[this] final val IntToFloat :Int=>Float = intBitsToFloat

	final val Empty :StableSet[Float] = new IntSet.ViewAs[Float](IntToFloat, FloatToInt)(IntSet.Empty)
	
	def empty :StableSet[Float] = Empty
	
	def newBuilder :AptBuilder[Float, StableSet[Float]] =
		IntSet.newBuilder.mapInput(FloatToInt).mapResult(ints => new IntSet.ViewAs[Float](IntToFloat, FloatToInt)(ints))

	def singleton(value :Float) :StableSet[Float] = new IntSet.ViewAs[Float](IntToFloat, FloatToInt)(IntSet.singleton(floatToRawIntBits(value)))

	def mutable :MutableSet[Float] = new IntSet.MutableViewAs[Float](IntToFloat, FloatToInt)(IntSet.mutable)
//	type Sorted = FitSet.Sorted[Float]
//
//	object Sorted {
//		final val Empty :SortedFitSet[Float] = new IntSet.SortedViewAs[Float](IntToFloat, FloatToInt)(IntSet.Sorted.Empty)
//		def newBuilder :AptBuilder[Float, SortedFitSet[Float]] = IntSet.Sorted.newBuilder.mapInput(FloatToInt).mapResult(
//			ints => new IntSet.SortedViewAs[Float](IntToFloat, FloatToInt)(ints)
//		)
//		def Singleton(value :Float) :SortedFitSet[Float] = new IntSet.SortedViewAs[Float](IntToFloat, FloatToInt)(IntSet.Sorted.Singleton(value))
//	}

	
}
*/