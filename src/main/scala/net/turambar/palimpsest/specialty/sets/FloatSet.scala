package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.FitBuilder


/**
  * @author Marcin Mościcki
  */
object FloatSet {
	import java.lang.Float.{intBitsToFloat, floatToRawIntBits}

	private[this] final val FloatToInt :Float => Int = floatToRawIntBits
	private[this] final val IntToFloat :Int=>Float = intBitsToFloat

	final val Empty = new IntSet.ViewAs[Float](IntToFloat, FloatToInt)(IntSet.Empty)
	
	def empty :FitSet[Float] = Empty
	
	def newBuilder :FitBuilder[Float, FitSet[Float]] =
		IntSet.newBuilder.mapInput(FloatToInt).mapResult(ints => new IntSet.ViewAs[Float](IntToFloat, FloatToInt)(ints))

	def Singleton(value :Float) :FitSet[Float] = new IntSet.ViewAs[Float](IntToFloat, FloatToInt)(IntSet.Singleton(floatToRawIntBits(value)))

//	type Sorted = FitSet.Sorted[Float]
//
//	object Sorted {
//		final val Empty :SortedFitSet[Float] = new IntSet.SortedViewAs[Float](IntToFloat, FloatToInt)(IntSet.Sorted.Empty)
//		def newBuilder :FitBuilder[Float, SortedFitSet[Float]] = IntSet.Sorted.newBuilder.mapInput(FloatToInt).mapResult(
//			ints => new IntSet.SortedViewAs[Float](IntToFloat, FloatToInt)(ints)
//		)
//		def Singleton(value :Float) :SortedFitSet[Float] = new IntSet.SortedViewAs[Float](IntToFloat, FloatToInt)(IntSet.Sorted.Singleton(value))
//	}

	
}
