package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.FitBuilder


/**
  * @author Marcin Mościcki
  */

object CharSet {


	private[this] final val CharToInt = (_:Char).toInt
	private[this] final val IntToChar = (_:Int).toChar

	final val Empty = new IntSet.ViewAs[Char](IntToChar, CharToInt)(IntSet.Empty)
	
	def empty :FitSet[Char] = Empty
	
	def newBuilder :FitBuilder[Char, FitSet[Char]] =
		IntSet.newBuilder.mapInput(CharToInt).mapResult(ints => new IntSet.ViewAs[Char](IntToChar, CharToInt)(ints))

	def Singleton(value :Char) :FitSet[Char] = new IntSet.ViewAs[Char](IntToChar, CharToInt)(IntSet.Singleton(value.toInt))

	type Sorted = FitSet.Sorted[Char]

	object Sorted {
		final val Empty :SortedFitSet[Char] = new IntSet.SortedViewAs[Char](IntToChar, CharToInt)(IntSet.Sorted.Empty)
		def newBuilder :FitBuilder[Char, SortedFitSet[Char]] = IntSet.Sorted.newBuilder.mapInput(CharToInt).mapResult(
			ints => new IntSet.SortedViewAs[Char](IntToChar, CharToInt)(ints)			
		)
		def Singleton(value :Char) :SortedFitSet[Char] = new IntSet.SortedViewAs[Char](IntToChar, CharToInt)(IntSet.Sorted.Singleton(value.toInt))
	}

	
}
