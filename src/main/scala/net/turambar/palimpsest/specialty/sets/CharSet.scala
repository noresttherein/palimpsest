package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.FitBuilder


/**
  * @author Marcin Mościcki
  */

private[sets] object CharSet {


	private[this] final val CharToInt = (_:Char).toInt
	private[this] final val IntToChar = (_:Int).toChar

	final val Empty = new IntSet.ViewAs[Char](IntToChar, CharToInt)(IntSet.Empty)
	
	def empty :StableSet[Char] = Empty
	
	def newBuilder :FitBuilder[Char, StableSet[Char]] =
		IntSet.newBuilder.mapInput(CharToInt).mapResult(ints => new IntSet.ViewAs[Char](IntToChar, CharToInt)(ints))

	def singleton(value :Char) :StableSet[Char] = new IntSet.ViewAs[Char](IntToChar, CharToInt)(IntSet.singleton(value.toInt))

	def mutable :MutableSet[Char] = new IntSet.MutableViewAs[Char](IntToChar, CharToInt)(IntSet.mutable)
	//	type Sorted = OrderedSet[Char]


	object Ordered {
		final val Empty :StableOrderedSet[Char] = new IntSet.SortedViewAs[Char](IntToChar, CharToInt)(IntSet.Sorted.Empty)
		def newBuilder :FitBuilder[Char, StableOrderedSet[Char]] = IntSet.Sorted.newBuilder.mapInput(CharToInt).mapResult(
			ints => new IntSet.SortedViewAs[Char](IntToChar, CharToInt)(ints)
		)
		def Singleton(value :Char) :StableOrderedSet[Char] = new IntSet.SortedViewAs[Char](IntToChar, CharToInt)(IntSet.Sorted.singleton(value.toInt))

		def mutable :MutableOrderedSet[Char] = new IntSet.MutableSortedViewAs[Char](IntToChar, CharToInt)(IntSet.Sorted.mutable)
	}
	
//	type Sorted = StableSet.Sorted[Char]
//
//	object Sorted {
//		final val Empty :OrderedSet[Char] = new IntSet.SortedViewAs[Char](IntToChar, CharToInt)(IntSet.Sorted.Empty)
//		def newBuilder :FitBuilder[Char, OrderedSet[Char]] = IntSet.Sorted.newBuilder.mapInput(CharToInt).mapResult(
//			ints => new IntSet.SortedViewAs[Char](IntToChar, CharToInt)(ints)
//		)
//		def Singleton(value :Char) :OrderedSet[Char] = new IntSet.SortedViewAs[Char](IntToChar, CharToInt)(IntSet.Sorted.Singleton(value.toInt))
//	}
	
	

	
}
