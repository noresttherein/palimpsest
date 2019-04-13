package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.FitBuilder
/*

/** Factory for sets of characters, not to be confused with charsets - character code mappings.
  * Implementations provided here use [[IntSet]] behind the scenes and simply newRoot argument
  * and return types between `Int` and `Char`.
  * @author Marcin Mościcki
  */
private[sets] object CharSet {


	private[this] final val CharToInt = (_:Char).toInt
	private[this] final val IntToChar = (_:Int).toChar

	final val Empty = new IntSet.ViewAs[Char](IntToChar, CharToInt)(IntSet.Empty)
	
	def empty :StableSet[Char] = Empty

	/** A builder for immutable sets of characters creating an `IntSet` and wrapping it with [[IntSet.ViewAs]] at the end. */
	def newBuilder :FitBuilder[Char, StableSet[Char]] =
		IntSet.newBuilder.mapInput(CharToInt).mapResult(ints => new IntSet.ViewAs[Char](IntToChar, CharToInt)(ints))

	def singleton(value :Char) :StableSet[Char] = new IntSet.ViewAs[Char](IntToChar, CharToInt)(IntSet.singleton(value.toInt))

	def mutable :MutableSet[Char] = new IntSet.MutableViewAs[Char](IntToChar, CharToInt)(IntSet.mutable)


	object Ordered {
		final val Empty :StableOrderedSet[Char] = new IntSet.SortedViewAs[Char](IntToChar, CharToInt)(IntSet.Sorted.Empty)

		/** A builder for immutable, ordered sets creating an `IntSet` and wrapping it with [[IntSet.SortedViewAs]] at the end. */
		def newBuilder :FitBuilder[Char, StableOrderedSet[Char]] = IntSet.Sorted.newBuilder.mapInput(CharToInt).mapResult(
			ints => new IntSet.SortedViewAs[Char](IntToChar, CharToInt)(ints)
		)
		def singleton(value :Char) :StableOrderedSet[Char] = new IntSet.SortedViewAs[Char](IntToChar, CharToInt)(IntSet.Sorted.singleton(value.toInt))

		def mutable :MutableOrderedSet[Char] = new IntSet.MutableSortedViewAs[Char](IntToChar, CharToInt)(IntSet.Sorted.mutable)
	}


	
}
*/