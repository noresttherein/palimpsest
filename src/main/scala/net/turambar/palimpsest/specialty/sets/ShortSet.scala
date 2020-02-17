package net.turambar.palimpsest.specialty.sets
/*

import net.turambar.palimpsest.specialty.AptIterable.{IterableMapping}
import net.turambar.palimpsest.specialty.FitIterator.{BaseIterator, MappedIterator}
import net.turambar.palimpsest.specialty.Specialized.{Fun1Res, Fun1Vals, Fun2, Fun2Vals}
import net.turambar.palimpsest.specialty.sets.ValSet.Sorted
import net.turambar.palimpsest.specialty.{AptBuilder, FitIterator, forceFit}


/** A factory for immutable and mutable sets of `Short`s.
  * All created instances are simple wrappers for corresponding [[IntSet]] implementations simply
  * mapping argument and return types between `Int` and `Short`.
  * @author Marcin Mościcki
  */
private[sets] object ShortSet {


	private[this] final val ShortToInt = (_:Short).toInt
	private[this] final val IntToShort = (_:Int).toShort

	final val Empty :StableSet[Short] = new IntSet.ViewAs[Short](IntToShort, ShortToInt)(IntSet.Empty)

	def empty :StableSet[Short] = Empty

	def newBuilder :AptBuilder[Short, StableSet[Short]] =
		IntSet.newBuilder.mapInput(ShortToInt).mapResult(ints => new IntSet.ViewAs[Short](IntToShort, ShortToInt)(ints))

	def singleton(value :Short) :StableSet[Short] = new IntSet.ViewAs[Short](IntToShort, ShortToInt)(IntSet.singleton(value.toInt))

	def mutable :MutableSet[Short] = new IntSet.MutableViewAs[Short](IntToShort, ShortToInt)(IntSet.mutable)


	object Ordered {
		final val Empty :StableOrderedSet[Short] = new IntSet.SortedViewAs[Short](IntToShort, ShortToInt)(IntSet.Sorted.Empty)
		def newBuilder :AptBuilder[Short, StableOrderedSet[Short]] = IntSet.Sorted.newBuilder.mapInput(ShortToInt).mapResult(
			ints => new IntSet.SortedViewAs[Short](IntToShort, ShortToInt)(ints)
		)
		def singleton(value :Short) :StableOrderedSet[Short] = new IntSet.SortedViewAs[Short](IntToShort, ShortToInt)(IntSet.Sorted.singleton(value.toInt))

		def mutable :MutableOrderedSet[Short] = new IntSet.MutableSortedViewAs[Short](IntToShort, ShortToInt)(IntSet.Sorted.mutable)
	}

}
*/
