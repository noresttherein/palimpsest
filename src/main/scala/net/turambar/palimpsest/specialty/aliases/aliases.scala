package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.iterables.FitIterable
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.seqs.FitSeq
import net.turambar.palimpsest.specialty.sets.{OrderedSet, ValSet}
import net.turambar.palimpsest.specialty.maps.{FitMap, KeyTypes, OrderedMap, ValueTypes}

/**
  * @author Marcin Mościcki
  */
package object aliases {

	type TraversableOnce[@specialized(ItemTypes) E] = FitTraversableOnce[E]
	final val TraversableOnce = FitTraversableOnce

	type Iterator[@specialized(ItemTypes) E] = FitIterator[E]
	type BufferedIterator[@specialized(ItemTypes) E] = FitIterator[E]
	final val Iterator = FitIterator


	type Iterable[@specialized(ItemTypes) +E] = FitIterable[E]
	final val Iterable = FitIterable

	type Seq[@specialized(ItemTypes) +E] = FitSeq[E]
	final val Seq = FitSeq

	type Set[@specialized(ItemTypes) E] = ValSet[E]
	final val Set = ValSet

	type SortedSet[@specialized(ItemTypes) E] = OrderedSet[E]
	final val SortedSet = OrderedSet

	type Map[@specialized(KeyTypes) K, @specialized(ValueTypes) +V] = FitMap[K, V]
	final val Map = FitMap

	type SortedMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V] = OrderedMap[K, V]
	final val SortedMap = OrderedMap
}
