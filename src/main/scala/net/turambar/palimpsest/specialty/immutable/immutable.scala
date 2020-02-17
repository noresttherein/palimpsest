package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.iterables.StableIterable
import net.turambar.palimpsest.specialty.seqs.{StableIndexedSeq, StableSeq}
import net.turambar.palimpsest.specialty.sets.{StableOrderedSet, StableSet, StableTreeSet}
import net.turambar.palimpsest.specialty.maps.{KeyTypes, StableMap, StableOrderedMap, ValueTypes}

package object immutable {
	type FitIterable[@specialized(ItemTypes) +E] = StableIterable[E]
	final val FitIterable = StableIterable

	type FitSeq[@specialized(ItemTypes) +E] = StableSeq[E]
	final val FitSeq = StableSeq

	type ValSeq[@specialized(ItemTypes) E] = seqs.ValSeq[E] with StableSeq[E]
//	final val ValSeq

	type FitIndexedSeq[@specialized(ItemTypes) +E] = StableIndexedSeq[E]
	final val FitIndexedSeq = StableIndexedSeq



	type ValSet[@specialized(ItemTypes) E] = StableSet[E]
	final val ValSet = StableSet

	type OrderedSet[@specialized(ItemTypes) E] = StableOrderedSet[E]
	final val OrderedSet = StableOrderedSet

	type TreeSet[@specialized(ItemTypes) E] = StableTreeSet[E]
	final val TreeSet = StableTreeSet


	type FitMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V] = StableMap[K, V]
	final val FitMap = StableMap

	type OrderedMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V] = StableOrderedMap[K, V]
	final val OrderedMap = StableOrderedMap

}