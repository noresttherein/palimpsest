package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.iterables.MutableIterable
import net.turambar.palimpsest.specialty.seqs.{MutableSeq}
import net.turambar.palimpsest.specialty.sets.{MutableOrderedSet, MutableSet, MutableTreeSet}
import net.turambar.palimpsest.specialty.maps.{KeyTypes, MutableMap, MutableOrderedMap, ValueTypes}

package object mutable {
	type FitIterable[@specialized(ItemTypes) E] = MutableIterable[E]
	final val FitIterable = MutableIterable

	type FitSeq[@specialized(ItemTypes) E] = MutableSeq[E]
	final val FitSeq = MutableSeq

//	type FitIndexedSeq[@specialized(ItemTypes) +E] = MutableIndexedSeq[E]
//	final val FitIndexedSeq = MutableIndexedSeq

	type ValSeq[@specialized(ItemTypes) E] = seqs.ValSeq[E] with MutableSeq[E]


	type ValSet[@specialized(ItemTypes) E] = MutableSet[E]
	final val ValSet = MutableSet

	type OrderedSet[@specialized(ItemTypes) E] = MutableOrderedSet[E]
	final val OrderedSet = MutableOrderedSet

	type TreeSet[@specialized(ItemTypes) E] = MutableTreeSet[E]
	final val TreeSet = MutableTreeSet


	type FitMap[@specialized(KeyTypes) K, @specialized(ValueTypes) V] = MutableMap[K, V]
	final val FitMap = MutableMap

	type OrderedMap[@specialized(KeyTypes) K, @specialized(ValueTypes) V] = MutableOrderedMap[K, V]
	final val OrderedMap = MutableOrderedMap

}