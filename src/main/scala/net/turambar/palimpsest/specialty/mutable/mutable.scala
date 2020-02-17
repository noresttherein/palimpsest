package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.iterables.MutableIterable
import net.turambar.palimpsest.specialty.seqs.{MutableSeq}
import net.turambar.palimpsest.specialty.sets.{MutableOrderedSet, MutableSet, MutableTreeSet}
import net.turambar.palimpsest.specialty.maps.{KeyTypes, MutableMap, MutableOrderedMap, ValueTypes}

package object mutable {
	type AptIterable[@specialized(ItemTypes) E] = MutableIterable[E]
	final val AptIterable = MutableIterable

	type AptSeq[@specialized(ItemTypes) E] = MutableSeq[E]
	final val AptSeq = MutableSeq

//	type AptIndexedSeq[@specialized(ItemTypes) +E] = MutableIndexedSeq[E]
//	final val AptIndexedSeq = MutableIndexedSeq

	type ValSeq[@specialized(ItemTypes) E] = seqs.ValSeq[E] with MutableSeq[E]


	type ValSet[@specialized(ItemTypes) E] = MutableSet[E]
	final val ValSet = MutableSet

	type OrderedSet[@specialized(ItemTypes) E] = MutableOrderedSet[E]
	final val OrderedSet = MutableOrderedSet

	type TreeSet[@specialized(ItemTypes) E] = MutableTreeSet[E]
	final val TreeSet = MutableTreeSet


	type AptMap[@specialized(KeyTypes) K, @specialized(ValueTypes) V] = MutableMap[K, V]
	final val AptMap = MutableMap

	type OrderedMap[@specialized(KeyTypes) K, @specialized(ValueTypes) V] = MutableOrderedMap[K, V]
	final val OrderedMap = MutableOrderedMap

}