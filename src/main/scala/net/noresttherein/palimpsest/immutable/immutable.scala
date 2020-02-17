package net.noresttherein.palimpsest

import net.noresttherein.palimpsest.iterables.StableIterable
import net.noresttherein.palimpsest.seqs.{StableIndexedSeq, StableSeq}
import net.noresttherein.palimpsest.sets.{StableOrderedSet, StableSet, StableTreeSet}
import net.noresttherein.palimpsest.maps.{KeyTypes, StableMap, StableOrderedMap, ValueTypes}

package object immutable {
	type AptIterable[@specialized(ItemTypes) +E] = StableIterable[E]
	final val AptIterable = StableIterable

	type AptSeq[@specialized(ItemTypes) +E] = StableSeq[E]
	final val AptSeq = StableSeq

	type ValSeq[@specialized(ItemTypes) E] = seqs.ValSeq[E] with StableSeq[E]
//	final val ValSeq

	type AptIndexedSeq[@specialized(ItemTypes) +E] = StableIndexedSeq[E]
	final val AptIndexedSeq = StableIndexedSeq



	type ValSet[@specialized(ItemTypes) E] = StableSet[E]
	final val ValSet = StableSet

	type OrderedSet[@specialized(ItemTypes) E] = StableOrderedSet[E]
	final val OrderedSet = StableOrderedSet

	type TreeSet[@specialized(ItemTypes) E] = StableTreeSet[E]
	final val TreeSet = StableTreeSet


	type AptMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V] = StableMap[K, V]
	final val AptMap = StableMap

	type OrderedMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V] = StableOrderedMap[K, V]
	final val OrderedMap = StableOrderedMap

}