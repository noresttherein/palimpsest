package net.noresttherein.palimpsest.aliases

import net.noresttherein.palimpsest.ItemTypes
import net.noresttherein.palimpsest.iterables.StableIterable
import net.noresttherein.palimpsest.maps.{KeyTypes, StableMap, StableOrderedMap, ValueTypes}
import net.noresttherein.palimpsest.seqs.StableSeq
import net.noresttherein.palimpsest.sets.{StableOrderedSet, StableSet}

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object immutable {

	type Iterable[@specialized(ItemTypes) E] = StableIterable[E]
	final val Iterable = StableIterable

	type Seq[@specialized(ItemTypes) +E] = StableSeq[E]
	final val Seq = StableSeq

	type Set[@specialized(ItemTypes) E] = StableSet[E]
	final val Set = StableSet

	type SortedSet[@specialized(ItemTypes) E] = StableOrderedSet[E]
	final val SortedSet = StableOrderedSet

	type Map[@specialized(KeyTypes) K, @specialized(ValueTypes) +V] = StableMap[K, V]
	final val Map = StableMap

	type SortedMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V] = StableOrderedMap[K, V]
	final val SortedMap = StableOrderedMap
}
