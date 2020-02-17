package net.noresttherein.palimpsest.aliases

import net.noresttherein.palimpsest.ItemTypes
import net.noresttherein.palimpsest.maps.{KeyTypes, MutableMap, MutableOrderedMap, ValueTypes}
import net.noresttherein.palimpsest.seqs.{AptBuffer, MutableSeq}
import net.noresttherein.palimpsest.sets.{MutableOrderedSet, MutableSet}


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object mutable {
	type Seq[@specialized(ItemTypes) E] = MutableSeq[E]
	final val Seq = MutableSeq

	type Buffer[@specialized(ItemTypes) E] = AptBuffer[E]
	final val Buffer = AptBuffer

	type Set[@specialized(ItemTypes) E] = MutableSet[E]
	final val Set = MutableSet

	type SortedSet[@specialized(ItemTypes) E] = MutableOrderedSet[E]
	final val SortedSet = MutableOrderedSet

	type Map[@specialized(KeyTypes) K, @specialized(ValueTypes) V] = MutableMap[K, V]
	final val Map = MutableMap

	type SortedMap[@specialized(KeyTypes) K, @specialized(ValueTypes) V] = MutableOrderedMap[K, V]
	final val SortedMap = MutableOrderedMap
}
