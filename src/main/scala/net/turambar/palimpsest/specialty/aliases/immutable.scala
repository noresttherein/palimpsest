package net.turambar.palimpsest.specialty.aliases

import net.turambar.palimpsest.specialty.Elements
import net.turambar.palimpsest.specialty.iterables.StableIterable
import net.turambar.palimpsest.specialty.maps.{KeyTypes, ValueTypes}
import net.turambar.palimpsest.specialty.maps.StableMap
import net.turambar.palimpsest.specialty.seqs.StableSeq
import net.turambar.palimpsest.specialty.sets.StableSet

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object immutable {

	type Iterable[@specialized(Elements) E] = StableIterable[E]
	final val Iterable = StableIterable

	type Seq[@specialized(Elements) +E] = StableSeq[E]
	final val Seq = StableSeq

	type Set[@specialized(Elements) E] = StableSet[E]
	final val Set = StableSet

	type Map[@specialized(KeyTypes) K, @specialized(ValueTypes) +V] = StableMap[K, V]
	final val Map = StableMap

}
