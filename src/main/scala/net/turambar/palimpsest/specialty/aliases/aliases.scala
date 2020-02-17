package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.iterables.AptIterable
import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.seqs.AptSeq
import net.turambar.palimpsest.specialty.sets.{OrderedSet, ValSet}
import net.turambar.palimpsest.specialty.maps.{AptMap, KeyTypes, OrderedMap, ValueTypes}

/**
  * @author Marcin Mościcki
  */
package object aliases {

	type TraversableOnce[@specialized(ItemTypes) E] = Vals[E]
	final val TraversableOnce = Vals

	type Iterator[@specialized(ItemTypes) E] = AptIterator[E]
	type BufferedIterator[@specialized(ItemTypes) E] = AptIterator[E]
	final val Iterator = AptIterator


	type Iterable[@specialized(ItemTypes) +E] = AptIterable[E]
	final val Iterable = AptIterable

	type Seq[@specialized(ItemTypes) +E] = AptSeq[E]
	final val Seq = AptSeq

	type Set[@specialized(ItemTypes) E] = ValSet[E]
	final val Set = ValSet

	type SortedSet[@specialized(ItemTypes) E] = OrderedSet[E]
	final val SortedSet = OrderedSet

	type Map[@specialized(KeyTypes) K, @specialized(ValueTypes) +V] = AptMap[K, V]
	final val Map = AptMap

	type SortedMap[@specialized(KeyTypes) K, @specialized(ValueTypes) +V] = OrderedMap[K, V]
	final val SortedMap = OrderedMap
}
