package net.turambar.palimpsest.specialty.aliases

import net.turambar.palimpsest.specialty.Elements
import net.turambar.palimpsest.specialty.maps.{KeyTypes, MutableMap, ValueTypes}
import net.turambar.palimpsest.specialty.seqs.{FitBuffer, MutableSeq}
import net.turambar.palimpsest.specialty.sets.MutableSet


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object mutable {
	type Seq[@specialized(Elements) E] = MutableSeq[E]
	final val Seq = MutableSeq

	type Buffer[@specialized(Elements) E] = FitBuffer[E]
	final val Buffer = FitBuffer

	type Set[@specialized(Elements) E] = MutableSet[E]
	final val Set = MutableSet

	type Map[@specialized(KeyTypes) K, @specialized(ValueTypes) V] = MutableMap[K, V]
	final val Map = MutableMap

}
