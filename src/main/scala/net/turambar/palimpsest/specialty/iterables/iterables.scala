package net.turambar.palimpsest.specialty

import net.turambar.palimpsest.specialty.seqs.{MutableSeq, StableSeq}


package object iterables {
	type StableIterable[+E] = FitIterable[E] with StableIterableOverrides[E]

	final val StableIterable :FitCompanion[StableIterable] = StableSeq

	type MutableIterable[E] = FitIterable[E] with MutableIterableOverrides[E]

	final val MutableIterable :FitCompanion[MutableIterable] = MutableSeq
}