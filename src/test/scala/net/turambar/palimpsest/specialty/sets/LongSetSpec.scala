package net.turambar.palimpsest.specialty.sets

import org.scalacheck.Properties
import org.scalacheck.Prop._

/**
  * @author Marcin Mościcki
  */
object LongSetSpec extends Properties("DirectLongSet") {
	import java.lang.Long.compareUnsigned

	property("SortedLongSet.ordering") = forAll { (l1 :Long, l2 :Long) =>
		compareUnsigned(l1 - Long.MinValue, l2 - Long.MinValue).signum ?= (l1 compare l2).signum
	}
}
