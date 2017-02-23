package net.turambar.palimpsest.specialty.seqs

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._


/**
  * @author Marcin Mościcki
  */
object SharedArrayBufferSpec extends Properties("SharedArrayBuffer") {

	property("nextCapacity") = forAll { (cap :Int, req :Int) =>
		val current = cap.abs; val required = req.abs
		Prop(current >= required) || {
			val next = SharedArrayBuffer.nextCapacity(current, required)
			Prop(next >= required && next/2 <= required) :| s"grew from $current to $required: $next"
		}
	}
}
