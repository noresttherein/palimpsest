import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator}
import net.turambar.palimpsest.specialty.seqs.FitSeq
import net.turambar.palimpsest.specialty.sets.{ByteSet, ValSet, LongSet, SetSpecialization, StableSet}

import net.turambar.palimpsest.specialty.Elements
/**
  * @author Marcin Mościcki
  */
object playground extends App {

	class A; class B extends A; class C extends B

	trait Covariant[+This] {
		protected[this] def map(x :This, y :This) :This
	}

	class CoA(a :A) extends Covariant[A] {
		override protected[this] def map(x: A, y :A): A = x
	}

	class CoB(b :B) extends CoA(b) with Covariant[B] {
		override protected[this] def map(x: B, y :B): B = y
	}



	var empt = LongSet.empty
	println(empt); println(empt.toSeq); println(empt.iterator.toSeq); println

	for { i <- 0 until 10 by 2 } empt += i
	println(empt); println(empt.toSeq); println(empt.iterator.toSeq); println

	for { i <- 1 until 10 by 2} empt += -i
	println(empt); println(empt.toSeq); println(empt.iterator.toSeq); println

	for { i <- 1 until 10 by 2 } empt += i
	println(empt); println(empt.toSeq); println(empt.iterator.toSeq); println

	for { i <- 0 until 10 by 2 } empt += -i
	println(empt); println(empt.toSeq); println(empt.iterator.toSeq); println

	import Long.MinValue

	var sort = LongSet.empty
	for { i <- - 10 until 10 } sort += (i - MinValue)
	println(sort.toSeq.map(_ + MinValue))

/*		var gorszysort = LongSet.Sorted.empty
		for { i <- 0 until 16 } gorszysort += i
		for { i <- 0 until 16 } gorszysort += -i
//		val it = gorszysort.iterator
//		val items = it.toSeq
//
	//	println("set:      "+empt)
	//	println("iterated: "+items)
		for { l <- gorszysort }
			println(s"from($l): "+gorszysort.iteratorFrom(l).toSeq)
		println("from 77: " + gorszysort.iteratorFrom(77).toSeq)
*/

}
