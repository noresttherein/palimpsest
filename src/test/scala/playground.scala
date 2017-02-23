import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator}
import net.turambar.palimpsest.specialty.seqs.FitSeq
import net.turambar.palimpsest.specialty.sets.{ByteSet, FitSet, LongSet, SetSpecialization, StableSet}

import net.turambar.palimpsest.specialty.Elements
/**
  * @author Marcin Mościcki
  */
object playground extends App {



	val empt = LongSet.Mutable.empty
	println(empt); println(empt.toSeq); println(empt.iterator.toSeq); println

	for { i <- 0 until 10 by 2 } empt += i
	println(empt); println(empt.toSeq); println(empt.iterator.toSeq); println

	for { i <- 1 until 10 by 2} empt += -i
	println(empt); println(empt.toSeq); println(empt.iterator.toSeq); println

	for { i <- 1 until 10 by 2 } empt += i
	println(empt); println(empt.toSeq); println(empt.iterator.toSeq); println

	for { i <- 0 until 10 by 2 } empt += -i
	println(empt); println(empt.toSeq); println(empt.iterator.toSeq); println



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
