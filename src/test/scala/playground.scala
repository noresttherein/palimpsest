import net.turambar.palimpsest.specialty.seqs.FitSeq

/**
  * @author Marcin Mościcki
  */
object playground extends App {
	
	
	val twoInts = FitSeq(42, 44)
	twoInts.foldLeft(0)((_:Int) + (_:Int))
	twoInts.traverse { i :Int => i*i; }
//	twoInts.items.foreach { i :Int => i*i; }
	val slice = twoInts.slice(0, 2)
	val filt = twoInts.filter { x => x % 2 == 0 }
	val swapped = twoInts.reverse
	val tail = twoInts.tail
	val doubled = twoInts ++ twoInts
	val dec = twoInts.map { x :Int => x - 2 }
	val squared = twoInts.flatMap { x => FitSeq(x, x*x) }
	
	val hidden = twoInts :Seq[Int]
	val sum = hidden ++ hidden
	hidden.map(_ -2)
	
}
