package net.turambar

import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer

/**
  * @author Marcin Mościcki
  */
package object collection {

	implicit class Shuffler[T](val seq :Seq[T]) extends AnyVal {
		@inline private def rand(range :Int) = (math.random * range).toInt

		/** Return `this` sequence shuffled. As most commonly used sequences, even IndexedSeq[] don't have
		  * a particulary fast `updated` methods, the shuffling is done in a side buffer and the result
		  * is built using the builder associated with `this` sequence.
		  * @return
		  */
		def shuffled :Seq[T] = {
//			case _ :immutable.IndexedSeq[_] =>
//				val len = seq.length
//				(seq /: (0 until len-1)){ case (shuffling, i) =>
//					val swap = rand(len-i) + i
//					seq.updated(i, seq(swap)).updated(swap, seq(i))
//				}
//			case _ =>
			val array = ArrayBuffer[T](seq:_*)
//			seq.copyToArray(array)
			for (i <- 0 until array.length-2) {
				val swap = rand(array.length-i) +i; val stooge = array(swap)
				array(swap) = array(i); array(i) = stooge
			}
			val cbf = implicitly[CanBuildFrom[Seq[T], T, Seq[T]]]
			(cbf(seq) ++= array).result()
		}
	}


	implicit class IterableExtension[C[X]<:Iterable[X], E](private val items :C[E]) extends AnyVal {
		def unique[R](implicit cbf :CanBuildFrom[C[E], E, R]) :R = {
			var duplicate = mutable.Set[E]()
			var b = cbf(items)
			items.foreach { i =>
				if (!duplicate(i)) { duplicate += i; b += i }
			}
			b.result()
		}
	}
}
