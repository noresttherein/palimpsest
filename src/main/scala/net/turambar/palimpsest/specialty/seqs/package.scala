package net.turambar.palimpsest.specialty
import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom

import scala.collection.generic.CanBuildFrom

/**
  * @author Marcin Mościcki
  */
package object seqs {
//	type ValSeq[@specialized(Elements) E] = ValSeqLike[E, ValSeq[E]]

//	val ValSeq = ValSeqLike

//	type ValArray[@specialized(Elements) E] = ArrayView[E] with ValSeq[E] with ValSeqLike[E, ArrayView[E]]
//
//	object ValArray extends ArrayViewFactory[ValArray] {
//		override protected def using[E](array: Array[E], offset: Int, length: Int): ValArray[E] =
//			SharedArray.view(array, offset, length)
//
//		@inline override implicit def canBuildFrom[E](implicit fit: CanFitFrom[ValArray[_], E, ValArray[E]]): CanBuildFrom[ValArray[_], E, ValArray[E]] =
//			fit.cbf
//	}
}
