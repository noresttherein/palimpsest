package net.turambar.collection.specnaz

import java.util

import net.turambar.collection.specnaz.SpecCompanion.{ArraySection, SpecBuilder, SpecCanBuildFrom}

import scala.collection.immutable.{IndexedSeq=> immutableIndexedSeq}
import scala.reflect.ClassTag



class ConstArray[@specialized(Reified) +E] protected[specnaz]
		(final protected[this] val array :Array[E], final protected[specnaz] val offset :Int, final val length :Int)
	extends ConstSeq[E] with SharedArray[E] with SharedArrayLike[E, ConstArray] with SpecSeqLike[E, ConstArray[E]]
{

//	override def toIndexedSeq :immutableIndexedSeq[E] = this

	override protected[this] def factory: SharedArrayFactory[ConstArray] = ConstArray

	@inline
	final override protected def subseq(from: Int, until: Int): ConstArray[E] =
		if (from==until) ConstArray.Empty //ConstArray.empty[E](storageType)
		else new ConstArray[E](array, offset+from, until-from)

	override protected[this] def typeStringPrefix = "ConstArray"
}


object ConstArray extends SharedArrayFactory[ConstArray] { factory =>
	
	
	protected[specnaz] override def apply[E](contents: ArraySection[E]): ConstArray[E] = shared(contents.copy)

	protected def using[@specialized(Reified) E](array: Array[E], offset: Int, length: Int): ConstArray[E] =
		new ConstArray[E](array, offset, length)

}





