package net.turambar.collection.specnaz

import net.turambar.collection.specnaz.SpecCompanion.{ArraySection$, SpecBuilder}

import scala.collection.generic.{GenericCompanion, GenericTraversableTemplate}
import scala.collection.mutable
import scala.reflect.ClassTag



trait ArraySlice[@specialized(Reified) E]
	extends MutableSeq[E] with SharedArray[E] with ArraySliceLike[E, ArraySlice]
{

	override protected[this] def typeStringPrefix = "ArraySlice"

}



object ArraySlice extends SharedArrayFactory[ArraySlice] {



	@inline
	final override protected def using[@specialized(Reified) E](array: Array[E], offset: Int, length: Int): ArraySlice[E] =
		new ArraySliceImpl[E](array, offset, length)


	private class ArraySliceImpl[@specialized(Reified) E](protected[this] final val array :Array[E], protected[specnaz] final val offset :Int, final val length :Int)
		extends ArraySlice[E]
	{
		override protected def subseq(from: Int, until: Int): ArraySlice[E] =
			new ArraySliceImpl[E](array, offset + from, until-from)

		override protected[this] def factory: SharedArrayFactory[ArraySlice] = ArraySlice
	}

}

