package net.turambar.collection.specnaz

import net.turambar.collection.specnaz.ArraySlice.ArraySliceImpl
import net.turambar.collection.specnaz.SpecCompanion.{ArraySection, SpecBuilder, SpecCanBuildFrom}
import scala.reflect._






trait SharedArray[@specialized(Reified) +E]
	extends SpecSeq[E] with SharedArrayLike[E, SharedArray] with IndexedSeq[E]





trait SharedArrayFactory[S[@specialized(Reified) X] <: SharedArray[X] with SharedArrayLike[X, S]]
	extends SpecSeqFactory[S]
{ outer =>

	@inline
	final override def empty[@specialized(Reified) E]: S[E] = using(Specialized.erasedArray[E], 0, 0)

	@inline final def emptyOfClass[E](elementType :Class[E]) :S[E] = emptyOf(ClassTag(elementType))

	@inline final def emptyOf[E :ClassTag] :S[E] = specialize(new ArraySection(new Array[E](0)))(Specialized.of[E])



	def apply[E](contents :Array[E]) :S[E] = apply(ArraySection.share(contents))

	def apply[E](contents :Array[E], offset :Int, length :Int) :S[E] = apply(ArraySection.share(contents, offset, length))

	def apply[E](from :Int, contents :Array[E], until :Int) :S[E] = apply(ArraySection.share(from, contents, until))

	protected[specnaz] def apply[E](contents :ArraySection[E]) :S[E] = specialize(contents)(contents.specialization)


	@inline final def of[E <:AnyVal :ClassTag](size :Int) :S[E] = shared(ArraySection.share[E](new Array[E](size)))

	@inline final def of[E](size :Int, value :E)(implicit elementType :ClassTag[E]=ClassTag(value.getClass)) :S[E] =
		shared(ArraySection.share[E](arrayFill(new Array[E](size), value)))

	def ofClass[E<:AnyVal](elementType :Class[E], size :Int) :S[E] = of(size)(ClassTag(elementType))

	def ofClass[E](elementType :Class[E], size :Int, value :E) :S[E] = of(size, value)(ClassTag(elementType))



	@inline final def copy[E](contents :Array[E]) :S[E] = shared(ArraySection.copy(contents))

	@inline final def copy[E](contents :Array[E], offset :Int, length :Int) :S[E] = shared(ArraySection.copy(contents, offset, length))

//	protected[specnaz] def view[E](contents :ArrayWindow[E]) :S[E] = specialize(contents)(contents.specialization)
	protected[specnaz] def shared[E](contents :ArraySection[E]) :S[E] = specialize(contents)(contents.specialization)

	private[this] final val specialize = new Specialize.With[S, ArraySection] {
		override def specialized[@specialized E: Specialized](param: ArraySection[E]): S[E] =
			using(param.array, param.start, param.length)
	}


	@inline final private[specnaz] def subseq[@specialized(Reified) E] (array :Array[E], offset :Int, length :Int) = using(array, offset, length)

	protected def using[@specialized(Reified) E](array :Array[E], offset :Int, length :Int) :S[E]




	override def newBuilder[@specialized(Reified) E]: SpecBuilder[E, S[E]] =
		new SharedArrayBuilder[E](Specialized.erasedArray[E])

	def dedicatedBuilder[@specialized(Reified) E :ClassTag] = new SharedArrayBuilder[E]

	def specBuilder[E](implicit specialization :Specialized[E]) :SpecBuilder[E, S[E]] =
		Builders[E](specialization.classTag.asInstanceOf[ClassTag[E]])



	def as[E](implicit elementType :ClassTag[E]) :SpecBuilder[E, S[E]] = Builders(elementType)

	def asClass[E](elementType :Class[E]) :SpecBuilder[E, S[E]] = Builders(ClassTag(elementType))


	protected[this] type DefaultBuilder[E] = SpecBuilder[E, S[E]]

	private[this] final val Builders = new Specialize.With[DefaultBuilder, ClassTag] {
		override def specialized[@specialized E: Specialized](elementType :ClassTag[E]): SpecBuilder[E, S[E]] =
			dedicatedBuilder[E](elementType)
	}



	implicit def CanBuildDedicated[E <: AnyRef :ClassTag] :SpecCanBuildFrom[S[_], E, S[E]] =
		new CanBuildArray[E]


	protected class CanBuildArray[@specialized E :ClassTag] extends SpecCanBuildFrom[S[_], E, S[E]] {
		override def elementType = classTag[E].runtimeClass

		override def apply(from: S[_]): SpecBuilder[E, S[E]] = from.dedicatedBuilder[E]

		override def apply(): SpecBuilder[E, S[E]] = dedicatedBuilder[E]

		override def canEqual(that :Any) = that.isInstanceOf[CanBuildArray[_]]

		override def equals(that :Any) = that match {
			case cbf :CanBuildArray[_] => (cbf eq this) || elementType==cbf.elementType && super.equals(cbf)
			case _ => false
		}

		override def toString = s"$outer.CBF[$classTag]"
	}



	protected class SharedArrayBuilder[@specialized(Reified) E](buffer :Array[E])
		extends SpecBuilder[E, S[E]] with AbstractResizableArrayBuffer[E]
	{

		def this()(implicit elementType :ClassTag[E]) = this(new Array[E](0))

		array = buffer

		override def sizeHint(size: Int): Unit = reserve(size)

		override def result(): S[E] = {
			val res = outer.using(array, offset, length)
			unmodifiable = true
			res
		}


	}


}





object SharedArray extends SharedArrayFactory[SharedArray] {
	@inline def Acc[E :Specialized] :SharedArray[E] = GrowableArray.Acc[E]

	override protected def using[@specialized(Reified) E](array: Array[E], offset: Int, length: Int): SharedArray[E] =
		ArraySlice.subseq(array, offset, length)


}




