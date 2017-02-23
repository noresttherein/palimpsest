package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator}
import net.turambar.palimpsest.specialty.FitIterable.IterableMapping
import net.turambar.palimpsest.specialty.FitIterator.MappedIterator
import net.turambar.palimpsest.specialty.Specialized.{Fun1Res, Fun2}


/**
  * @author Marcin Mościcki
  */
object DoubleSet {
	import java.lang.Double.{doubleToLongBits, longBitsToDouble}

	private[this] final val DoubleToLong :Double => Long = doubleToLongBits
	private[this] final val LongToDouble :Long=>Double = longBitsToDouble

	final val Empty :FitSet[Double] = new LongView(LongSet.Empty)
	
	def empty :FitSet[Double] = Empty
	
	def newBuilder :FitBuilder[Double, FitSet[Double]] =
		LongSet.newBuilder.mapInput(DoubleToLong).mapResult(ints => new LongView(ints))

	def Singleton(value :Double) :FitSet[Double] = new LongView(LongSet.Singleton(doubleToLongBits(value)))

//	type Sorted = FitSet.Sorted[Double]
//
//	object Sorted {
//		final val Empty :SortedFitSet[Double] = new LongSet.SortedViewAs[Double](LongToDouble, DoubleToLong)(LongSet.Sorted.Empty)
//		def newBuilder :FitBuilder[Double, SortedFitSet[Double]] = LongSet.Sorted.newBuilder.mapInput(DoubleToLong).mapResult(
//			ints => new LongSet.SortedViewAs[Double](LongToDouble, DoubleToLong)(ints)			
//		)
//		def Singleton(value :Double) :SortedFitSet[Double] = new LongSet.SortedViewAs[Double](LongToDouble, DoubleToLong)(LongSet.Sorted.Singleton(value))
//	}

	object Mutable {
		def empty :MutableSet[Double] = new MutableLongView(LongSet.Mutable.empty)
		def newBuilder :FitBuilder[Double, MutableSet[Double]] = new MutableLongView(LongSet.Mutable.empty)
		def Singleton(value :Double) :MutableSet[Double] = new MutableLongView(LongSet.Mutable.Singleton(doubleToLongBits(value)))
	}

	private trait DoubleAsLongSet[+S<:FitSet[Long] with SetSpecialization[Long, S], +Repr <: FitSet[Double] with SetSpecialization[Double, Repr]]
		extends IterableMapping[Long, S, Double, Repr] with FitSet[Double] with SetSpecialization[Double, Repr]
	{
		@inline override protected def forSource[@specialized(Fun1Res) O](f :Double=>O) = { x :Long => f(longBitsToDouble(x)) }

		override protected[this] def fromSource(col: S): Repr
		override protected[this] val source :S

		protected def my(x :Long) :Double = longBitsToDouble(x)
		@inline override final def from = LongToDouble

		override def empty :Repr = fromSource((source :SetSpecialization[Long, S]).empty)
		override def mutable :MutableSet[Double] = new MutableLongView(source.mutable)
		//		override def stable =

		override def head :Double = from(source.head)
		override def last :Double = from(source.last)

		override def contains(elem: Double): Boolean = source.contains(DoubleToLong(elem))


		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, Double) => O) =
			source.foldLeft(z)( (o :O, x :Long) => op(o, longBitsToDouble(x)))

		override def foldRight[@specialized(Fun2) O](z :O)(op :(Double, O)=>O) =
			source.foldRight(z)( (x :Long, o :O) => op(longBitsToDouble(x), o) )


		override def +(elem: Double): Repr = fromSource(source + doubleToLongBits(elem))

		override def -(elem: Double): Repr = fromSource(source - doubleToLongBits(elem))

		override def fitIterator: FitIterator[Double] = new MappedIterator(from)(source.iterator)

		override def newBuilder =
			source.newBuilder.mapInput(DoubleToLong).mapResult(fromSource)

	}



	private trait DoubleAsLongMutableSet[+S<:MutableSet[Long] with SetSpecialization[Long, S], +Repr<:MutableSet[Double] with SetSpecialization[Double, Repr]]
		extends IterableMapping[Long, S, Double, Repr] with MutableSet[Double] with DoubleAsLongSet[S, Repr]
	{
		override def stable :FitSet.Stable[Double] = new LongView(source.stable)

		override def add(elem: Double) = source.add(doubleToLongBits(elem))
		override def remove(elem: Double) = source.remove(doubleToLongBits(elem))

		override def +=(elem: Double): this.type = { source -= doubleToLongBits(elem); this }
		override def -=(elem: Double): this.type = { source -= doubleToLongBits(elem); this }

		override def +=(elem1: Double, elem2: Double, elems: Double*) :this.type = {
			source += (doubleToLongBits(elem1), doubleToLongBits(elem2), elems.map(DoubleToLong):_*); this
		}
		override def -=(elem1: Double, elem2: Double, elems: Double*) = {
			source.-=(doubleToLongBits(elem1), doubleToLongBits(elem2), elems.map(DoubleToLong):_*); this
		}

		override def sizeHint(expect: Int) = source.sizeHint(expect)

		override def count = size


	}



	private class LongView(protected val source :FitSet[Long])
		extends DoubleAsLongSet[FitSet[Long], FitSet[Double]]
	{
		override protected[this] def fromSource(col: FitSet[Long]): FitSet[Double] = new LongView(col)
	}


	private class MutableLongView(protected val source :MutableSet[Long])
		extends DoubleAsLongMutableSet[MutableSet[Long], MutableSet[Double]]
	{
		override protected[this] def fromSource(col: MutableSet[Long]): MutableSet[Double] = new MutableLongView(col)
	}


}
