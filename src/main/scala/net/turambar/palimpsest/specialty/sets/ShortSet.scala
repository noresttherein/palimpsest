package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.FitIterable.{IterableMapping}
import net.turambar.palimpsest.specialty.FitIterator.{BaseIterator, MappedIterator}
import net.turambar.palimpsest.specialty.Specialized.{Fun1Res, Fun1Vals, Fun2, Fun2Vals}
import net.turambar.palimpsest.specialty.sets.FitSet.Sorted
import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator, forceFit}


/**
  * @author Marcin Mościcki
  */
//trait ShortSet extends FitSet[Short] {
//	override def newBuilder :FitBuilder[Short, ShortSet] = ShortSet.newBuilder
//	override def typeStringPrefix = "ShortSet"
//}



object ShortSet {


	private[this] final val ShortToInt = (_:Short).toInt
	private[this] final val IntToShort = (_:Int).toShort

//	final val Empty :FitSet[Short] = new EmptyIterable[Short, FitSet[Short]] with FitSet[Short] {
//		override def contains(elem: Short): Boolean = false
//		override def +(elem: Short): FitSet[Short] = Singleton(elem)
//		override def -(elem: Short): FitSet[Short] = this
//	}
	final val Empty = new IntSet.ViewAs[Short](IntToShort, ShortToInt)(IntSet.Empty)

	def empty :FitSet[Short] = Empty

	def newBuilder :FitBuilder[Short, FitSet[Short]] =
		IntSet.newBuilder.mapInput(ShortToInt).mapResult(ints => new IntSet.ViewAs[Short](IntToShort, ShortToInt)(ints))

	def Singleton(value :Short) :FitSet[Short] = new IntSet.ViewAs[Short](IntToShort, ShortToInt)(IntSet.Singleton(value.toInt))

	type Sorted = FitSet.Sorted[Short]

	object Sorted {
		final val Empty :SortedFitSet[Short] = new IntSet.SortedViewAs[Short](IntToShort, ShortToInt)(IntSet.Sorted.Empty)
		def newBuilder :FitBuilder[Short, SortedFitSet[Short]] = IntSet.Sorted.newBuilder.mapInput(ShortToInt).mapResult(
			ints => new IntSet.SortedViewAs[Short](IntToShort, ShortToInt)(ints)
		)
		def Singleton(value :Short) :SortedFitSet[Short] = new IntSet.SortedViewAs[Short](IntToShort, ShortToInt)(IntSet.Sorted.Singleton(value.toInt))
	}

/*

	private[sets] abstract class AsInts[Repr <: FitSet[Short] with SetSpecialization[Short, Repr]]
		extends IterableMapping[Int, FitSet[Int], Short, Repr] with FitSet[Short] //with SetSpecialization[Short, Repr]
	{ this :Repr =>
		@inline override final protected def forSource[@specialized(Fun1Res) O](f :Short=>O) = { i :Int => f(i.toShort) }
		@inline override final protected def my(x: Int): Short = x.toShort
		@inline override final protected def from = IntToShort

		override def head :Short = source.head.toShort
		override def last :Short = source.last.toShort

		override def contains(elem: Short): Boolean = source.contains(elem & 0xffff)


		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, Short) => O) =
			source.foldLeft(z)( (o :O, i :Int) => op(o, i.toShort))

		override def foldRight[@specialized(Fun2) O](z :O)(op :(Short, O)=>O) =
			source.foldRight(z)( (i :Int, o :O) => op(i.toShort, o) )


		override def +(elem: Short): Repr = fromSource(source + (elem & 0xffff))

		override def -(elem: Short): Repr = fromSource(source - (elem & 0xffff))

		override def fitIterator: FitIterator[Short] = new IntAsShortIterator(source.iterator)

		override def newBuilder =
			source.newBuilder.mapInput(ShortToInt).mapResult(fromSource)
	}

	private class AsIntSet(protected val source :FitSet[Int]) extends AsInts[FitSet[Short]] {
		override protected[this] def fromSource(col: FitSet[Int]): FitSet[Short] = new AsIntSet(col)
	}

	private class AsIntSorted(override val source :FitSet.Sorted[Int])
		extends AsInts[FitSet.Sorted[Short]] with FitSet.MakeSorted[Short]
	{
		override implicit def ordering: Ordering[Short] = Ordering.Short

		override protected[this] def fromSource(col: FitSet[Int]): FitSet.Sorted[Short] =
			fromSorted(col.asInstanceOf[FitSet.Sorted[Int]])

		@inline final private[this] def fromSorted(col :FitSet.Sorted[Int]) :FitSet.Sorted[Short] =
			new AsIntSorted(col)

		override def rangeImpl(from: Option[Short], until: Option[Short]): SortedFitSet[Short] = fromSorted(
			(from, until) match {
				case (Some(f), Some(t)) => source.rangeImpl(Some(f & 0xffff), Some(t & 0xffff))
				case (Some(f), _) => source.rangeImpl(Some(f & 0xffff), None)
				case (_, Some(t)) => source.rangeImpl(None, Some(t & 0xffff))
				case _ => source.rangeImpl(None, None)
			}
		)

		override def keysIteratorFrom(start: Short): FitIterator[Short] =
			new IntAsShortIterator(source.keysIteratorFrom(start & 0xffff))
	}

	private[sets] class IntAsShortIterator(i :FitIterator[Int]) extends MappedIterator[Int, Short]((_:Int).toShort)(i) {
		override def head = source.head.toShort
		override def next() = source.next().toShort
	}
*/

}
