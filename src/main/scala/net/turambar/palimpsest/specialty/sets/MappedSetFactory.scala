package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitIterator, TypedIterableFactory}
import net.turambar.palimpsest.specialty.FitIterable.{IterableMapping}
import net.turambar.palimpsest.specialty.FitIterator.MappedIterator
import net.turambar.palimpsest.specialty.iterables.EmptyIterable
import net.turambar.palimpsest.specialty.Specialized.{Fun1Res, Fun2}

/**
  * @author Marcin Mościcki
  */
abstract class MappedSetFactory[@specialized(Int, Long) X, @specialized(Byte, Short, Char, Float, Double) Y](impl :TypedIterableFactory[X, FitSet[X]], From :X=>Y, To :Y=>X)(implicit order :Ordering[Y])
	extends TypedIterableFactory[Y, FitSet[Y]]
{
	type Sorted = SortedFitSet[Y]

	final val Empty :FitSet[Y] = new EmptyIterable[Y, FitSet[Y]] with FitSet[Y] {
		override def contains(elem: Y): Boolean = false
		override def +(elem: Y): FitSet[Y] = Singleton(elem)
		override def -(elem: Y): FitSet[Y] = this
	}

	def empty = Empty

	def newBuilder :FitBuilder[Y, FitSet[Y]] =
		impl.newBuilder.mapInput(To).mapResult(ints => new MappedSet(ints))

	override def Singleton(value :Y) :FitSet[Y] = new MappedSet(impl.Singleton(To(value)))

	private[sets] abstract class AbstractMappedSet[Repr <: FitSet[Y] with SetSpecialization[Y, Repr]]
		extends IterableMapping[X, FitSet[X], Y, Repr] with FitSet[Y] //with SetSpecialization[Short, Repr]
	{ this :Repr =>
		@inline override final protected def forSource[@specialized(Fun1Res) O](f :Y=>O) = { x :X => f(From(x)) }
		@inline override final protected def my(x: X): Y = From(x)
		@inline override final protected def from = From

		override def head :Y = From(source.head)
		override def last :Y = From(source.last)

		override def contains(elem: Y): Boolean = source.contains(To(elem))


		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, Y) => O) =
			source.foldLeft(z)( (o :O, x :X) => op(o, From(x)))

		override def foldRight[@specialized(Fun2) O](z :O)(op :(Y, O)=>O) =
			source.foldRight(z)( (x :X, o :O) => op(From(x), o) )


		override def +(elem: Y): Repr = fromSource(source + To(elem))

		override def -(elem: Y): Repr = fromSource(source - To(elem))

		override def fitIterator: FitIterator[Y] = new MappedIterator(From)(source.iterator)

		override def newBuilder =
			source.newBuilder.mapInput(To).mapResult(fromSource)
	}



	private class MappedSet(protected val source :FitSet[X]) extends AbstractMappedSet[FitSet[Y]] {
		override protected[this] def fromSource(col: FitSet[X]): FitSet[Y] = new MappedSet(col)
	}

	private class MappedSortedSet(override val source :FitSet.Sorted[X])
		extends AbstractMappedSet[FitSet.Sorted[Y]] with FitSet.MakeSorted[Y]
	{
		override implicit def ordering: Ordering[Y] = order

		override protected[this] def fromSource(col: FitSet[X]): FitSet.Sorted[Y] =
			fromSorted(col.asInstanceOf[FitSet.Sorted[X]])

		@inline final private[this] def fromSorted(col :FitSet.Sorted[X]) :FitSet.Sorted[Y] =
			new MappedSortedSet(col)

		override def rangeImpl(from: Option[Y], until: Option[Y]): SortedFitSet[Y] = fromSorted(
			(from, until) match {
				case (Some(f), Some(t)) => source.rangeImpl(Some(To(f)), Some(To(t)))
				case (Some(f), _) => source.rangeImpl(Some(To(f)), None)
				case (_, Some(t)) => source.rangeImpl(None, Some(To(t)))
				case _ => source.rangeImpl(None, None)
			}
		)

		override def keysIteratorFrom(start: Y): FitIterator[Y] =
			new MappedIterator(From)(source.keysIteratorFrom(To(start)))
	}



}
