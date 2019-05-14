package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.{?, Blank, Elements, FitBuilder, FitIterator, Sure}
import net.turambar.palimpsest.specialty.FitIterator.MappedIterator
import net.turambar.palimpsest.specialty.iterables.EmptyIterableFoundation
import net.turambar.palimpsest.specialty.RuntimeType.{Fun1Res, Fun2}
import net.turambar.palimpsest.specialty.sets.ValSet.{Mutable, Stable}
/*
/**
  * @author Marcin Mościcki
  */
abstract class MappedSetFactory[@specialized(Int, Long) X, @specialized(Byte, Short, Char, Float, Double) Y]
		(impl :TypedIterableFactory[X, ValSet[X]], From :X=>Y, To :Y=>X)(implicit order :Ordering[Y])
	extends TypedIterableFactory[Y, ValSet[Y]]
{
	type Sorted = OrderedSet[Y]

	final val Empty :StableSet[Y] = new EmptyIterableFoundation[Y, StableSet[Y]] with StableSet[Y] with EmptySetSpecialization[Y, StableSet[Y]] {
		override def contains(elem: Y): Boolean = false
		override def +(elem: Y): StableSet[Y] = Singleton(elem)
		override def -(elem: Y): StableSet[Y] = this

		override def mutable: Mutable[Y] = MutableSet.from(this)
	}

	def empty = Empty

	def newBuilder :FitBuilder[Y, StableSet[Y]] =
		impl.newBuilder.mapInput(To).mapResult(ints => new MappedSet(ints))

	override def Singleton(value :Y) :StableSet[Y] = new MappedSet(impl.Singleton(To(value)))




	private[sets] abstract class AbstractMappedSet[Repr <: ValSet[Y] with SetSpecialization[Y, Repr]]
		extends IterableMapping[X, ValSet[X], Y, Repr] with SetTemplate[Y, Repr] //ValSet[Y] with SetSpecialization[Y, Repr]
	{ this :Repr =>
		@inline override final protected def forSource[@specialized(Fun1Res) O](f :Y=>O) = { x :X => f(From(x)) }
		@inline override final protected def adapt(x: X): Y = From(x)
		@inline override final protected def from = From

		override def empty :Repr = fromSource(source.empty)

		override def head :Y = From(source.head)
		override def last :Y = From(source.last)

		override def contains(elem: Y): Boolean = source.contains(To(elem))


		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, Y) => O) =
			source.foldLeft(z)( (o :O, x :X) => op(o, From(x)))

		override def foldRight[@specialized(Fun2) O](z :O)(op :(Y, O)=>O) =
			source.foldRight(z)( (x :X, o :O) => op(From(x), o) )


		override def +(elem: Y): Repr = fromSource(source + To(elem))

		override def -(elem: Y): Repr = fromSource(source - To(elem))

		override def iterator: FitIterator[Y] = new MappedIterator(From)(source.iterator)

		override def newBuilder =
			source.newBuilder.mapInput(To).mapResult(fromSource)

		override def mutable: Mutable[Y] = MutableSet.from(this)
		override def stable: Stable[Y] = StableSet.empty[Y] ++ this
	}



	private class MappedSet(protected val source :ValSet[X]) extends AbstractMappedSet[StableSet[Y]] with StableSet[Y] {
		override protected[this] def fromSource(col: ValSet[X]): StableSet[Y] = new MappedSet(col)
	}

	private class MappedSortedSet(override val source :ValSet.Sorted[X])
		extends AbstractMappedSet[StableOrderedSet[Y]] with StableOrderedSet[Y]
	{
		override implicit def ordering: Ordering[Y] = order

		override protected[this] def fromSource(col: ValSet[X]): StableOrderedSet[Y] =
			fromSorted(col.asInstanceOf[OrderedSet[X]])

		@inline final private[this] def fromSorted(col :OrderedSet[X]) :StableOrderedSet[Y] =
			new MappedSortedSet(col)

		override def rangeImpl(from: ?[Y], until: ?[Y]): StableOrderedSet[Y] = fromSorted(
			(from, until) match {
				case (f :Sure[Y], t :Sure[Y]) => source.rangeImpl(Sure(To(f.value)), Sure(To(t.value)))
				case (f :Sure[Y], _) => source.rangeImpl(Sure(To(f.value)), Blank)
				case (_, t :Sure[Y]) => source.rangeImpl(Blank, Sure(To(t.value)))
				case _ => source.rangeImpl(Blank, Blank)
			}
		)

		override def keysIteratorFrom(start: Y): FitIterator[Y] =
			new MappedIterator(From)(source.keysIteratorFrom(To(start)))
	}



}
*/