package net.turambar.palimpsest.specialty.sets

import scala.annotation.unspecialized
import scala.collection.generic.CanBuildFrom

import net.turambar.palimpsest.specialty.{?, FitBuilder, ItemTypes, RuntimeType, Specialize}
import net.turambar.palimpsest.specialty.iterables.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.maps.RedBlackTree
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.Fun2
import net.turambar.palimpsest.specialty.Specialize.SpecializeSome
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.maps.RedBlackTree.EntryLens
import net.turambar.palimpsest.specialty.sets.MutableTreeSet.MutableTreeSetRange
import net.turambar.palimpsest.specialty.sets.OrderedSet.OrderedSetRangeSpecialization



/**
  * @author Marcin Mościcki
  */
trait MutableTreeSet[@specialized(ItemTypes) E]
	extends MutableOrderedSet[E] with MutableSetSpecialization[E, MutableTreeSet[E]]
	   with SpecializableOrderedSet[E, MutableTreeSet]
{
	protected type Key
	protected def root :RedBlackTree[Key, Unit]
	protected def lens :EntryLens[Key, Unit, E]

	override def factory :OrderedSetFactory[MutableTreeSet] = MutableTreeSet




//	override def head :E = root.min(lens)
//
//	override def last :E = root.max(lens)



	override def find_?(p :E => Boolean, where :Boolean): ?[E] = root.find_?(lens)(p, where)

	override def count(p :E => Boolean) :Int = root.count(lens)(p)

	override def foldLeft[@specialized(Fun2) O](z :O)(op :(O, E) => O) :O = root.foldLeft(lens)(z)(op)

	override def foldRight[@specialized(Fun2) O](z :O)(op :(E, O) => O) :O = root.foldRight(lens)(z)(op)


	override def foreach[@specialized(Unit) U](f :E => U) :Unit = root.foreach(lens)(f.asInstanceOf[E => Unit])

	protected override def reverseForeach(f :E => Unit) :Unit = root.reverseForeach(lens)(f)


	protected override def trustedCopyTo(xs :Array[E], start :Int, total :Int) :Int =
		root.copyToArray(lens)(xs, start, total)


	override def iterator :FitIterator[E] = root.iterator(lens)

	override def reverseIterator :FitIterator[E] = root.reverseIterator(lens)


	override def rangeImpl(from: ?[E], until: ?[E]) :MutableTreeSet[E] =
		if (from.isEmpty && until.isEmpty) this
		else new MutableTreeSetRange[E](this, from, until)



	override def typeStringPrefix = "TreeSet"

}






object MutableTreeSet extends OrderedSetFactory[MutableTreeSet] {

		@inline final implicit def canBuildFrom[E](implicit fit :CanFitFrom[MutableTreeSet[_], E, MutableTreeSet[E]])
				:CanBuildFrom[MutableTreeSet[_], E, MutableTreeSet[E]] =
			fit.cbf

		override def empty[@specialized(ItemTypes) E](implicit ordering :ValOrdering[E]) :MutableTreeSet[E] =
			Empty()(RuntimeType.specialized[E])(ordering)

		override def newBuilder[@specialized(ItemTypes) E :ValOrdering] :FitBuilder[E, MutableTreeSet[E]] = empty[E]


		private type Constructor[X] = ValOrdering[X] => MutableTreeSet[X]

		private[this] final val Empty :Specialize.Individually[Constructor] = new SpecializeSome[Constructor] {
			override val forByte = { ord :ValOrdering[Byte] => new MutableByteTreeSet()(ord) }
			override val forShort = { ord :ValOrdering[Short] => new MutableShortTreeSet()(ord) }
			override val forChar = { ord :ValOrdering[Char] => new MutableCharTreeSet()(ord) }
			override val forInt = { ord :ValOrdering[Int] => new MutableIntTreeSet()(ord) }
			override val forLong = { ord :ValOrdering[Long] => new MutableLongTreeSet()(ord) }
			override val forFloat = { ord :ValOrdering[Float] => new MutableFloatTreeSet()(ord) }
			override val forDouble = { ord :ValOrdering[Double] => new MutableDoubleTreeSet()(ord) }

			override protected[this] def generic[E :RuntimeType] = erased.asInstanceOf[Constructor[E]]

			private[this] final val erased = { ord :ValOrdering[Any] => new StableAVLSet[Any](null, 0)(ord) }
		}



		private[sets] class MutableTreeSetRange[@specialized(ItemTypes) E](
				protected override val source :MutableTreeSet[E],
				protected override val minKey: ?[E],
				protected override val maxKey: ?[E]
			) extends MutableTreeSet[E] with OrderedSetRangeSpecialization[E, MutableTreeSet[E]]
		{
			override protected type Key = source.Key
			override protected def root :RedBlackTree[Key, Unit] = null
			override protected def lens :EntryLens[Key, Unit, E] = source.lens



			override def +=(elem :E) :this.type = { source += elem; this }

			override def -=(elem :E) :this.type = { source += elem; this }


			override def head :E = iterator.next()

			override def last :E = reverseIterator.next()

			@unspecialized
			override def find_?(p :E => Boolean, where :Boolean) : ?[E] = iterator.find_?(p, where)

			@unspecialized
			override def count(p :E => Boolean) :Int = iterator.count(p)

			@unspecialized
			override def foldLeft[@specialized(Fun2) O](z :O)(op :(O, E) => O) :O = iterator.foldLeft(z)(op)

			@unspecialized
			override def foldRight[@specialized(Fun2) O](z :O)(op :(E, O) => O) :O = iterator.foldRight(z)(op)

			@unspecialized
			override def foreach[@specialized(Unit) U](f :E => U) :Unit = iterator.foreach(f.asInstanceOf[E => Unit])



			protected override def trustedCopyTo(xs :Array[E], start :Int, total :Int) :Int = {
				val iter = iterator
				var copied = 0
				while (copied < total && iter.hasNext) {
					xs(start + copied) = iter.next()
					copied += 1
				}
				copied
			}

		}


}


