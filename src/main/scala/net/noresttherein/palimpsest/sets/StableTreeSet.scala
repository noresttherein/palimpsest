package net.noresttherein.palimpsest.sets

import scala.annotation.unspecialized
import scala.collection.generic.CanBuildFrom
import net.noresttherein.palimpsest.{?, Blank, ItemTypes, RuntimeType, Specialize}
import net.noresttherein.palimpsest.iterables.AptCompanion.CanFitFrom
import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.maps.AVLTree
import net.noresttherein.palimpsest.maps.AVLTree.EntryLens
import net.noresttherein.palimpsest.ordered.ValOrdering
import net.noresttherein.palimpsest.RuntimeType.Specialized.Fun2
import net.noresttherein.palimpsest.iterables.{AptIterable, IterableFoundation, IterableOverrides, StableIterableTemplate}
import net.noresttherein.palimpsest.sets.OrderedSet.OrderedSetRangeSpecialization
import net.noresttherein.palimpsest.sets.StableTreeSet.StableTreeSetRange
import net.noresttherein.palimpsest.Specialize.SpecializeSome
import net.noresttherein.palimpsest.LibraryError

/**
  * @author Marcin Mościcki
  */
trait StableTreeSet[@specialized(ItemTypes) E]
	extends StableOrderedSet[E] with SpecializableOrderedSet[E, StableTreeSet] with StableIterableTemplate[E, StableTreeSet[E]]
{
	protected type Key
	protected def root :AVLTree[Key, Unit]
	protected def lens :EntryLens[Key, Unit, E]

	override def factory :OrderedSetFactory[StableTreeSet] = StableTreeSet



	override def head :E = {
		val tree = root
		if (tree == null)
			throw new NoSuchElementException(this + ".head")
		else lens.element(tree.minEntry)
	}

	override def last :E = {
		val tree = root
		if (tree == null)
			throw new NoSuchElementException(this + ".last")
		else lens.element(tree.maxEntry)
	}



	override def find_?(p :E => Boolean, where :Boolean) : ?[E] = {
		val tree = root
		if (tree == null) Blank
		else tree.find_?(lens)(p, where)
	}

	override def count(p :E => Boolean): Int = {
		val tree = root
		if (tree == null) 0
		else tree.count(lens)(p)
	}


	override def foldLeft[@specialized(Fun2) O](z :O)(op :(O, E) => O) :O = {
		val tree = root
		if (tree == null) z
		else tree.foldLeft(lens)(z)(op)
	}

	override def foldRight[@specialized(Fun2) O](z :O)(op :(E, O) => O) :O = {
		val tree = root
		if (tree == null) z
		else tree.foldRight(lens)(z)(op)
	}


	override def foreach[@specialized(Unit) U](f :E => U) :Unit = {
		val tree = root
		if (tree != null)
			tree.foreach(lens)(f.asInstanceOf[E => Unit])
	}

	override protected def reverseForeach(f :E => Unit) :Unit = {
		val tree = root
		if (tree != null)
			tree.foreach(lens)(f)
	}



	protected override def trustedCopyTo(xs :Array[E], start :Int, total :Int) :Int = {
		val tree = root
		if (tree == null) 0
		else tree.copyToArray(lens)(xs, start, total)
	}




	override def iterator :AptIterator[E] = {
		val tree = root
		if (tree == null) AptIterator.Empty
		else tree.iterator(lens)
	}

	override def reverseIterator :AptIterator[E] = {
		val tree = root
		if (tree == null) AptIterator.Empty
		else tree.reverseIterator(lens)
	}


	override def rangeImpl(from: ?[E], until: ?[E]) :StableTreeSet[E] =
		if (from.isEmpty && until.isEmpty) this
		else new StableTreeSetRange[E](this, from, until)


//	override def keysIteratorFrom(start :E) :FitIterator[E] = tree.iteratorFrom(lens)(start)

	override def typeStringPrefix :String = "TreeSet"
}





private[sets] abstract class AbstractStableTreeSet[E](keyCount :Int = -1) extends IterableFoundation[E, StableTreeSet[E]] {
	this :StableTreeSet[E] =>

	@volatile
	private[this] var _size = keyCount

	override def hasFastSize :Boolean = _size >= 0

	override def size :Int = {
		var res = _size
		if (res < 0) {
			val tree = root
			if (tree == null) res = 0
			else res = tree.count
			_size = res
		}
		res
	}

}






object StableTreeSet extends OrderedSetFactoryImplicits[StableTreeSet] {

	@inline final implicit def canBuildFrom[E](implicit fit :CanFitFrom[StableTreeSet[_], E, StableTreeSet[E]])
			:CanBuildFrom[StableTreeSet[_], E, StableTreeSet[E]] =
		fit.cbf

	override def empty[@specialized(ItemTypes) E](implicit ordering :ValOrdering[E]) :StableTreeSet[E] =
		Empty()(RuntimeType.specialized[E])(ordering)

	private type Constructor[X] = ValOrdering[X] => StableTreeSet[X]
	
	private[this] final val Empty :Specialize.Individually[Constructor] = new SpecializeSome[Constructor] {
		override val forByte = { ord :ValOrdering[Byte] => new StableByteTreeSet(null, 0)(ord) }
		override val forShort = { ord :ValOrdering[Short] => new StableShortTreeSet(null, 0)(ord) }
		override val forChar = { ord :ValOrdering[Char] => new StableCharTreeSet(null, 0)(ord) }
		override val forInt = { ord :ValOrdering[Int] => new StableIntTreeSet(null, 0)(ord) }
		override val forLong = { ord :ValOrdering[Long] => new StableLongTreeSet(null, 0)(ord) }
		override val forFloat = { ord :ValOrdering[Float] => new StableFloatTreeSet(null, 0)(ord) }
		override val forDouble = { ord :ValOrdering[Double] => new StableDoubleTreeSet(null, 0)(ord) }

		override protected[this] def generic[E :RuntimeType] = erased.asInstanceOf[Constructor[E]]

		private[this] final val erased = { ord :ValOrdering[Any] => new StableErasedTreeSet[Any](null, 0)(ord) }
	}



	private[sets] class StableTreeSetRange[@specialized(ItemTypes) E](
			protected override val source :StableTreeSet[E],
			protected override val minKey: ?[E],
			protected override val maxKey: ?[E]
		) extends StableTreeSet[E] with OrderedSetRangeSpecialization[E, StableTreeSet[E]]
	         with IterableOverrides[E, StableTreeSet[E]]
	{
		override protected type Key = source.Key
		override protected def root :AVLTree[Key, Unit] =
			throw new LibraryError("StableTreeSetRange.root : this method should not be used!")

		override protected def lens :EntryLens[Key, Unit, E] = source.lens

	}

}
