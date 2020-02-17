package net.noresttherein.palimpsest.sets

import java.lang.Float.{floatToIntBits, intBitsToFloat}

import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.ordered.ValOrdering
import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.maps.AVLTree
import net.noresttherein.palimpsest.maps.AVLTree.EntryLens
import net.noresttherein.palimpsest.ordered.ValOrdering


/**
  * @author Marcin Mościcki
  */
private[sets] class StableFloatTreeSet(protected val root :AVLTree[Int, Unit], elems :Int = -1)
	                                  (implicit override val ordering :ValOrdering[Float])
	extends AbstractStableTreeSet[Float](elems) with StableTreeSet[Float] with EntryLens[Int, Unit, Float]
{
	protected type Key = Int
	protected override def lens :EntryLens[Int, Unit, Float] = this

	private implicit val keyOrdering :ValOrdering[Int] = (x :Int, y :Int) => ordering.compare(x.toFloat, y.toFloat)

	override def element(entry :AVLTree.Entry[Int, Unit]) :Float = intBitsToFloat(entry.key)

	override def empty :StableFloatTreeSet = new StableFloatTreeSet(null, 0)


	override def +(elem :Float) :StableFloatTreeSet = {
		val tree = root
		val key = floatToIntBits(elem)
		if (tree == null)
			new StableFloatTreeSet(AVLTree.IntSet(key), 1)
		else {
			val res = tree.insertRaw(key, ())(keyOrdering)
			if (res eq tree) this
			else new StableFloatTreeSet(res, if (hasFastSize) size + 1 else -1)
		}
	}

	override def -(elem :Float) :StableFloatTreeSet = {
		val tree = root
		if (tree == null)
			this
		else {
			val res = tree.deleteRaw(floatToIntBits(elem))(keyOrdering)
			if (res eq tree) this
			else new StableFloatTreeSet(res, if (hasFastSize) size - 1 else -1)
		}
	}

	override def contains(key :Float) :Boolean = {
		val tree = root
		if (tree == null) false
		else tree.entryFor(floatToIntBits(key))(keyOrdering) != null
	}



	override def keysIteratorFrom(start :Float) :AptIterator[Float] = {
		val tree = root
		if (tree == null) AptIterator.Empty
		else tree.iteratorFrom(this)(floatToIntBits(start))(keyOrdering)
	}

	protected override def debugString = "StableFloatTreeSet"
}



