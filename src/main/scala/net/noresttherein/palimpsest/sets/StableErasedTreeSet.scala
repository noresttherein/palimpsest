package net.noresttherein.palimpsest.sets

import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.maps.AVLTree
import net.noresttherein.palimpsest.maps.AVLTree.EntryLens
import net.noresttherein.palimpsest.ordered.ValOrdering


/**
  * @author Marcin Mościcki
  */
private[sets] class StableErasedTreeSet[E](protected val root :AVLTree[E, Unit], elems :Int = -1)
                                          (implicit override val ordering :ValOrdering[E])
	extends AbstractStableTreeSet[E](elems) with StableTreeSet[E] with EntryLens[E, Unit, E]
{
	protected type Key = E
	protected override def lens :EntryLens[E, Unit, E] = this

	override def element(entry :AVLTree.Entry[E, Unit]) :E = entry.key

	override def empty :StableErasedTreeSet[E] = new StableErasedTreeSet(null, 0)

	override def +(elem :E) :StableErasedTreeSet[E] = {
		val tree = root
		if (tree == null)
			new StableErasedTreeSet[E](AVLTree.Set(elem), 1)
		else {
			val res = tree.insertRaw(elem, ())
			if (res eq tree) this
			else new StableErasedTreeSet[E](res, if (hasFastSize) size + 1 else -1)
		}
	}

	override def -(elem :E) :StableErasedTreeSet[E] = {
		val tree = root
		if (tree == null)
			this
		else {
			val res = tree.deleteRaw(elem)
			if (res eq tree) this
			else new StableErasedTreeSet(res, if (hasFastSize) size - 1 else -1)
		}
	}

	override def contains(key :E) :Boolean = {
		val tree = root
		if (tree == null) false
		else tree.entryFor(key) != null
	}



	override def keysIteratorFrom(start :E) :AptIterator[E] = {
		val tree = root
		if (tree == null) AptIterator.Empty
		else tree.iteratorFrom(this)(start)
	}

	protected override def debugString = "StableErasedTreeSet"
}



