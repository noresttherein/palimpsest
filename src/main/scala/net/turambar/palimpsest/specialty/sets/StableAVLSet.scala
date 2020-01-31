package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.maps.AVLTree
import net.turambar.palimpsest.specialty.maps.AVLTree.EntryLens
import net.turambar.palimpsest.specialty.ordered.ValOrdering


/**
  * @author Marcin Mościcki
  */
private[sets] class StableAVLSet[E](protected val root :AVLTree[E, Unit], elems :Int = -1)
	                                (implicit override val ordering :ValOrdering[E])
	extends AbstractStableTreeSet[E](elems) with StableTreeSet[E] with EntryLens[E, Unit, E]
{
	protected type Key = E
	protected override def lens :EntryLens[E, Unit, E] = this

	override def element(entry :AVLTree.Entry[E, Unit]) :E = entry.key

	override def empty :StableAVLSet[E] = new StableAVLSet(null, 0)

	override def +(elem :E) :StableAVLSet[E] = {
		val tree = root
		if (tree == null)
			new StableAVLSet[E](AVLTree.Set(elem), 1)
		else {
			val res = tree.insertRaw(elem, ())
			if (res eq tree) this
			else new StableAVLSet[E](res, if (hasFastSize) size + 1 else -1)
		}
	}

	override def -(elem :E) :StableAVLSet[E] = {
		val tree = root
		if (tree == null)
			this
		else {
			val res = tree.deleteRaw(elem)
			if (res eq tree) this
			else new StableAVLSet(res, if (hasFastSize) size - 1 else -1)
		}
	}

	override def contains(key :E) :Boolean = {
		val tree = root
		if (tree == null) false
		else tree.entryFor(key) != null
	}



	override def keysIteratorFrom(start :E) :FitIterator[E] = {
		val tree = root
		if (tree == null) FitIterator.Empty
		else tree.iteratorFrom(this)(start)
	}

	protected override def debugString = "StableAVLSet"
}



