package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.maps.AVLTree
import net.turambar.palimpsest.specialty.maps.AVLTree.EntryLens
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.sets

/**
  * @author Marcin Mościcki
  */
private[sets] class StableIntTreeSet(protected val root :AVLTree[Int, Unit], elems :Int = -1)
	                                (implicit override val ordering :ValOrdering[Int])
	extends AbstractStableTreeSet[Int](elems) with StableTreeSet[Int] with EntryLens[Int, Unit, Int]
{
	protected type Key = Int
	protected override def lens :EntryLens[Int, Unit, Int] = this

	override def element(entry :AVLTree.Entry[Int, Unit]) :Int = entry.key

	override def empty :StableIntTreeSet = new StableIntTreeSet(null, 0)



	override def +(elem :Int) :StableIntTreeSet = {
		val tree = root
		if (tree == null)
			new StableIntTreeSet(AVLTree.IntSet(elem), 1)
		else {
			val res = tree.insertRaw(elem, ())
			if (res eq tree) this
			else new StableIntTreeSet(res, if (hasFastSize) size + 1 else -1)
		}
	}

	override def -(elem :Int) :StableIntTreeSet = {
		val tree = root
		if (tree == null)
			this
		else {
			val res = tree.deleteRaw(elem)
			if (res eq tree) this
			else new StableIntTreeSet(res, if (hasFastSize) size - 1 else -1)
		}
	}

	override def contains(key :Int) :Boolean = {
		val tree = root
		if (tree == null) false
		else tree.entryFor(key) != null
	}



	override def keysIteratorFrom(start :Int) :FitIterator[Int] = {
		val tree = root
		if (tree == null) FitIterator.Empty
		else tree.iteratorFrom(this)(start)
	}

	protected override def debugString = "StableIntTreeSet"
}



