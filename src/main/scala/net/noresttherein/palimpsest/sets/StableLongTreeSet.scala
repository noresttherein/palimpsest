package net.noresttherein.palimpsest.sets

import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.maps.AVLTree
import net.noresttherein.palimpsest.maps.AVLTree.EntryLens
import net.noresttherein.palimpsest.ordered.ValOrdering


/**
  * @author Marcin Mościcki
  */
private[sets] class StableLongTreeSet(protected val root :AVLTree[Long, Unit], elems :Int = -1)
	                                 (implicit override val ordering :ValOrdering[Long])
	extends AbstractStableTreeSet[Long](elems) with StableTreeSet[Long] with EntryLens[Long, Unit, Long]
{
	protected type Key = Long
	protected override def lens :EntryLens[Long, Unit, Long] = this

	override def element(entry :AVLTree.Entry[Long, Unit]) :Long = entry.key

	override def empty :StableLongTreeSet = new StableLongTreeSet(null, 0)



	override def +(elem :Long) :StableLongTreeSet = {
		val tree = root
		if (tree == null)
			new StableLongTreeSet(AVLTree.LongSet(elem), 1)
		else {
			val res = tree.insertRaw(elem, ())
			if (res eq tree) this
			else new StableLongTreeSet(res, if (hasFastSize) size + 1 else -1)
		}
	}

	override def -(elem :Long) :StableLongTreeSet = {
		val tree = root
		if (tree == null)
			this
		else {
			val res = tree.deleteRaw(elem)
			if (res eq tree) this
			else new StableLongTreeSet(res, if (hasFastSize) size - 1 else -1)
		}
	}

	override def contains(key :Long) :Boolean = {
		val tree = root
		if (tree == null) false
		else tree.entryFor(key) != null
	}



	override def keysIteratorFrom(start :Long) :AptIterator[Long] = {
		val tree = root
		if (tree == null) AptIterator.Empty
		else tree.iteratorFrom(this)(start)
	}

	protected override def debugString = "StableLongTreeSet"
}



