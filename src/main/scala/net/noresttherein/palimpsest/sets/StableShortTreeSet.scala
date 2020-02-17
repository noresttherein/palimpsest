package net.noresttherein.palimpsest.sets

import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.maps.AVLTree
import net.noresttherein.palimpsest.maps.AVLTree.EntryLens
import net.noresttherein.palimpsest.ordered.ValOrdering


/**
  * @author Marcin Mościcki
  */
private[sets] class StableShortTreeSet(protected val root :AVLTree[Int, Unit], elems :Int = -1)
	                                 (implicit override val ordering :ValOrdering[Short])
	extends AbstractStableTreeSet[Short](elems) with StableTreeSet[Short] with EntryLens[Int, Unit, Short]
{
	protected type Key = Int
	protected override def lens :EntryLens[Int, Unit, Short] = this

	private implicit val keyOrdering :ValOrdering[Int] = (x :Int, y :Int) => ordering.compare(x.toShort, y.toShort)

	override def element(entry :AVLTree.Entry[Int, Unit]) :Short = entry.key.toShort

	override def empty :StableShortTreeSet = new StableShortTreeSet(null, 0)
	
	

	override def +(elem :Short) :StableShortTreeSet = {
		val tree = root
		val key = elem.toInt
		if (tree == null)
			new StableShortTreeSet(AVLTree.IntSet(key), 1)
		else {
			val res = tree.insertRaw(key, ())(keyOrdering)
			if (res eq tree) this
			else new StableShortTreeSet(res, if (hasFastSize) size + 1 else -1)
		}
	}

	override def -(elem :Short) :StableShortTreeSet = {
		val tree = root
		if (tree == null)
			this
		else {
			val res = tree.deleteRaw(elem.toInt)(keyOrdering)
			if (res eq tree) this
			else new StableShortTreeSet(res, if (hasFastSize) size - 1 else -1)
		}
	}

	override def contains(elem :Short) :Boolean = {
		val tree = root
		if (tree == null) false
		else tree.entryFor(elem.toInt)(keyOrdering) != null
	}



	override def keysIteratorFrom(start :Short) :AptIterator[Short] = {
		val tree = root
		if (tree == null) AptIterator.Empty
		else tree.iteratorFrom(this)(start.toInt)(keyOrdering)
	}

	protected override def debugString = "StableShortTreeSet"
}



