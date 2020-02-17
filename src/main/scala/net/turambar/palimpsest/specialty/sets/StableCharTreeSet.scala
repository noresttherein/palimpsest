package net.turambar.palimpsest.specialty.sets


import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.maps.AVLTree
import net.turambar.palimpsest.specialty.maps.AVLTree.EntryLens
import net.turambar.palimpsest.specialty.ordered.ValOrdering


/**
  * @author Marcin Mościcki
  */
private[sets] class StableCharTreeSet(protected val root :AVLTree[Int, Unit], elems :Int = -1)
	                                 (implicit override val ordering :ValOrdering[Char])
	extends AbstractStableTreeSet[Char](elems) with StableTreeSet[Char] with EntryLens[Int, Unit, Char]
{
	protected type Key = Int
	protected override def lens :EntryLens[Int, Unit, Char] = this

	private implicit val keyOrdering :ValOrdering[Int] = (x :Int, y :Int) => ordering.compare(x.toChar, y.toChar)

	override def element(entry :AVLTree.Entry[Int, Unit]) :Char = entry.key.toChar

	override def empty :StableCharTreeSet = new StableCharTreeSet(null, 0)


	
	override def +(elem :Char) :StableCharTreeSet = {
		val tree = root
		val key = elem.toInt
		if (tree == null)
			new StableCharTreeSet(AVLTree.IntSet(key), 1)
		else {
			val res = tree.insertRaw(key, ())(keyOrdering)
			if (res eq tree) this
			else new StableCharTreeSet(res, if (hasFastSize) size + 1 else -1)
		}
	}

	override def -(elem :Char) :StableCharTreeSet = {
		val tree = root
		if (tree == null)
			this
		else {
			val res = tree.deleteRaw(elem.toInt)(keyOrdering)
			if (res eq tree) this
			else new StableCharTreeSet(res, if (hasFastSize) size - 1 else -1)
		}
	}

	override def contains(elem :Char) :Boolean = {
		val tree = root
		if (tree == null) false
		else tree.entryFor(elem.toInt)(keyOrdering) != null
	}



	override def keysIteratorFrom(start :Char) :AptIterator[Char] = {
		val tree = root
		if (tree == null) AptIterator.Empty
		else tree.iteratorFrom(this)(start.toInt)(keyOrdering)
	}

	protected override def debugString = "StableCharTreeSet"
}



