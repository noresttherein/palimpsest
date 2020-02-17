package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.maps.AVLTree
import net.turambar.palimpsest.specialty.maps.AVLTree.EntryLens
import net.turambar.palimpsest.specialty.ordered.ValOrdering


/**
  * @author Marcin Mościcki
  */
private[sets] class StableByteTreeSet(protected val root :AVLTree[Int, Unit], elems :Int = -1)
	                                 (implicit override val ordering :ValOrdering[Byte])
	extends AbstractStableTreeSet[Byte](elems) with StableTreeSet[Byte] with EntryLens[Int, Unit, Byte]
{
	protected type Key = Int
	protected override def lens :EntryLens[Int, Unit, Byte] = this

	private implicit val keyOrdering :ValOrdering[Int] = (x :Int, y :Int) => ordering.compare(x.toByte, y.toByte)

	override def element(entry :AVLTree.Entry[Int, Unit]) :Byte = entry.key.toByte

	override def empty :StableByteTreeSet = new StableByteTreeSet(null, 0)



	override def +(elem :Byte) :StableByteTreeSet = {
		val tree = root
		val key = elem.toInt
		if (tree == null)
			new StableByteTreeSet(AVLTree.IntSet(key), 1)
		else {
			val res = tree.insertRaw(key, ())(keyOrdering)
			if (res eq tree) this
			else new StableByteTreeSet(res, if (hasFastSize) size + 1 else -1)
		}
	}

	override def -(elem :Byte) :StableByteTreeSet = {
		val tree = root
		if (tree == null)
			this
		else {
			val res = tree.deleteRaw(elem.toInt)(keyOrdering)
			if (res eq tree) this
			else new StableByteTreeSet(res, if (hasFastSize) size - 1 else -1)
		}
	}

	override def contains(elem :Byte) :Boolean = {
		val tree = root
		if (tree == null) false
		else tree.entryFor(elem.toInt)(keyOrdering) != null
	}



	override def keysIteratorFrom(start :Byte) :AptIterator[Byte] = {
		val tree = root
		if (tree == null) AptIterator.Empty
		else tree.iteratorFrom(this)(start.toInt)(keyOrdering)
	}

	protected override def debugString = "StableByteTreeSet"
}



