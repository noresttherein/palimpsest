package net.turambar.palimpsest.specialty.sets

import java.lang.Double.{doubleToLongBits, longBitsToDouble}

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.maps.AVLTree
import net.turambar.palimpsest.specialty.maps.AVLTree.EntryLens
import net.turambar.palimpsest.specialty.ordered.ValOrdering


/**
  * @author Marcin Mościcki
  */
private[sets] class StableDoubleTreeSet(protected val root :AVLTree[Long, Unit], elems :Int = -1)
	                                   (implicit override val ordering :ValOrdering[Double])
	extends AbstractStableTreeSet[Double](elems) with StableTreeSet[Double] with EntryLens[Long, Unit, Double]
{
	protected type Key = Long
	protected override def lens :EntryLens[Long, Unit, Double] = this

	private implicit val keyOrdering :ValOrdering[Long] = (x :Long, y :Long) => ordering.compare(x.toDouble, y.toDouble)

	override def element(entry :AVLTree.Entry[Long, Unit]) :Double = longBitsToDouble(entry.key)

	override def empty :StableDoubleTreeSet = new StableDoubleTreeSet(null, 0)
	


	override def +(elem :Double) :StableDoubleTreeSet = {
		val tree = root
		val key = doubleToLongBits(elem)
		if (tree == null)
			new StableDoubleTreeSet(AVLTree.LongSet(key), 1)
		else {
			val res = tree.insertRaw(key, ())(keyOrdering)
			if (res eq tree) this
			else new StableDoubleTreeSet(res, if (hasFastSize) size + 1 else -1)
		}
	}

	override def -(elem :Double) :StableDoubleTreeSet = {
		val tree = root
		if (tree == null)
			this
		else {
			val res = tree.deleteRaw(doubleToLongBits(elem))(keyOrdering)
			if (res eq tree) this
			else new StableDoubleTreeSet(res, if (hasFastSize) size - 1 else -1)
		}
	}

	override def contains(key :Double) :Boolean = {
		val tree = root
		if (tree == null) false
		else tree.entryFor(doubleToLongBits(key))(keyOrdering) != null
	}



	override def keysIteratorFrom(start :Double) :FitIterator[Double] = {
		val tree = root
		if (tree == null) FitIterator.Empty
		else tree.iteratorFrom(this)(doubleToLongBits(start))(keyOrdering)
	}

	protected override def debugString = "StableDoubleTreeSet"
}



