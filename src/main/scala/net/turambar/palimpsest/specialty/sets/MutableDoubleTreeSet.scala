package net.turambar.palimpsest.specialty.sets

import java.lang.Double.{longBitsToDouble, doubleToLongBits}

import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.maps.RedBlackTree
import net.turambar.palimpsest.specialty.maps.RedBlackTree.{EntryLens, Node}
import net.turambar.palimpsest.specialty.maps.RedBlackTree.Node.{LongSetNode}
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.Vals.OfKnownSize


/**
  * @author Marcin Mościcki
  */
private[sets] class MutableDoubleTreeSet(implicit val ordering :ValOrdering[Double])
	extends LongSetNode(0L, null, null)
	   with MutableTreeSet[Double] with RedBlackTree[Long, Unit] with EntryLens[Long, Unit, Double] with OfKnownSize
{
	protected final override var keyCount = 0

	override protected type Key = Long

	override protected def root :RedBlackTree[Long, Unit] = this

	override protected def lens :EntryLens[Long, Unit, Double] = this

	
	override def element(node :Node[Long, Unit], sign :Int) :Double = longBitsToDouble(node.key(sign))
	
	override protected def compareRaw(k1 :Long, k2 :Long) :Int = ordering.compare(longBitsToDouble(k1), longBitsToDouble(k2))



	override def contains(key :Double) :Boolean = containsRaw(doubleToLongBits(key))
	
	override def +=(elem :Double) :this.type = { insertRaw(doubleToLongBits(elem), ()); this }
	
	override def -=(elem :Double) :this.type = { deleteRaw(doubleToLongBits(elem)); this }


	
	override def keysIteratorFrom(start :Double) :AptIterator[Double] = iteratorFrom(this)(doubleToLongBits(start))

	
	override def debugString = "MutableDoubleTreeSet"

}
