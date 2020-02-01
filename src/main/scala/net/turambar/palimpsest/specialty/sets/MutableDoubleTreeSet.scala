package net.turambar.palimpsest.specialty.sets

import java.lang.Double.{longBitsToDouble, doubleToLongBits}

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.maps.RedBlackTree
import net.turambar.palimpsest.specialty.maps.RedBlackTree.{EntryLens, Node}
import net.turambar.palimpsest.specialty.maps.RedBlackTree.Node.{LongSetNode}
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize


/**
  * @author Marcin Mościcki
  */
private[sets] class MutableDoubleTreeSet(private[palimpsest] final var keyCount :Int = 0, zero :Long = 0, l :Node[Long, Unit] = null, r :Node[Long, Unit] = null)
                                        (implicit val ordering :ValOrdering[Double])
	extends LongSetNode(zero, l, r) with MutableTreeSet[Double] with RedBlackTree[Long, Unit] with EntryLens[Long, Unit, Double] with OfKnownSize
{
	override protected type Key = Long

	override protected def root :RedBlackTree[Long, Unit] = this

	override protected def lens :EntryLens[Long, Unit, Double] = this

	
	override def element(node :Node[Long, Unit], sign :Int) :Double = longBitsToDouble(node.key(sign))
	
	override protected def compareRaw(k1 :Long, k2 :Long) :Int = ordering.compare(longBitsToDouble(k1), longBitsToDouble(k2))



	override def size :Int = keyCount

	override def head :Double = longBitsToDouble(firstRawKey)

	override def last :Double = longBitsToDouble(lastRawKey)



	override def contains(key :Double) :Boolean = containsRaw(doubleToLongBits(key))
	
	override def +=(elem :Double) :this.type = { insertRaw(doubleToLongBits(elem), ()); this }
	
	override def -=(elem :Double) :this.type = { deleteRaw(doubleToLongBits(elem)); this }


	
	override def keysIteratorFrom(start :Double) :FitIterator[Double] = iteratorFrom(this)(doubleToLongBits(start))

	
	override def debugString = "MutableDoubleTreeSet"

}
