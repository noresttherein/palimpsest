package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.maps.RedBlackTree
import net.turambar.palimpsest.specialty.maps.RedBlackTree.{EntryLens, Node}
import net.turambar.palimpsest.specialty.maps.RedBlackTree.Node.LongSetNode
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize


/**
  * @author Marcin Mościcki
  */
private[sets] class MutableLongTreeSet(private[palimpsest] final var keyCount :Int = 0, zero :Long = 0, l :Node[Long, Unit] = null, r :Node[Long, Unit] = null)
                                      (implicit val ordering :ValOrdering[Long])
	extends LongSetNode(zero, l, r) with MutableTreeSet[Long] with RedBlackTree[Long, Unit] with EntryLens[Long, Unit, Long] with OfKnownSize
{
	override protected type Key = Long

	override protected def root :RedBlackTree[Long, Unit] = this

	override protected def lens :EntryLens[Long, Unit, Long] = this

	
	override def element(node :Node[Long, Unit], sign :Int) :Long = node.key(sign)
	
	override protected def compareRaw(k1 :Long, k2 :Long) :Int = ordering.compare(k1, k2)



	override def size :Int = keyCount

	override def head :Long = firstRawKey

	override def last :Long = lastRawKey



	override def contains(key :Long) :Boolean = containsRaw(key)
	
	override def +=(elem :Long) :this.type = { insertRaw(elem, ()); this }
	
	override def -=(elem :Long) :this.type = { deleteRaw(elem); this }


	
	override def keysIteratorFrom(start :Long) :FitIterator[Long] = iteratorFrom(this)(start)

	
	override def debugString = "MutableLongTreeSet"

}
