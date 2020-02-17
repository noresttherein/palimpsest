package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.maps.RedBlackTree
import net.turambar.palimpsest.specialty.maps.RedBlackTree.{EntryLens, Node}
import net.turambar.palimpsest.specialty.maps.RedBlackTree.Node.LongSetNode
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.Vals.OfKnownSize


/**
  * @author Marcin Mościcki
  */
private[sets] class MutableLongTreeSet(implicit val ordering :ValOrdering[Long])
	extends LongSetNode(0L, null, null)
	   with MutableTreeSet[Long] with RedBlackTree[Long, Unit] with EntryLens[Long, Unit, Long] with OfKnownSize
{
	protected final override var keyCount = 0

	override protected type Key = Long

	override protected def root :RedBlackTree[Long, Unit] = this

	override protected def lens :EntryLens[Long, Unit, Long] = this

	
	override def element(node :Node[Long, Unit], sign :Int) :Long = node.key(sign)
	
	override protected def compareRaw(k1 :Long, k2 :Long) :Int = ordering.compare(k1, k2)



	override def contains(key :Long) :Boolean = containsRaw(key)
	
	override def +=(elem :Long) :this.type = { insertRaw(elem, ()); this }
	
	override def -=(elem :Long) :this.type = { deleteRaw(elem); this }


	
	override def keysIteratorFrom(start :Long) :AptIterator[Long] = iteratorFrom(this)(start)

	
	override def debugString = "MutableLongTreeSet"

}
