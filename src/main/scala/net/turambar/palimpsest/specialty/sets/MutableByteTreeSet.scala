package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.maps.RedBlackTree
import net.turambar.palimpsest.specialty.maps.RedBlackTree.{EntryLens, Node}
import net.turambar.palimpsest.specialty.maps.RedBlackTree.Node.IntSetNode
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize


/**
  * @author Marcin Mościcki
  */
private[sets] class MutableByteTreeSet(implicit val ordering :ValOrdering[Byte])
	extends IntSetNode(0, null, null)
	   with MutableTreeSet[Byte] with RedBlackTree[Int, Unit] with EntryLens[Int, Unit, Byte] with OfKnownSize
{
	protected final override var keyCount = 0

	override protected type Key = Int

	override protected def root :RedBlackTree[Int, Unit] = this

	override protected def lens :EntryLens[Int, Unit, Byte] = this

	
	override def element(node :Node[Int, Unit], sign :Int) :Byte = node.key(sign).toByte
	
	override protected def compareRaw(k1 :Int, k2 :Int) :Int = ordering.compare(k1.toByte, k2.toByte)



	override def contains(key :Byte) :Boolean = containsRaw(key)
	
	override def +=(elem :Byte) :this.type = { insertRaw(elem, ()); this }
	
	override def -=(elem :Byte) :this.type = { deleteRaw(elem); this }


	
	override def keysIteratorFrom(start :Byte) :FitIterator[Byte] = iteratorFrom(this)(start)

	
	override def debugString = "MutableByteTreeSet"

}
