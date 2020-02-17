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
private[sets] class MutableIntTreeSet(implicit val ordering :ValOrdering[Int])
	extends IntSetNode(0, null, null)
	   with MutableTreeSet[Int] with RedBlackTree[Int, Unit] with EntryLens[Int, Unit, Int] with OfKnownSize
{
	protected final override var keyCount = 0

	override protected type Key = Int

	override protected def root :RedBlackTree[Int, Unit] = this

	override protected def lens :EntryLens[Int, Unit, Int] = this


	override def element(node :Node[Int, Unit], sign :Int) :Int = node.key(sign)

	override protected def compareRaw(k1 :Int, k2 :Int) :Int = ordering.compare(k1, k2)



	override def contains(key :Int) :Boolean = containsRaw(key)

	override def +=(elem :Int) :this.type = { insertRaw(elem, ()); this }

	override def -=(elem :Int) :this.type = { deleteRaw(elem); this }



	override def keysIteratorFrom(start :Int) :FitIterator[Int] = iteratorFrom(this)(start)


	override def debugString = "MutableIntTreeSet"

}
