package net.turambar.palimpsest.specialty.sets


import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.maps.RedBlackTree
import net.turambar.palimpsest.specialty.maps.RedBlackTree.{EntryLens, Node}
import net.turambar.palimpsest.specialty.maps.RedBlackTree.Node.IntSetNode
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.Vals.OfKnownSize


/**
  * @author Marcin Mościcki
  */
private[sets] class MutableShortTreeSet(implicit val ordering :ValOrdering[Short])
	extends IntSetNode(0, null, null)
	   with MutableTreeSet[Short] with RedBlackTree[Int, Unit] with EntryLens[Int, Unit, Short] with OfKnownSize
{
	protected final override var keyCount = 0

	override protected type Key = Int

	override protected def root :RedBlackTree[Int, Unit] = this

	override protected def lens :EntryLens[Int, Unit, Short] = this

	
	override def element(node :Node[Int, Unit], sign :Int) :Short = node.key(sign).toShort
	
	override protected def compareRaw(k1 :Int, k2 :Int) :Int = ordering.compare(k1.toShort, k2.toShort)



	override def contains(key :Short) :Boolean = containsRaw(key)
	
	override def +=(elem :Short) :this.type = { insertRaw(elem, ()); this }
	
	override def -=(elem :Short) :this.type = { deleteRaw(elem); this }


	
	override def keysIteratorFrom(start :Short) :AptIterator[Short] = iteratorFrom(this)(start)

	
	override def debugString = "MutableShortTreeSet"

}
