package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.maps.RedBlackTree
import net.turambar.palimpsest.specialty.maps.RedBlackTree.{EntryLens, Node}
import net.turambar.palimpsest.specialty.maps.RedBlackTree.Node.{AnySetNode}
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.Vals.OfKnownSize


/** A generic variant of Red-Black tree implementation. We can't use the default optimizations introduced
  * by [[net.turambar.palimpsest.specialty.maps.RedBlackTree RedBlackTree]] as stored keys are pointers
  * and not numbers and thus have no sign bit that can be repurposed to store color information, requiring an additional
  * field in the node. As the special 'zero' value we use `null` and manually explicitly enforce ordering in which
  * it sorts before any other value, hence only the right child of this tree will ever be non-null.
  * @author Marcin Mościcki
  */
private[sets] class MutableErasedTreeSet[E](implicit val ordering :ValOrdering[E])
	extends AnySetNode[E](null.asInstanceOf[E], RedBlackTree.Black, null, null)
	   with MutableTreeSet[E] with RedBlackTree[E, Unit] with EntryLens[E, Unit, E] with OfKnownSize
{
	protected final override var keyCount = 0

	override protected type Key = E

	override protected def root :RedBlackTree[E, Unit] = this

	override protected def lens :EntryLens[E, Unit, E] = this


	
	override def element(node :Node[E, Unit], sign :Int) :E = node.key(sign)
	
	override protected def compareRaw(k1 :E, k2 :E) :Int =
		if (k1 == null)
			if (k2 == null) 0 else -1
		else if (k2 == null)
			1
		else
			ordering.compare(k1, k2)



	override def contains(key :E) :Boolean = containsRaw(key)
	
	override def +=(elem :E) :this.type = { insertRaw(elem, ()); this }
	
	override def -=(elem :E) :this.type = { deleteRaw(elem); this }


	
	override def keysIteratorFrom(start :E) :AptIterator[E] = iteratorFrom(this)(start)

	
	override def debugString = "MutableErasedTreeSet"

}
