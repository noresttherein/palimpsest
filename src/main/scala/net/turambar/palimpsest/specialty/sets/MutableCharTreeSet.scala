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
private[sets] class MutableCharTreeSet(private[palimpsest] final var keyCount :Int = 0, zero :Int = 0, l :Node[Int, Unit] = null, r :Node[Int, Unit] = null)
                                      (implicit val ordering :ValOrdering[Char])
	extends IntSetNode(zero, l, r) with MutableTreeSet[Char] with RedBlackTree[Int, Unit] with EntryLens[Int, Unit, Char] with OfKnownSize
{
	override protected type Key = Int

	override protected def root :RedBlackTree[Int, Unit] = this

	override protected def lens :EntryLens[Int, Unit, Char] = this

	
	override def element(node :Node[Int, Unit], sign :Int) :Char = node.key(sign).toChar
	
	override protected def compareRaw(k1 :Int, k2 :Int) :Int = ordering.compare(k1.toChar, k2.toChar)



	override def size :Int = keyCount

	override def head :Char = firstRawKey.toChar

	override def last :Char = lastRawKey.toChar



	override def contains(key :Char) :Boolean = containsRaw(key)
	
	override def +=(elem :Char) :this.type = { insertRaw(elem, ()); this }
	
	override def -=(elem :Char) :this.type = { deleteRaw(elem); this }


	
	override def keysIteratorFrom(start :Char) :FitIterator[Char] = iteratorFrom(this)(start)

	
	override def debugString = "MutableCharTreeSet"

}
