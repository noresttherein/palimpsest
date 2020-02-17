package net.noresttherein.palimpsest.sets

import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.maps.RedBlackTree
import net.noresttherein.palimpsest.maps.RedBlackTree.{EntryLens, Node}
import net.noresttherein.palimpsest.maps.RedBlackTree.Node.IntSetNode
import net.noresttherein.palimpsest.ordered.ValOrdering
import net.noresttherein.palimpsest.Vals.OfKnownSize


/**
  * @author Marcin Mościcki
  */
private[sets] class MutableCharTreeSet(implicit val ordering :ValOrdering[Char])
	extends IntSetNode(0, null, null)
	   with MutableTreeSet[Char] with RedBlackTree[Int, Unit] with EntryLens[Int, Unit, Char] with OfKnownSize
{
	protected final override var keyCount = 0

	override protected type Key = Int

	override protected def root :RedBlackTree[Int, Unit] = this

	override protected def lens :EntryLens[Int, Unit, Char] = this

	
	override def element(node :Node[Int, Unit], sign :Int) :Char = node.key(sign).toChar
	
	override protected def compareRaw(k1 :Int, k2 :Int) :Int = ordering.compare(k1.toChar, k2.toChar)



	override def contains(key :Char) :Boolean = containsRaw(key)
	
	override def +=(elem :Char) :this.type = { insertRaw(elem, ()); this }
	
	override def -=(elem :Char) :this.type = { deleteRaw(elem); this }


	
	override def keysIteratorFrom(start :Char) :AptIterator[Char] = iteratorFrom(this)(start)

	
	override def debugString = "MutableCharTreeSet"

}
