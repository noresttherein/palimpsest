package net.noresttherein.palimpsest.sets

import java.lang.Float.{floatToIntBits, intBitsToFloat}

import net.noresttherein.palimpsest.iterators.AptIterator
import net.noresttherein.palimpsest.maps.RedBlackTree
import net.noresttherein.palimpsest.maps.RedBlackTree.{EntryLens, Node}
import net.noresttherein.palimpsest.maps.RedBlackTree.Node.IntSetNode
import net.noresttherein.palimpsest.ordered.ValOrdering
import net.noresttherein.palimpsest.Vals.OfKnownSize


/**
  * @author Marcin Mościcki
  */
private[sets] class MutableFloatTreeSet(implicit val ordering :ValOrdering[Float])
	extends IntSetNode(0, null, null)
	   with MutableTreeSet[Float] with RedBlackTree[Int, Unit] with EntryLens[Int, Unit, Float] with OfKnownSize
{
	protected final override var keyCount = 0

	override protected type Key = Int

	override protected def root :RedBlackTree[Int, Unit] = this

	override protected def lens :EntryLens[Int, Unit, Float] = this

	
	override def element(node :Node[Int, Unit], sign :Int) :Float = intBitsToFloat(node.key(sign))
	
	override protected def compareRaw(k1 :Int, k2 :Int) :Int = ordering.compare(intBitsToFloat(k1), intBitsToFloat(k2))



	override def contains(key :Float) :Boolean = containsRaw(floatToIntBits(key))
	
	override def +=(elem :Float) :this.type = { insertRaw(floatToIntBits(elem), ()); this }
	
	override def -=(elem :Float) :this.type = { deleteRaw(floatToIntBits(elem)); this }


	
	override def keysIteratorFrom(start :Float) :AptIterator[Float] = iteratorFrom(this)(floatToIntBits(start))

	
	override def debugString = "MutableFloatTreeSet"

}
