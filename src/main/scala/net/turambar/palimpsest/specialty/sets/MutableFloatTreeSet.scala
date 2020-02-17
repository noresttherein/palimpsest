package net.turambar.palimpsest.specialty.sets

import java.lang.Float.{floatToIntBits, intBitsToFloat}

import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.maps.RedBlackTree
import net.turambar.palimpsest.specialty.maps.RedBlackTree.{EntryLens, Node}
import net.turambar.palimpsest.specialty.maps.RedBlackTree.Node.IntSetNode
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.Vals.OfKnownSize


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
