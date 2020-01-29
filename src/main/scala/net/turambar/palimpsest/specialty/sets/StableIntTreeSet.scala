package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.maps.AVLTree
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.sets.StableIntTreeSet.NodeKeys

/**
  * @author Marcin Mościcki
  */
class StableIntTreeSet(protected val root :AVLTree.Node[Int, Unit])(implicit val ordering :ValOrdering[Int])
	extends StableOrderedSet[Int] with AVLTree[Int, Unit]
{
	override protected def leaf(key :Int, value :Unit) :AVLTree.Node[Int, Unit] =
		new AVLTree.Node.BalancedIntSet(key, null, null)


	override protected def compareRaw(k1 :Int, k2 :Int) :Int = ordering.compare(k1, k2)

	override def +(elem :Int) :StableOrderedSet[Int] = {
		val tree = insertRaw(elem, ())
		if (tree eq root) this
		else new StableIntTreeSet(tree)
	}

	override def -(elem :Int) :StableOrderedSet[Int] = {
		val tree = deleteRaw(elem)
		if (tree eq root) this
		else new StableIntTreeSet(tree)
	}

	override def contains(key :Int) :Boolean = getNode(key) != null

	override protected def reverseForeach(f :Int => Unit) :Unit = ???

	override def iterator :FitIterator[Int] = iterator(NodeKeys)

	override def keysIteratorFrom(start :Int) :FitIterator[Int] = iteratorFrom(NodeKeys)(start)

}

object StableIntTreeSet {
	private final val NodeKeys = { node :AVLTree.Node[Int, Unit] => node.key }
}
