package net.turambar.palimpsest.specialty.maps

import scala.collection.mutable.ArrayBuffer

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.ItemTypes


/**
  * @author Marcin Mościcki
  */
private[palimpsest] class BSTIterator[N <: BSTNode[N], @specialized(ItemTypes) +T](stack :ArrayBuffer[N])(value :N => T)
	extends FitIterator[T]
{
	def this(root :N)(value :N => T) = this(BSTIterator.initStack(root))(value)

	override def hasNext :Boolean = stack.nonEmpty

	override def head :T = value(stack(stack.length - 1))

	override def next() :T = {
		val depth = stack.length - 1
		var top = stack(depth)
		val res = value(top)

		if (top.right == null) {
			stack.remove(depth)
		} else {
			top = top.right
			stack(depth) = top
			while (top.left != null) {
				top = top.left
				stack += top
			}
		}
		res
	}

	override def skip() :Unit = next()

}


private[palimpsest] object BSTIterator {

	def initStack[N <: BSTNode[N]](root :N) :ArrayBuffer[N] = {
		val stack = new ArrayBuffer[N]
		var node = root
		while (node != null) {
			stack += node
			node = node.left
		}
		stack
	}
}