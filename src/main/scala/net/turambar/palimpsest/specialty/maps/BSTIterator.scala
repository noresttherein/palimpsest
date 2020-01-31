package net.turambar.palimpsest.specialty.maps

import scala.collection.mutable.ArrayBuffer

import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.{ElementLens, ItemTypes}






/**
  * @author Marcin Mościcki
  */
private[palimpsest] class BSTIterator[N <: BinaryTree[N], @specialized(ItemTypes) +T](stack :ArrayBuffer[N])(lens :ElementLens[N, T])
	extends FitIterator[T]
{
	def this(root :N)(lens :ElementLens[N, T]) = this(BSTIterator.initStack(root))(lens)

	override def hasNext :Boolean = stack.nonEmpty

	override def head :T = lens.element(stack(stack.length - 1))

	override def next() :T = {
		val depth = stack.length - 1
		var top = stack(depth)
		val res = lens.element(top)

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



private[palimpsest] class ReverseBSTIterator[N <: BinaryTree[N], @specialized(ItemTypes) +T](stack :ArrayBuffer[N])(lens :ElementLens[N, T])
	extends FitIterator[T]
{
	def this(root :N)(lens :ElementLens[N, T]) = this(BSTIterator.reverseStack(root))(lens)

	override def hasNext :Boolean = stack.nonEmpty

	override def head :T = lens.element(stack(stack.length - 1))

	override def next() :T = {
		val depth = stack.length - 1
		var top = stack(depth)
		val res = lens.element(top)

		if (top.right == null) {
			stack.remove(depth)
		} else {
			top = top.left
			stack(depth) = top
			while (top.right != null) {
				top = top.right
				stack += top
			}
		}
		res
	}

	override def skip() :Unit = next()

}



private[palimpsest] object BSTIterator {

	def initStack[N <: BinaryTree[N]](root :N) :ArrayBuffer[N] = {
		val stack = new ArrayBuffer[N]
		var node = root
		while (node != null) {
			stack += node
			node = node.left
		}
		stack
	}

	def reverseStack[N <: BinaryTree[N]](root :N) :ArrayBuffer[N] = {
		val stack = new ArrayBuffer[N]
		var node = root
		while (node != null) {
			stack += node
			node = node.right
		}
		stack
	}

}

