package net.turambar.palimpsest.specialty.maps

import scala.collection.mutable.ArrayBuffer

import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.{ElementLens, ItemTypes}






/**
  * @author Marcin Mościcki
  */
private[palimpsest] class BSTIterator[N >: Null <: BinaryTree[N], @specialized(ItemTypes) +T](stack :ArrayBuffer[N])(lens :ElementLens[N, T])
	extends AptIterator[T]
{
	def this(root :N)(lens :ElementLens[N, T]) = this(BSTIterator.initStack(root))(lens)

	private[this] var last :N = null

	override def hasNext :Boolean = stack.nonEmpty

	override def head :T = lens.element(stack(stack.length - 1))

	override def next() :T = {
		skip()
		lens.element(last)
	}

	override def skip() :Unit = { //can't just call next() as it would box the returned value
		val depth = stack.length - 1
		var top = stack(depth)
		this.last = top

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
	}

}



private[palimpsest] class ReverseBSTIterator[N <: BinaryTree[N], @specialized(ItemTypes) +T](stack :ArrayBuffer[N])(lens :ElementLens[N, T])
	extends AptIterator[T]
{
	def this(root :N)(lens :ElementLens[N, T]) = this(BSTIterator.reverseStack(root))(lens)

	private[this] var last :N = _

	override def hasNext :Boolean = stack.nonEmpty

	override def head :T = lens.element(stack(stack.length - 1))

	override def next() :T = {
		skip() //we call skip() from next() rather than the other way round to avoid boxing the discarded value by skip()
		lens.element(last)
	}

	override def skip() :Unit = {
		val depth = stack.length - 1
		var top = stack(depth)
		last = top

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
	}

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

