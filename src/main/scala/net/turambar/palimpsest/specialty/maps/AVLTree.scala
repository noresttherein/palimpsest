package net.turambar.palimpsest.specialty.maps

import net.turambar.palimpsest.specialty.ordered.ValOrdering


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
/*
trait AVLTree[@specialized(KeyTypes) K, @specialized(UnitAndValueTypes) V] extends ValOrdering[K] {
//	root :Node[K, V] =>
	protected val root :Node[K, V]

	protected def left(key :K, value :V, left :Node[K, V] = null, right :Node[K, V] = null) :Node[K, V]
	protected def right(key :K, value :V, left :Node[K, V] = null, right :Node[K, V] = null) :Node[K, V]
	protected def balanced(key :K, value :V, left :Node[K, V] = null, right :Node[K, V] = null) :Node[K, V]

	def insert(key :K, value :V) :Node[K, V] =
		if (root == null) balanced(key, value)
		else insert_rec(key, value)(root)



	private def insert_rec(key :K, value :V)(node :Node[K, V]) :Node[K, V] = {
		val k = node.key
		val cmp = compare(key, k)
		if (cmp < 0) node.left match { //key < node.key
			case null =>
//				val l = tree.balanced(key, value)
				val l = node.leaf(key, value)
				val r = node.right
				if (r == null)
//					tree.left(k, node.value, l, null)
					node.leftHeavy(l, null)
				else
//					tree.balanced(k, node.value, l, r)
					node.balanced(l, r)
			case l :Balanced[K, V] => insert(tree)(l, key, value) match {
				case replacement if replacement eq l => node
				case replacement :Balanced[K, V] =>
					node.left = replacement
					node
				case replacement :RightHeavy[K, V] if node.isInstanceOf[LeftHeavy[K, V]] => //depth(replacement) == depth(node.right) + 2
					??? //doubleRightRotation
					val lr = replacement.right
					val rotateLeft = lr.balanced()
				case replacement => node match { //depth of replacement increased from node.left
					case _ :LeftHeavy[K, V] => //depth(replacement) == depth(node.right) + 2 && replacement is LeftHeavy
//						val r = tree.balanced(k, value, replacement.right, node.right)
//						tree.balanced(replacement.key, replacement.value, replacement.left, r)
						val r = node.balanced(replacement.right, node.right) //rotate right
						replacement.balanced(replacement.left, r)
					case _ :Balanced[K, V] =>
//						tree.left(k, node.value, replacement, node.right)
						node.leftHeavy(replacement, node.right)
					case _ =>
//						tree.balanced(k, node.value, replacement, node.right)
						node.balanced(replacement, node.right)
				}

			}

			case l => //depth(l) == depth(node.left)
				node.left = insert(tree)(l, key, value)
				node

		} else if (cmp > 0) node.right match { //key > node.key
			case null =>
				val r = tree.balanced(key, value)
				val l = node.left
				if (l == null)
					tree.right(k, node.value, null, r)
				else
					tree.balanced(k, node.value, l, r)

			case r :Balanced[K, V] => insert(tree)(r, key, value) match {
				case replacement if replacement eq r => node
				case replacement :Balanced[K, V] =>
					node.right = replacement
					node
				case replacement :LeftHeavy[K, V] if node.isInstanceOf[RightHeavy[K, V]] =>
					??? //doubleLeftRotation
				case replacement => node match { //depth of replacement increased from node.right
					case _ :LeftHeavy[K, V] =>
						tree.balanced(k, node.value, node.left, replacement)
					case _ :Balanced[K, V] =>
						tree.right(k, node.value, node.left, replacement)
					case _ => //right heavy - depth(node.left) + 2 == depth(replacement)  && replacement is RightHeavy
						val l = node.balanced(node.left, replacement.left)
						replacement.balanced(l, replacement.right)
				}

			}

			case r => //depth of node.right can't increase
				node.right = insert(tree)(r, key, value)
				node
		} else {
			node.copy(value)
		}
	}

}



object AVLTree {
	type Balance = Int
	@inline final val Balanced :Balance = 0
	@inline final val LeftHeavy :Balance = 0x80000000
	@inline final val RightHeavy :Balance = 0x040000000

	final val MaxSize = 0x7fffffff


	private def rotateRight[K, V](node :Node[K, V]) :Node[K, V] = ???


	trait Node[@specialized(KeyTypes) K, @specialized(UnitAndValueTypes) V] extends ImmutableBSTNode[Node[K, V]] {
		def key :K
		def value :V

//		def compareKey(key :K) :Int

		def leaf(key :K, value :V) :Node[K, V]

		def balanced(left :Node[K, V], right :Node[K, V]) :Node[K, V]

		def leftHeavy(left :Node[K, V], right :Node[K, V]) :Node[K, V]

		def rightHeavy(left :Node[K, V], right :Node[K, V]) :Node[K, V]

		def copy(value :V) :Node[K, V]
	}

	trait Balanced[K, V] extends Node[K, V]
	trait LeftHeavy[K, V] extends Node[K, V]
	trait RightHeavy[K, V] extends Node[K, V]

	trait SetNode[K] extends Node[K, Unit] {
		override def value :Unit = ()
		override def copy(value :Unit) :Node[K, Unit] = this
	}

	trait MapNode[@specialized(KeyTypes) K, @specialized(ValueTypes) V] extends Node[K, V] {
		override def leaf(key :K, value :V) :Node[K, V] = ???
		override def balanced(left :Node[K, V], right :Node[K, V]) :Node[K, V] = ???
		override def leftHeavy(left :Node[K, V], right :Node[K, V]) :Node[K, V] = ???
		override def rightHeavy(left :Node[K, V], right :Node[K, V]) :Node[K, V] = ???
	}

	class BalancedMapNode[@specialized(KeyTypes) K, @specialized(ValueTypes) V](override val key :K, override val value :V)(l :Node[K, V], r :Node[K, V])
		extends MapNode[K, V]
	{
		left = l
		right = r
		size = l.size + 1 + r.size

		override def copy(value :V) = new BalancedMapNode[K, V](key, value)(left, right)
	}

	trait IntMapNode[V] extends IntBSTNode[Node[Int, V]] with Node[Int, V] {
		override def leaf(key :Int, value :V) :Node[Int, V] = new BalancedIntMapNode(key, value, null, null)

		override def balanced(left :Node[Int, V], right :Node[Int, V]) :Node[Int, V] =
			new BalancedIntMapNode(key, value, left, right)

		override def leftHeavy(left :Node[Int, V], right :Node[Int, V]) :Node[Int, V] =
			new LeftHeavyIntMapNode(key, value, left, right)

		override def rightHeavy(left :Node[Int, V], right :Node[Int, V]) :Node[Int, V] =
			new RightHeavyIntMapNode(key, value, left, right)
	}

	class LeftHeavyIntMapNode[@specialized(ValueTypes) V](k :Int, var value :V, l :Node[Int, V], r :Node[Int, V])
		extends IntBSTNode[Node[Int, V]](k, l, r) with Node[Int, V] with IntMapNode[V]

	class RightHeavyIntMapNode[@specialized(ValueTypes) V](k :Int, var value :V, l :Node[Int, V], r :Node[Int, V])
		extends IntBSTNode[Node[Int, V]](k, l, r) with Node[Int, V] with IntMapNode[V]

	class BalancedIntMapNode[@specialized(ValueTypes) V](k :Int, var value :V, l :Node[Int, V], r :Node[Int, V])
		extends IntBSTNode[Node[Int, V]](k, l, r) with Node[Int, V] with IntMapNode[V]



	class IntSetNode(k :Int, l :Node[Int, Unit], r :Node[Int, Unit])
		extends IntBSTNode[Node[Int, Unit]](k, l, r) with SetNode[Int]
	{

		override def leaf(key :Int, value :Unit) :IntSetNode = new BalancedIntSetNode(key, null, null)

		override def balanced(left :Node[Int, Unit], right :Node[Int, Unit]) :IntSetNode =
			new BalancedIntSetNode(key, left, right)

		override def leftHeavy(left :Node[Int, Unit], right :Node[Int, Unit]) :IntSetNode =
			new LeftHeavyIntSetNode(key, left, right)

		override def rightHeavy(left :Node[Int, Unit], right :Node[Int, Unit]) :IntSetNode =
			new RightHeavyIntSetNode(key, left, right)

	}

	class LeftHeavyIntSetNode(k :Int, l :Node[Int, Unit], r :Node[Int, Unit])
		extends IntSetNode(k, l, r)

	class RightHeavyIntSetNode(k :Int, l :Node[Int, Unit], r :Node[Int, Unit])
		extends IntSetNode(k, l, r) with Node[Int, Unit] with SetNode[Int]

	class BalancedIntSetNode(k :Int, l :Node[Int, Unit], r :Node[Int, Unit])
		extends IntSetNode(k, l, r) with Node[Int, Unit] with SetNode[Int]

}
*/
