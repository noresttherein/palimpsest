package net.turambar.palimpsest.specialty.maps

import scala.annotation.{tailrec, unspecialized}
import scala.collection.mutable.ArrayBuffer

import net.turambar.palimpsest.specialty.maps.AVLTree.{Balanced, LeftHeavy, Node, RightHeavy}
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.Var
import net.turambar.palimpsest.specialty.iterators.FitIterator



private[palimpsest] sealed trait AVLTreeBase[K, V] { this :AVLTree[K, V] =>

	protected def iterator[@specialized(ValueTypes) T](value :Node[K, V] => T) :FitIterator[T] = {
		var node = root
		if (node == null)
			FitIterator.Empty
		else {
			val stack = new ArrayBuffer[Node[K, V]]()
			do {
				stack += node
				node = node.left
			} while (node != null)
			new BSTIterator[Node[K, V], T](stack)(value)
		}
	}


}



/** Base trait for [[net.turambar.palimpsest.specialty.maps.AVLTree]] extracted to define methods which
  * don't require specialization on values, only on keys.
  * @tparam K the raw key type of the tree
  * @tparam V the raw value type of the tree
  */
private[palimpsest] sealed trait AVLTreeKeySpecialization[@specialized(RawKeyTypes) K, V]
	extends AVLTreeBase[K, V]
{ this :AVLTree[K, V] =>
	protected def root :Node[K, V]

	protected def compareRaw(k1 :K, k2 :K) :Int

	@unspecialized
	protected def minNode :Node[K, V] = BSTNode.min(root)

	@unspecialized
	protected def maxNode :Node[K, V] = BSTNode.max(root)

	@unspecialized
	protected def node(idx :Int) :Node[K, V] = BSTNode.node(root, idx)

	protected def iteratorFrom[@specialized(ValueTypes) T](value :Node[K, V] => T)(key :K) :FitIterator[T] = {
		var node = root
		if (node == null)
			FitIterator.Empty
		else {
			val stack = new ArrayBuffer[Node[K, V]]
			do {
				compareRaw(key, node.key) match {
					case -1 =>
						stack += node
						node = node.left
					case 1 =>
						node = node.right
					case _ =>
						stack += node
						node = null
				}
			} while (node != null)
			new BSTIterator(stack)(value)
		}
	}



	protected def deleteRaw(key :K) :Node[K, V] = delete_rec(key)(root)



	private def delete_rec(key :K)(node :Node[K, V]) :Node[K, V] =
		if (node == null)
			null
		else compareRaw(key, node.key) match {
			case -1 =>
				leftDeletion(node, delete_rec(key)(node.left))
			case 1 =>
				rightDeletion(node, delete_rec(key)(node.right))
			case _ => //key == node.key
				val l = node.left
				val r = node.right
				if (l == null)
					r
				else if (r == null)
					l
				else {
					val res = Var[Node[K, V]](null)
					val replacement = delete_min(r, res)
					rightDeletion(node.copy(res.value), replacement)
				}
		}



	private def delete_min(node :Node[K, V], res :Var[Node[K, V]]) :Node[K, V] = {
		val l = node.left
		if (l == null) {
			res := node
			node.right //null or leaf
		} else
			leftDeletion(node, delete_min(l, res))
	}



	private def leftDeletion(node :Node[K, V], replacement :Node[K, V]) = node.left match {
		case _ :Balanced[_, _] =>
			if (replacement == null)
				rebalanceLeftDeletion(node, null)
			else
				node.copy(replacement, node.right)

		case _ => replacement match {
			case _ :Balanced[_, _] =>
				rebalanceLeftDeletion(node, replacement)
			case _ =>
				node.copy(replacement, node.right)
		}
	}

	private def rightDeletion(node :Node[K, V], replacement :Node[K, V]) = node.right match {
		case _ :Balanced[_, _] =>
			if (replacement == null)
				rebalanceRightDeletion(node, null)
			else
				node.copy(node.left, replacement)

		case _ => replacement match {
			case _ :Balanced[_, _] =>
				rebalanceRightDeletion(node, replacement)
			case _ =>
				node.copy(node.left, replacement)
		}
	}



	private def rebalanceLeftDeletion(node :Node[K, V], replacement :Node[K, V]) :Node[K, V] = node match { //depth(l) = depth(replacement) + 1
		case _ :Balanced[_, _] => node.rightHeavy(replacement, node.right) //same depth as before deletion

		case _ :LeftHeavy[_, _] => node.balanced(replacement, node.right) //lower depth after deletion

		case _ => node.right match { //depth(node.right) = depth(replacement) + 2
			case r  :Balanced[K, V] =>
				r.leftHeavy(node.rightHeavy(replacement, r.left), r.right) //same depth as before
			case r :LeftHeavy[K, V] => r.left match { //depth(r.left) = depth(replacement) + 1
				case l :Balanced[K, V] =>
					l.balanced(node.balanced(replacement, l.left), r.balanced(l.right, r.right))
				case l :LeftHeavy[K, V] =>
					l.balanced(node.balanced(replacement, l.left), r.rightHeavy(l.right, r.right))
				case l =>
					l.balanced(node.leftHeavy(replacement, l.left), r.balanced(l.right, r.right))
				//in all cases depth of the node decreases
			}
			case r => //depth(node) = depth(replacement) + 3
				r.balanced(node.balanced(replacement, r.left), r.right) //lower depth than before
		}
	}

	private def rebalanceRightDeletion(node :Node[K, V], replacement :Node[K, V]) :Node[K, V] = node match { //depth(node.right) = depth(replacement) + 1
		case _ :Balanced[_, _] => node.leftHeavy(node.left, replacement)

		case _ :RightHeavy[_, _] => node.balanced(node.left, replacement)

		case _ => node.left match { //depth(node.left) = depth(replacement) + 2
			case l :Balanced[K, V] =>
				l.rightHeavy(l.left, node.leftHeavy(l.right, replacement))
			case l :RightHeavy[K, V] => l.right match { //depth(l.right) = depth(replacement) + 1
				case r :Balanced[K, V] =>
					r.balanced(l.balanced(l.left, r.left), node.balanced(r.right, replacement))
				case r :RightHeavy[K, V] =>
					r.balanced(l.leftHeavy(l.left, r.left), node.balanced(r.right, replacement))
				case r =>
					r.balanced(l.balanced(l.left, r.left), node.rightHeavy(r.right, replacement))
				//in all cases depth of the node decreases
			}
			case l =>
				l.balanced(l.left, node.balanced(l.right, replacement))
		}
	}

}






/** Implementation of immutable Adelson-Velsky and Landis balanced binary search trees.
  * This class
  */
private[palimpsest] trait AVLTree[@specialized(RawKeyTypes) K, @specialized(RawValueTypes) V] extends AVLTreeKeySpecialization[K, V] {
//	root :Node[K, V] =>

	protected def leaf(key :K, value :V) :Node[K, V]

	protected def getNode(key :K) :Node[K, V] = get_rec(key)(root)

	@tailrec
	private def get_rec(key :K)(node :Node[K, V]) :Node[K, V] =
		if (node == null)
			null
		else compareRaw(key, node.key) match {
			case -1 => get_rec(key)(node.left)
			case 1 => get_rec(key)(node.right)
			case _ => node
		}


	def insertRaw(key :K, value :V) :Node[K, V] = {
		val r = root
		if (r == null) leaf(key, value)
		else insert_rec(key, value)(r)
	}


	private def insert_rec(key :K, value :V)(node :Node[K, V]) :Node[K, V] = {
		val cmp = compareRaw(key, node.key)
		if (cmp < 0) node.left match { //key < node.key
			case null =>
				val l = node.leaf(key, value)
				val r = node.right
				if (r == null)
					node.leftHeavy(l, null)
				else
					node.balanced(l, r)

			case l :Balanced[K, V] => insert_rec(key, value)(l) match { //inserting the key can change the depth/balance
				case replacement if replacement eq l => node

				case replacement :Balanced[K, V] =>
					//depth does not change as it's impossible to increase the depth of both left and right at the same time
					node.copy(replacement, node.right)
				//depth(replacement) == depth(node.right) + 2
				case replacement :RightHeavy[K, V] if node.isInstanceOf[LeftHeavy[K, V]] => replacement.right match {
					//left-right rotation
					case lr :Balanced[K, V] =>
						val left = replacement.balanced(replacement.left, lr.left)
						val right = node.balanced(lr.right, node.right)
						lr.balanced(left, right)
					case lr :LeftHeavy[K, V] =>
						val left = replacement.balanced(replacement.left, lr.left)
						val right = node.rightHeavy(lr.right, node.right)
						lr.balanced(left, right)
					case lr =>
						val left = replacement.leftHeavy(replacement.left, lr.left)
						val right = node.balanced(lr.right, node.right)
						lr.balanced(left, right)
				}

				case replacement => node match { //depth of replacement increased from node.left
					case _ :Balanced[K, V] =>
						node.leftHeavy(replacement, node.right)
					case _ :LeftHeavy[K, V] => //depth(replacement) == depth(node.right) + 2 && replacement is LeftHeavy
						//right rotation
						replacement.balanced(replacement.left, node.balanced(replacement.right, node.right))
					case _ => //depth(replacement) == depth(node.left) + 1 == depth(node.right)
						node.balanced(replacement, node.right)
				}

			}

			case l => //depth(l.left) != depth(l.right) => depth of node.left won't change
				val replacement = insert_rec(key, value)(l)
				if (replacement eq l) node
				else node.copy(replacement, node.right)

		} else if (cmp > 0) node.right match { //key < node.key
			case null =>
				val r = node.leaf(key, value)
				val l = node.left
				if (l == null)
					node.rightHeavy(null, r)
				else
					node.balanced(l, r)

			case r :Balanced[K, V] => insert_rec(key, value)(r) match { //inserting the key can change the depth/balance
				case replacement if replacement eq r => node

				case replacement :Balanced[K, V] => node.copy(node.left, replacement)
				//depth(replacement) == depth(node.left) + 2
				case replacement :LeftHeavy[K, V] if node.isInstanceOf[RightHeavy[K, V]] => replacement.left match {
					//right-left rotation
					case rl :Balanced[K, V] =>
						val right = replacement.balanced(rl.right, replacement.right)
						val left = node.balanced(node.left, rl.left)
						rl.balanced(left, right)
					case rl :RightHeavy[K, V] =>
						val right = replacement.balanced(rl.right, replacement.right)
						val left = node.leftHeavy(node.left, rl.left)
						rl.balanced(left, right)
					case rl =>
						val right = replacement.rightHeavy(rl.right, replacement.right)
						val left = node.balanced(node.left, rl.left)
						rl.balanced(left, right)
				}

				case replacement => node match { //depth of replacement increased from node.left
					case _ :Balanced[K, V] =>
						node.rightHeavy(node.left, replacement)
					case _ :RightHeavy[K, V] => //depth(replacement) == depth(node.left) + 2 && replacement is RightHeavy
						//left rotation
						replacement.balanced(node.balanced(node.left, replacement.left), replacement.right)
					case _ => //depth(replacement) == depth(node.right) + 1 == depth(node.left)
						node.balanced(node.left, replacement)
				}

			}

			case r => //r is imbalanced => depth of node.right won't change
				val replacement = insert_rec(key, value)(r)
				if (replacement eq r) node
				else node.copy(node.left, replacement)

		} else { //node.key == key
			node.copy(value)
		}
	}


}






private[palimpsest] object AVLTree {
	type Balance = Int
	@inline final val Balanced :Balance = 0
	@inline final val LeftHeavy :Balance = 0x80000000
	@inline final val RightHeavy :Balance = 0x040000000

	final val MaxSize = 0x7fffffff


	trait Node[@specialized(RawKeyTypes) K, @specialized(RawValueTypes) V] extends BSTNode[Node[K, V]] {
		def key :K
		def value :V

		def leaf(key :K, value :V) :Node[K, V]
		
		def balanced(left: Node[K, V], right: Node[K, V]) :Node[K, V]

		def leftHeavy(left: Node[K, V], right: Node[K, V]) :Node[K, V]

		def rightHeavy(left: Node[K, V], right: Node[K, V]) :Node[K, V]

		def copy(left :Node[K, V], right :Node[K, V]) :Node[K, V]
		
		def copy(value :V) :Node[K, V]

		def copy(entry :Node[K, V]) :Node[K, V]


		override def toString = s"($left, $key: $value, $right)"
	}

	trait Balanced[K, V] extends Node[K, V] {
		override def copy(entry :Node[K, V]) :Node[K, V] = entry.balanced(left, right)
	}

	trait LeftHeavy[K, V] extends Node[K, V] {
		override def copy(entry :Node[K, V]) :Node[K, V] = entry.leftHeavy(left, right)
	}

	trait RightHeavy[K, V] extends Node[K, V] {
		override def copy(entry :Node[K, V]) :Node[K, V] = entry.rightHeavy(left, right)
	}

	object Node {

		trait SetNode[K] extends Node[K, Unit] {
			override def value :Unit = ()
			override def copy(value :Unit) :Node[K, Unit] = this
		}
		
		trait MapNode[K, @specialized(RawValueTypes) V] extends Node[K, V] {
			def copy(value :V, left :Node[K, V], right :Node[K, V]) :Node[K, V]
			
			override def copy(left :Node[K, V], right :Node[K, V]) :Node[K, V] = copy(value, left, right)
			override def copy(value :V) :Node[K, V] = copy(value, left, right)
		}

		//manual specialization to avoid fields for erased keys and values

		trait IntSetTree extends Node[Int, Unit] with SetNode[Int] {

			override def leaf(key :Int, value :Unit) :Node[Int, Unit] =
				new BalancedIntSet(key, null, null)
			
			override def balanced(left: Node[Int, Unit], right: Node[Int, Unit]) :Node[Int, Unit] =
				new BalancedIntSet(key, left, right)

			override def leftHeavy(left: Node[Int, Unit], right: Node[Int, Unit]) :Node[Int, Unit] =
				new LeftIntSet(key, left, right)

			override def rightHeavy(left: Node[Int, Unit], right: Node[Int, Unit]) :Node[Int, Unit] =
				new RightIntSet(key, left, right)
		}

		class BalancedIntSet(override val key :Int, left :Node[Int, Unit], right :Node[Int, Unit])
			extends BSTNode[Node[Int, Unit]](left, right) with IntSetTree with Balanced[Int, Unit]
		{
			override def copy(left :Node[Int, Unit], right :Node[Int, Unit]) :Node[Int, Unit] =
				new BalancedIntSet(key, left, right)
		}

		class LeftIntSet(override val key :Int, left :Node[Int, Unit], right :Node[Int, Unit])
			extends BSTNode[Node[Int, Unit]](left, right) with IntSetTree with LeftHeavy[Int, Unit]
		{
			override def copy(left :Node[Int, Unit], right :Node[Int, Unit]) :Node[Int, Unit] =
				new LeftIntSet(key, left, right)
		}

		class RightIntSet(override val key :Int, left :Node[Int, Unit], right :Node[Int, Unit])
			extends BSTNode[Node[Int, Unit]](left, right) with IntSetTree with RightHeavy[Int, Unit]
		{
			override def copy(left :Node[Int, Unit], right :Node[Int, Unit]) :Node[Int, Unit] =
				new RightIntSet(key, left, right)
		}


		trait LongSetTree extends Node[Long, Unit] with SetNode[Long] {

			override def leaf(key :Long, value :Unit) :Node[Long, Unit] =
				new BalancedLongSet(key, null, null)
			
			override def balanced(left: Node[Long, Unit], right: Node[Long, Unit]) :Node[Long, Unit] =
				new BalancedLongSet(key, left, right)

			override def leftHeavy(left: Node[Long, Unit], right: Node[Long, Unit]) :Node[Long, Unit] =
				new LeftLongSet(key, left, right)

			override def rightHeavy(left: Node[Long, Unit], right: Node[Long, Unit]) :Node[Long, Unit] =
				new RightLongSet(key, left, right)
		}

		class BalancedLongSet(override val key :Long, left :Node[Long, Unit], right :Node[Long, Unit])
			extends BSTNode[Node[Long, Unit]](left, right) with LongSetTree with Balanced[Long, Unit]
		{
			override def copy(left :Node[Long, Unit], right :Node[Long, Unit]) :Node[Long, Unit] =
				new BalancedLongSet(key, left, right)
		}

		class LeftLongSet(override val key :Long, left :Node[Long, Unit], right :Node[Long, Unit])
			extends BSTNode[Node[Long, Unit]](left, right) with LongSetTree with LeftHeavy[Long, Unit]
		{
			override def copy(left :Node[Long, Unit], right :Node[Long, Unit]) :Node[Long, Unit] =
				new LeftLongSet(key, left, right)
		}

		class RightLongSet(override val key :Long, left :Node[Long, Unit], right :Node[Long, Unit])
			extends BSTNode[Node[Long, Unit]](left, right) with LongSetTree with RightHeavy[Long, Unit]
		{
			override def copy(left :Node[Long, Unit], right :Node[Long, Unit]) :Node[Long, Unit] =
				new RightLongSet(key, left, right)
		}



		trait AnySetTree[K] extends Node[K, Unit] with SetNode[K] {

			override def leaf(key :K, value :Unit) :Node[K, Unit] =
				new BalancedAnySet(key, null, null)
			
			override def balanced(left: Node[K, Unit], right: Node[K, Unit]) :Node[K, Unit] =
				new BalancedAnySet(key, left, right)

			override def leftHeavy(left: Node[K, Unit], right: Node[K, Unit]) :Node[K, Unit] =
				new LeftAnySet(key, left, right)

			override def rightHeavy(left: Node[K, Unit], right: Node[K, Unit]) :Node[K, Unit] =
				new RightAnySet(key, left, right)
		}

		class BalancedAnySet[K](override val key :K, left :Node[K, Unit], right :Node[K, Unit])
			extends BSTNode[Node[K, Unit]](left, right) with AnySetTree[K] with Balanced[K, Unit]
		{
			override def copy(left :Node[K, Unit], right :Node[K, Unit]) :Node[K, Unit] =
				new BalancedAnySet(key, left, right)
		}

		class LeftAnySet[K](override val key :K, left :Node[K, Unit], right :Node[K, Unit])
			extends BSTNode[Node[K, Unit]](left, right) with AnySetTree[K] with LeftHeavy[K, Unit]
		{
			override def copy(left :Node[K, Unit], right :Node[K, Unit]) :Node[K, Unit] =
				new LeftAnySet(key, left, right)
		}

		class RightAnySet[K](override val key :K, left :Node[K, Unit], right :Node[K, Unit])
			extends BSTNode[Node[K, Unit]](left, right) with AnySetTree[K] with RightHeavy[K, Unit]
		{
			override def copy(left :Node[K, Unit], right :Node[K, Unit]) :Node[K, Unit] =
				new RightAnySet(key, left, right)
		}

		
		
		
		trait IntIntMapTree extends Node[Int, Int] {

			override def leaf(key :Int, value :Int) :Node[Int, Int] =
				new BalancedIntIntMap(key, value, null, null)
			
			override def balanced(left: Node[Int, Int], right: Node[Int, Int]) :Node[Int, Int] =
				new BalancedIntIntMap(key, value, left, right)

			override def leftHeavy(left: Node[Int, Int], right: Node[Int, Int]) :Node[Int, Int] =
				new LeftIntIntMap(key, value, left, right)

			override def rightHeavy(left: Node[Int, Int], right: Node[Int, Int]) :Node[Int, Int] =
				new RightIntIntMap(key, value, left, right)
		}

		class BalancedIntIntMap(override val key :Int, override val value :Int, l :Node[Int, Int], r :Node[Int, Int])
			extends BSTNode[Node[Int, Int]](l, r) with IntIntMapTree with Balanced[Int, Int]
		{
			override def copy(value :Int) :Node[Int, Int] = new BalancedIntIntMap(key, value, left, right)

			override def copy(left :Node[Int, Int], right :Node[Int, Int]) :Node[Int, Int] =
				new BalancedIntIntMap(key, value, left, right)
		}

		class LeftIntIntMap(override val key :Int, override val value :Int, l :Node[Int, Int], r :Node[Int, Int])
			extends BSTNode[Node[Int, Int]](l, r) with IntIntMapTree with LeftHeavy[Int, Int]
		{
			override def copy(value :Int) :Node[Int, Int] = new LeftIntIntMap(key, value, left, right)

			override def copy(left :Node[Int, Int], right :Node[Int, Int]) :Node[Int, Int] =
				new LeftIntIntMap(key, value, left, right)
		}

		class RightIntIntMap(override val key :Int, override val value :Int, l :Node[Int, Int], r :Node[Int, Int])
			extends BSTNode[Node[Int, Int]](l, r) with IntIntMapTree with RightHeavy[Int, Int]
		{
			override def copy(value :Int) :Node[Int, Int] = new RightIntIntMap(key, value, left, right)

			override def copy(left :Node[Int, Int], right :Node[Int, Int]) :Node[Int, Int] =
				new RightIntIntMap(key, value, left, right)
		}



		trait IntLongMapTree extends Node[Int, Long] {

			override def leaf(key :Int, value :Long) :Node[Int, Long] =
				new BalancedIntLongMap(key, value, null, null)
			
			override def balanced(left: Node[Int, Long], right: Node[Int, Long]) :Node[Int, Long] =
				new BalancedIntLongMap(key, value, left, right)

			override def leftHeavy(left: Node[Int, Long], right: Node[Int, Long]) :Node[Int, Long] =
				new LeftIntLongMap(key, value, left, right)

			override def rightHeavy(left: Node[Int, Long], right: Node[Int, Long]) :Node[Int, Long] =
				new RightIntLongMap(key, value, left, right)
		}

		class BalancedIntLongMap(override val key :Int, override val value :Long, l :Node[Int, Long], r :Node[Int, Long])
			extends BSTNode[Node[Int, Long]](l, r) with IntLongMapTree with Balanced[Int, Long]
		{
			override def copy(value :Long) :Node[Int, Long] = new BalancedIntLongMap(key, value, left, right)

			override def copy(left :Node[Int, Long], right :Node[Int, Long]) :Node[Int, Long] =
				new BalancedIntLongMap(key, value, left, right)
		}

		class LeftIntLongMap(override val key :Int, override val value :Long, l :Node[Int, Long], r :Node[Int, Long])
			extends BSTNode[Node[Int, Long]](l, r) with IntLongMapTree with LeftHeavy[Int, Long]
		{
			override def copy(value :Long) :Node[Int, Long] = new LeftIntLongMap(key, value, left, right)

			override def copy(left :Node[Int, Long], right :Node[Int, Long]) :Node[Int, Long] =
				new LeftIntLongMap(key, value, left, right)
		}

		class RightIntLongMap(override val key :Int, override val value :Long, l :Node[Int, Long], r :Node[Int, Long])
			extends BSTNode[Node[Int, Long]](l, r) with IntLongMapTree with RightHeavy[Int, Long]
		{
			override def copy(value :Long) :Node[Int, Long] = new RightIntLongMap(key, value, left, right)

			override def copy(left :Node[Int, Long], right :Node[Int, Long]) :Node[Int, Long] =
				new RightIntLongMap(key, value, left, right)
		}



		trait IntAnyMapTree[V] extends Node[Int, V] {

			override def leaf(key :Int, value :V) :Node[Int, V] =
				new BalancedIntAnyMap(key, value, null, null)
			
			override def balanced(left: Node[Int, V], right: Node[Int, V]) :Node[Int, V] =
				new BalancedIntAnyMap(key, value, left, right)

			override def leftHeavy(left: Node[Int, V], right: Node[Int, V]) :Node[Int, V] =
				new LeftIntAnyMap(key, value, left, right)

			override def rightHeavy(left: Node[Int, V], right: Node[Int, V]) :Node[Int, V] =
				new RightIntAnyMap(key, value, left, right)
		}

		class BalancedIntAnyMap[V](override val key :Int, override val value :V, l :Node[Int, V], r :Node[Int, V])
			extends BSTNode[Node[Int, V]](l, r) with IntAnyMapTree[V] with Balanced[Int, V]
		{
			override def copy(value :V) :Node[Int, V] = new BalancedIntAnyMap(key, value, left, right)

			override def copy(left :Node[Int, V], right :Node[Int, V]) :Node[Int, V] =
				new BalancedIntAnyMap(key, value, left, right)
		}

		class LeftIntAnyMap[V](override val key :Int, override val value :V, l :Node[Int, V], r :Node[Int, V])
			extends BSTNode[Node[Int, V]](l, r) with IntAnyMapTree[V] with LeftHeavy[Int, V]
		{
			override def copy(value :V) :Node[Int, V] = new LeftIntAnyMap(key, value, left, right)

			override def copy(left :Node[Int, V], right :Node[Int, V]) :Node[Int, V] =
				new LeftIntAnyMap(key, value, left, right)
		}

		class RightIntAnyMap[V](override val key :Int, override val value :V, l :Node[Int, V], r :Node[Int, V])
			extends BSTNode[Node[Int, V]](l, r) with IntAnyMapTree[V] with RightHeavy[Int, V]
		{
			override def copy(value :V) :Node[Int, V] = new RightIntAnyMap(key, value, left, right)

			override def copy(left :Node[Int, V], right :Node[Int, V]) :Node[Int, V] =
				new RightIntAnyMap(key, value, left, right)
		}



		trait LongIntMapTree extends Node[Long, Int] {

			override def leaf(key :Long, value :Int) :Node[Long, Int] =
				new BalancedLongIntMap(key, value, null, null)

			override def balanced(left: Node[Long, Int], right: Node[Long, Int]) :Node[Long, Int] =
				new BalancedLongIntMap(key, value, left, right)

			override def leftHeavy(left: Node[Long, Int], right: Node[Long, Int]) :Node[Long, Int] =
				new LeftLongIntMap(key, value, left, right)

			override def rightHeavy(left: Node[Long, Int], right: Node[Long, Int]) :Node[Long, Int] =
				new RightLongIntMap(key, value, left, right)
		}

		class BalancedLongIntMap(override val key :Long, override val value :Int, l :Node[Long, Int], r :Node[Long, Int])
			extends BSTNode[Node[Long, Int]](l, r) with LongIntMapTree with Balanced[Long, Int]
		{
			override def copy(value :Int) :Node[Long, Int] = new BalancedLongIntMap(key, value, left, right)

			override def copy(left :Node[Long, Int], right :Node[Long, Int]) :Node[Long, Int] =
				new BalancedLongIntMap(key, value, left, right)
		}

		class LeftLongIntMap(override val key :Long, override val value :Int, l :Node[Long, Int], r :Node[Long, Int])
			extends BSTNode[Node[Long, Int]](l, r) with LongIntMapTree with LeftHeavy[Long, Int]
		{
			override def copy(value :Int) :Node[Long, Int] = new LeftLongIntMap(key, value, left, right)

			override def copy(left :Node[Long, Int], right :Node[Long, Int]) :Node[Long, Int] =
				new LeftLongIntMap(key, value, left, right)
		}

		class RightLongIntMap(override val key :Long, override val value :Int, l :Node[Long, Int], r :Node[Long, Int])
			extends BSTNode[Node[Long, Int]](l, r) with LongIntMapTree with RightHeavy[Long, Int]
		{
			override def copy(value :Int) :Node[Long, Int] = new RightLongIntMap(key, value, left, right)

			override def copy(left :Node[Long, Int], right :Node[Long, Int]) :Node[Long, Int] =
				new RightLongIntMap(key, value, left, right)
		}



		trait LongLongMapTree extends Node[Long, Long] {

			override def leaf(key :Long, value :Long) :Node[Long, Long] =
				new BalancedLongLongMap(key, value, null, null)
			
			override def balanced(left: Node[Long, Long], right: Node[Long, Long]) :Node[Long, Long] =
				new BalancedLongLongMap(key, value, left, right)

			override def leftHeavy(left: Node[Long, Long], right: Node[Long, Long]) :Node[Long, Long] =
				new LeftLongLongMap(key, value, left, right)

			override def rightHeavy(left: Node[Long, Long], right: Node[Long, Long]) :Node[Long, Long] =
				new RightLongLongMap(key, value, left, right)
		}

		class BalancedLongLongMap(override val key :Long, override val value :Long, l :Node[Long, Long], r :Node[Long, Long])
			extends BSTNode[Node[Long, Long]](l, r) with LongLongMapTree with Balanced[Long, Long]
		{
			override def copy(value :Long) :Node[Long, Long] = new BalancedLongLongMap(key, value, left, right)

			override def copy(left :Node[Long, Long], right :Node[Long, Long]) :Node[Long, Long] =
				new BalancedLongLongMap(key, value, left, right)
		}

		class LeftLongLongMap(override val key :Long, override val value :Long, l :Node[Long, Long], r :Node[Long, Long])
			extends BSTNode[Node[Long, Long]](l, r) with LongLongMapTree with LeftHeavy[Long, Long]
		{
			override def copy(value :Long) :Node[Long, Long] = new LeftLongLongMap(key, value, left, right)

			override def copy(left :Node[Long, Long], right :Node[Long, Long]) :Node[Long, Long] =
				new LeftLongLongMap(key, value, left, right)
		}

		class RightLongLongMap(override val key :Long, override val value :Long, l :Node[Long, Long], r :Node[Long, Long])
			extends BSTNode[Node[Long, Long]](l, r) with LongLongMapTree with RightHeavy[Long, Long]
		{
			override def copy(value :Long) :Node[Long, Long] = new RightLongLongMap(key, value, left, right)

			override def copy(left :Node[Long, Long], right :Node[Long, Long]) :Node[Long, Long] =
				new RightLongLongMap(key, value, left, right)
		}



		trait LongAnyMapTree[V] extends Node[Long, V] {

			override def leaf(key :Long, value :V) :Node[Long, V] =
				new BalancedLongAnyMap(key, value, null, null)
			
			override def balanced(left: Node[Long, V], right: Node[Long, V]) :Node[Long, V] =
				new BalancedLongAnyMap(key, value, left, right)

			override def leftHeavy(left: Node[Long, V], right: Node[Long, V]) :Node[Long, V] =
				new LeftLongAnyMap(key, value, left, right)

			override def rightHeavy(left: Node[Long, V], right: Node[Long, V]) :Node[Long, V] =
				new RightLongAnyMap(key, value, left, right)
		}

		class BalancedLongAnyMap[V](override val key :Long, override val value :V, l :Node[Long, V], r :Node[Long, V])
			extends BSTNode[Node[Long, V]](l, r) with LongAnyMapTree[V] with Balanced[Long, V]
		{
			override def copy(value :V) :Node[Long, V] = new BalancedLongAnyMap(key, value, left, right)

			override def copy(left :Node[Long, V], right :Node[Long, V]) :Node[Long, V] =
				new BalancedLongAnyMap(key, value, left, right)
		}

		class LeftLongAnyMap[V](override val key :Long, override val value :V, l :Node[Long, V], r :Node[Long, V])
			extends BSTNode[Node[Long, V]](l, r) with LongAnyMapTree[V] with LeftHeavy[Long, V]
		{
			override def copy(value :V) :Node[Long, V] = new LeftLongAnyMap(key, value, left, right)

			override def copy(left :Node[Long, V], right :Node[Long, V]) :Node[Long, V] =
				new LeftLongAnyMap(key, value, left, right)
		}

		class RightLongAnyMap[V](override val key :Long, override val value :V, l :Node[Long, V], r :Node[Long, V])
			extends BSTNode[Node[Long, V]](l, r) with LongAnyMapTree[V] with RightHeavy[Long, V]
		{
			override def copy(value :V) :Node[Long, V] = new RightLongAnyMap(key, value, left, right)

			override def copy(left :Node[Long, V], right :Node[Long, V]) :Node[Long, V] =
				new RightLongAnyMap(key, value, left, right)
		}



		trait AnyIntMapTree[K] extends Node[K, Int] {

			override def leaf(key :K, value :Int) :Node[K, Int] =
				new BalancedAnyIntMap(key, value, null, null)
			
			override def balanced(left: Node[K, Balance], right: Node[K, Int]) :Node[K, Int] =
				new BalancedAnyIntMap(key, value, left, right)

			override def leftHeavy(left: Node[K, Balance], right: Node[K, Int]) :Node[K, Int] =
				new LeftAnyIntMap(key, value, left, right)

			override def rightHeavy(left: Node[K, Balance], right: Node[K, Int]) :Node[K, Int] =
				new RightAnyIntMap(key, value, left, right)
		}

		class BalancedAnyIntMap[K](override val key :K, override val value :Int, l :Node[K, Int], r :Node[K, Int])
			extends BSTNode[Node[K, Int]](l, r) with AnyIntMapTree[K] with Balanced[K, Int]
		{
			override def copy(value :Int) :Node[K, Int] = new BalancedAnyIntMap(key, value, left, right)

			override def copy(left :Node[K, Int], right :Node[K, Int]) :Node[K, Int] =
				new BalancedAnyIntMap(key, value, left, right)
		}

		class LeftAnyIntMap[K](override val key :K, override val value :Int, l :Node[K, Int], r :Node[K, Int])
			extends BSTNode[Node[K, Int]](l, r) with AnyIntMapTree[K] with LeftHeavy[K, Int]
		{
			override def copy(value :Int) :Node[K, Int] = new LeftAnyIntMap(key, value, left, right)

			override def copy(left :Node[K, Int], right :Node[K, Int]) :Node[K, Int] =
				new LeftAnyIntMap(key, value, left, right)
		}

		class RightAnyIntMap[K](override val key :K, override val value :Int, l :Node[K, Int], r :Node[K, Int])
			extends BSTNode[Node[K, Int]](l, r) with AnyIntMapTree[K] with RightHeavy[K, Int]
		{
			override def copy(value :Int) :Node[K, Int] = new RightAnyIntMap(key, value, left, right)

			override def copy(left :Node[K, Int], right :Node[K, Int]) :Node[K, Int] =
				new RightAnyIntMap(key, value, left, right)
		}



		trait AnyLongMapTree[K] extends Node[K, Long] {

			override def leaf(key :K, value :Long) :Node[K, Long] =
				new BalancedAnyLongMap(key, value, null, null)
			
			override def balanced(left: Node[K, Long], right: Node[K, Long]) :Node[K, Long] =
				new BalancedAnyLongMap(key, value, left, right)

			override def leftHeavy(left: Node[K, Long], right: Node[K, Long]) :Node[K, Long] =
				new LeftAnyLongMap(key, value, left, right)

			override def rightHeavy(left: Node[K, Long], right: Node[K, Long]) :Node[K, Long] =
				new RightAnyLongMap(key, value, left, right)
		}

		class BalancedAnyLongMap[K](override val key :K, override val value :Long, l :Node[K, Long], r :Node[K, Long])
			extends BSTNode[Node[K, Long]](l, r) with AnyLongMapTree[K] with Balanced[K, Long]
		{
			override def copy(value :Long) :Node[K, Long] = new BalancedAnyLongMap(key, value, left, right)

			override def copy(left :Node[K, Long], right :Node[K, Long]) :Node[K, Long] =
				new BalancedAnyLongMap(key, value, left, right)
		}

		class LeftAnyLongMap[K](override val key :K, override val value :Long, l :Node[K, Long], r :Node[K, Long])
			extends BSTNode[Node[K, Long]](l, r) with AnyLongMapTree[K] with LeftHeavy[K, Long]
		{
			override def copy(value :Long) :Node[K, Long] = new LeftAnyLongMap(key, value, left, right)

			override def copy(left :Node[K, Long], right :Node[K, Long]) :Node[K, Long] =
				new LeftAnyLongMap(key, value, left, right)
		}

		class RightAnyLongMap[K](override val key :K, override val value :Long, l :Node[K, Long], r :Node[K, Long])
			extends BSTNode[Node[K, Long]](l, r) with AnyLongMapTree[K] with RightHeavy[K, Long]
		{
			override def copy(value :Long) :Node[K, Long] = new RightAnyLongMap(key, value, left, right)

			override def copy(left :Node[K, Long], right :Node[K, Long]) :Node[K, Long] =
				new RightAnyLongMap(key, value, left, right)
		}



		trait AnyAnyMapTree[K, V] extends Node[K, V] {

			override def leaf(key :K, value :V) :Node[K, V] =
				new BalancedAnyAnyMap(key, value, null, null)
				
			override def balanced(left: Node[K, V], right: Node[K, V]) :Node[K, V] =
				new BalancedAnyAnyMap(key, value, left, right)

			override def leftHeavy(left: Node[K, V], right: Node[K, V]) :Node[K, V] =
				new LeftAnyAnyMap(key, value, left, right)

			override def rightHeavy(left: Node[K, V], right: Node[K, V]) :Node[K, V] =
				new RightAnyAnyMap(key, value, left, right)
		}

		class BalancedAnyAnyMap[K, V](override val key :K, override val value :V, l :Node[K, V], r :Node[K, V])
			extends BSTNode[Node[K, V]](l, r) with AnyAnyMapTree[K, V] with Balanced[K, V]
		{
			override def copy(value :V) :Node[K, V] = new BalancedAnyAnyMap(key, value, left, right)

			override def copy(left :Node[K, V], right :Node[K, V]) :Node[K, V] =
				new BalancedAnyAnyMap(key, value, left, right)
		}

		class LeftAnyAnyMap[K, V](override val key :K, override val value :V, l :Node[K, V], r :Node[K, V])
			extends BSTNode[Node[K, V]](l, r) with AnyAnyMapTree[K, V] with LeftHeavy[K, V]
		{
			override def copy(value :V) :Node[K, V] = new LeftAnyAnyMap(key, value, left, right)

			override def copy(left :Node[K, V], right :Node[K, V]) :Node[K, V] =
				new LeftAnyAnyMap(key, value, left, right)
		}

		class RightAnyAnyMap[K, V](override val key :K, override val value :V, l :Node[K, V], r :Node[K, V])
			extends BSTNode[Node[K, V]](l, r) with AnyAnyMapTree[K, V] with RightHeavy[K, V]
		{
			override def copy(value :V) :Node[K, V] = new RightAnyAnyMap(key, value, left, right)

			override def copy(left :Node[K, V], right :Node[K, V]) :Node[K, V] =
				new RightAnyAnyMap(key, value, left, right)
		}



	}	
	
	
}
