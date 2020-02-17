package net.turambar.palimpsest.specialty.maps

import scala.annotation.{tailrec, unspecialized}
import net.turambar.palimpsest.specialty.{?, maps, Blank, ItemTypes, Sure, Var}
import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.maps.RedBlackTree.{Black, Color, EntryLens, Negative, Node, Positive, Red, RedBlackIterator, ReverseRedBlackIterator}

import scala.collection.mutable.ArrayBuffer
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.Fun2



/** Base trait for RedBlackTree extracted to avoid specialization of defined methods. */
private[palimpsest] sealed trait RedBlackTreeBase[K, V] { root :RedBlackTree[K, V] with Node[K, V] =>


	def min[@specialized(ItemTypes) T](lens :EntryLens[K, V, T]): T =
		if (left != null) {
			var res = left.left
			while (res.left != null)
				res = res.left
			lens.element(res, Negative)
		} else if (colorBit != 0)
			lens.element(this, 0)
		else if (right != null) {
			var res = right.right
			while (res.right != null)
				res = res.right
			lens.element(res, Positive)
		} else
			throw new NoSuchElementException(this + ".head")


	def max[@specialized(ItemTypes) T](lens :EntryLens[K, V, T]): T =
		if (left != null) {
			var res = left.left
			while (res.left != null)
				res = res.left
			lens.element(res, Negative)
		} else if (colorBit != 0)
			lens.element(this, 0)
		else if (right != null) {
			var res = right.right
			while (res.right != null)
				res = res.right
			lens.element(res, Positive)
		} else
			  throw new NoSuchElementException(this + ".last")



	def keyAt[@specialized(ItemTypes) T](lens :EntryLens[K, V, T])(idx :Int) :T = {
		if (idx < 0)
			throw new IndexOutOfBoundsException(idx.toString)
		implicit val remainder = Var(idx)
		var res = keyAt_rec(left)
		if (res != null)
			return lens.element(res, Negative)
		if (colorBit != 0) {
			remainder.--
			if (remainder.get < 0)
				return lens.element(this, 0)
		}
		res = keyAt_rec(right)
		if (res == null)
			throw new IndexOutOfBoundsException(idx + " of " + keyCount)
		lens.element(res, Positive)
	}

	private def keyAt_rec(node :Node[K, V])(implicit remainder :Var[Int]) :Node[K, V] =
		if (node == null)
			null
		else keyAt_rec(node.left) match {
			case null =>
				if (remainder.get == 0)
					node
				else {
					remainder.--
					keyAt_rec(node.right)
				}
			case res => res
		}



	def find_?[@specialized(ItemTypes) T](lens :EntryLens[K, V, T])(p :T => Boolean, where :Boolean): ?[T] =
		find_rec(left)(Negative, lens, p, where) match {
			case null if colorBit != 0 && p(lens.element(this, 0)) == where =>
				Sure(lens.element(this, 0))
			case null =>
				find_rec(right)(Positive, lens, p, where) match {
					case null => Blank
					case found => found
				}
			case found => found
		}

	private def find_rec[@specialized(ItemTypes) T]
	                    (node :Node[K, V])(implicit sign :Int, lens :EntryLens[K, V, T], p :T => Boolean, where :Boolean): ?[T] =
		if (node == null)
			null
		else {
			val e = lens.element(node, sign)
			if (p(e) == where)
				Sure(e)
			else find_rec(node.left) match {
					case null => find_rec(node.right)
					case found => found
				}
		}



	def count[@specialized(ItemTypes) T](lens :EntryLens[K, V, T])(p :T => Boolean) :Int =
		count_rec(left)(Negative, lens, p) + count_rec(right)(Positive, lens, p) + (
			if (colorBit != 0 && p(lens.element(this, 0))) 1
			else 0
		)

	private def count_rec[@specialized(ItemTypes) T](node :Node[K, V])(implicit sign :Int, lens :EntryLens[K, V, T], p :T => Boolean) :Int =
		if (node == null)
			0
		else
			count_rec(node.left) + count_rec(node.right) + (if (p(lens.element(node, sign))) 1 else 0)



	def foreach[@specialized(ItemTypes) T](lens :EntryLens[K, V, T])(p :T => Unit) :Unit = {
		foreach_rec(left)(Negative, lens, p)
		if (colorBit != 0)
			p(lens.element(this, 0))
		foreach_rec(right)(Positive, lens, p)
	}

	private def foreach_rec[@specialized(ItemTypes) T](node :Node[K, V])(implicit sign :Int, lens :EntryLens[K, V, T], p :T => Unit) :Unit =
		if (node != null) {
			foreach_rec(node.left)
			p(lens.element(node, sign))
			foreach_rec(node.right)
		}



	def reverseForeach[@specialized(ItemTypes) T](lens :EntryLens[K, V, T])(p :T => Unit) :Unit = {
		reverseForeach_rec(right)(Positive, lens, p)
		if (colorBit != 0)
			p(lens.element(this, 0))
		reverseForeach_rec(left)(Negative, lens, p)
	}

	private def reverseForeach_rec[@specialized(ItemTypes) T]
	                              (node :Node[K, V])(implicit sign :Int, lens :EntryLens[K, V, T], p :T => Unit) :Unit =
		if (node != null) {
			reverseForeach_rec(node.right)
			p(lens.element(node, sign))
			reverseForeach_rec(node.left)
		}



	def foldLeft[@specialized(Fun2) T, @specialized(Fun2) A](lens :EntryLens[K, V, T])(acc :A)(f :(A, T) => A) :A = {
		var a = foldLeft_rec(left)(acc)(Negative, lens, f)
		if (colorBit != 0)
			a = f(a, lens.element(this, 0))
		foldLeft_rec(right)(a)(Positive, lens, f)
	}

	private def foldLeft_rec[@specialized(Fun2) T, @specialized(Fun2) A]
	                        (node :Node[K, V])(acc :A)(implicit sign :Int, lens :EntryLens[K, V, T], f :(A, T) => A) :A =
		if (node == null)
			acc
		else
			foldLeft_rec(node.right)(f(foldLeft_rec(node.left)(acc), lens.element(node, sign)))



	def foldRight[@specialized(Fun2) T, @specialized(Fun2) A](lens :EntryLens[K, V, T])(acc :A)(f :(T, A) => A) :A = {
		var a = foldRight_rec(right)(acc)(Positive, lens, f)
		if (colorBit != 0)
			a = f(lens.element(this, 0), a)
		foldRight_rec(left)(acc)(Negative, lens, f)
	}

	private def foldRight_rec[@specialized(Fun2) T, @specialized(Fun2) A]
	                         (node :Node[K, V])(acc :A)(implicit sign :Int, lens :EntryLens[K, V, T], f :(T, A) => A) :A =
		if (node == null)
			acc
		else
			foldRight_rec(node.left)(f(lens.element(node, sign), foldRight_rec(node.left)(acc)))



	def copyToArray[@specialized(ItemTypes) T](lens :EntryLens[K, V, T])(xs :Array[T], start :Int, total :Int) :Int = {
		var copied = copy_rec(left)(start, total)(Negative, xs, lens)
		if (copied < total && colorBit != 0) {
			xs(start + copied) = lens.element(this, 0)
			copied += 1
		}
		copied + copy_rec(right)(start + copied, total - copied)(Positive, xs, lens)
	}

	private def copy_rec[@specialized(ItemTypes) T]
	                    (node :Node[K, V])(start :Int, total :Int)(implicit sign :Int, xs :Array[T], lens :EntryLens[K, V, T]) :Int =
		if (node == null || total <= 0)
			0
		else {
			var copied = copy_rec(node.left)(start, total)
			val remainder = total - copied
			if (remainder <= 0)
				copied
			else {
				xs(start + copied) = lens.element(node, sign)
				copied += 1
				if (remainder == 1)
					total
				else
					copied + copy_rec(node.right)(start + copied, remainder - 1)
			}
		}




	def iterator[@specialized(ItemTypes) T](lens :EntryLens[K, V, T]) :AptIterator[T] = {
		//the stack will contain all the nodes on the path to the smallest key
		val stack = new ArrayBuffer[Node[K, V]]
		val sign = fillIteratorStack(stack)
		new RedBlackIterator[K, V, T](stack, sign)(lens)
	}

	private def fillIteratorStack(stack: ArrayBuffer[Node[K, V]]) :Int =
		if (left != null) { //there are negative elements
			stack += this
			var top = left
			while (top.left != null) {
				top = top.left
				stack += top
			}
			Negative
		} else if (colorBit != 0) { //zero (this.key) is in the tree and no negative elements.
			stack += this
			Positive
		} else if (right != null) { //only positive elements present in the tree.
			var top = right
			stack += top
			while (top.left != null) {
				top = top.left
				stack += top
			}
			Positive
		} else //no keys in the tree => empty stack
			Positive



	def reverseIterator[@specialized(ItemTypes) T](lens :EntryLens[K, V, T]) :AptIterator[T] = {
		//the stack will contain all the nodes on the path to the smallest key
		val stack = new ArrayBuffer[Node[K, V]]
		val sign = fillReverseIteratorStack(stack)
		new ReverseRedBlackIterator[K, V, T](stack, sign)(lens)
	}

	def fillReverseIteratorStack(stack :ArrayBuffer[Node[K, V]]) :Int =
		if (right != null) { //there are positive elements
			stack += this
			var top = right
			while (top.right != null) {
				top = top.right
				stack += top
			}
			Positive
		} else if (colorBit != 0) { //zero (this.key) is in the tree and no positive elements.
			stack += this
			Positive
		} else if (left != null) { //only negative elements present in the tree.
			var top = left
			stack += top
			while (top.right != null) {
				top = top.right
				stack += top
			}
			Negative
		} else //no keys in the tree => empty stack
			Positive



}






private[palimpsest] sealed trait RedBlackTreeKeySpecialization[@specialized(RawKeyTypes) K, V] extends RedBlackTreeBase[K, V] {
	root :RedBlackTree[K, V] with Node[K, V] =>



	def iteratorFrom[@specialized(ItemTypes) T](lens :EntryLens[K, V, T])(start :K) :AptIterator[T] = {
		val stack = new ArrayBuffer[Node[K, V]]
		val sign = iteratorFromStack(start, stack)
		new RedBlackIterator[K, V, T](stack, sign)(lens)
	}

	private def iteratorFromStack(start :K, stack :ArrayBuffer[Node[K, V]]) :Int = {
		//the stack will contain all nodes on the path to the smallest key larger than start from which we descended left.
		var k = key(0)
		var cmp = compareRaw(start, k)

		/** Recursively go down the tree searching for the first key larger than start. */
		def descend(node :Node[K, V], sign :Int, stack :ArrayBuffer[Node[K, V]]) :Unit = {
			var top = node
			while (top != null) {
				k = top.key(sign)
				cmp = compareRaw(start, k)
				if (cmp < 0) {
					stack += top
					top = top.left
				} else if (cmp > 0)
					top = top.right
				else {
					stack += top
					top = null
				}
			}
		}

		val stack = new ArrayBuffer[Node[K, V]]
		if (cmp < 0 && left != null) { //first iterator key is in the left subtree
			stack += this
			descend(left, Negative, stack)
			Negative
		} else if (cmp <= 0 && colorBit != 0) { //start <= 0 and zero key is present, hence we are the first node.
			stack += this
			Positive
		} else { //start > 0 || start >= 0 && !contains(0) ==> the first iterator node is in the right subtree
			descend(right, Positive, stack)
			Positive
		}

	}



	/** Delete the node associated with `key`, which is known to belong to the left subtree of `parent`.
	  * If the node was found, it is deleted and returned, but before that a rebalancing attempt is made, possibly
	  * changing the colors of `parent` and any of its children as well as changing any node under `parent`, including
	  * replacing itself with another node as the `granny`'s child.
	  * After the method returns, invariants are restored except the black paths down the parent may be by one shorter
	  * than those down its sibling. If this happens, the color of the returned node is set to (or remains) black.
	  * Red color indicates that all invariants hold.
	  * @param key the key to delete from the tree.
	  * @param sign the sign bit shared by all keys under `parent.left`. Zero if we are in the positive (root.right)
	  *             tree and `Int.MinValue` (sole sign bit) for the negative (root.left) tree.
	  * @param granny the parent of `parent`; may be null if `granny` == `root`.
	  * @param parent the node from which we must continue descent left in order to find `key` in the tree.
	  * @return the deleted node or `null` if the key was not found. If not null, the node's key is equal to `key`
	  *         and `value` is equal to the previously associated value with `key` in the tree. The node is guaranteed
	  *         to not be the part of the tree anymore and its color indicates if rebalancing is needed after the method
	  *         returns: black means that the black paths through the `parent` node are one shorter than they were
	  *         before the call; red means that the tree has been completely restored.
	  */
	protected final def deleteLeft(key :K, sign :Int)(granny :Node[K, V], parent :Node[K, V]): Node[K, V] = {
		val node = parent.left
		if (node == null) //the key was not present in this tree
			null
		else {
			val k = node.key(sign)
			val cmp = compareRaw(key, k)
			val res =
				if (cmp < 0)
					deleteLeft(key, sign)(parent, node)
				else if (cmp > 0)
					deleteRight(key, sign)(parent, node)
				else
					deleteNode(sign)(parent, node)
			if (granny != null && res != null && res.color == Black)
				rebalanceLeftDelete(granny, parent, res)
			res
		}
	}



	/** Delete the node associated with `key`, which is known to belong to the right subtree of `parent`.
	  * If the node was found, it is deleted and returned, but before that a rebalancing attempt is made, possibly
	  * changing the colors of `parent` and any of its children as well as changing any node under `parent`, including
	  * replacing itself with another node as the `granny`'s child.
	  * After the method returns, invariants are restored except the black paths down the parent may be by one shorter
	  * than those down its sibling. If this happens, the color of the returned node is set to (or remains) black.
	  * Red color indicates that all invariants hold.
	  * @param key the key to delete from the tree.
	  * @param sign the sign bit shared by all keys under `parent.right`. Zero if we are in the positive (root.right)
	  *             tree and `Int.MinValue` (sole sign bit) for the negative (root.left) tree.
	  * @param granny the parent of `parent`; may be null if `granny` == `root`.
	  * @param parent the node from which we must continue descent right in order to find `key` in the tree.
	  * @return the deleted node or `null` if the key was not found. If not null, the node's key is equal to `key`
	  *         and `value` is equal to the previously associated value with `key` in the tree. The node is guaranteed
	  *         to not be the part of the tree anymore and its color indicates if rebalancing is needed after the method
	  *         returns: black means that the black paths through the `parent` node are one shorter than they were
	  *         before the call; red means that the tree has been completely restored.
	  */
	protected final def deleteRight(key :K, sign :Int)(granny :Node[K, V], parent :Node[K, V]): Node[K, V] = {
		val node = parent.right
		if (node == null) //the key was not present in this tree
			null
		else {
			val k = node.key(sign)
			val cmp = compareRaw(key, k)
			val res =
				if (cmp < 0)
					deleteLeft(key, sign)(parent, node)
				else if (cmp > 0)
			        deleteRight(key, sign)(parent, node)
				else
					deleteNode(sign)(parent, node)
			if (granny != null && res != null && res.color == Black)
				rebalanceRightDelete(granny, parent, res)
			res
		}
	}



	/** Delete the leftmost node in the tree rooted in `parent` (the one with the minimal key) and attempt rebalancing.
	  * @param granny the parent of `parent`. Will be not null.
	  * @param parent the subtree of which the minimal key must be deleted; may be any of `granny`'s children. Not null.
	  * @param node the left child of `parent`, may be `null`.
	  * @return the deleted node with its key and value unchanged. If its color is black, the black paths going through
	  *         the `parent` node (or the node which replaced it as `granny`'s child) are one shorter than they used to be.
	  *         Red color indicates the invariants hold for the whole tree.
	  */
	@unspecialized
	protected final def deleteMin(granny :Node[K, V], parent :Node[K, V], node: Node[K, V]) :Node[K, V] = {
		if (node == null) { //parent is the minimal element
			if (granny.left eq parent)
				granny.left = parent.right
			else
				granny.right = parent.right
			parent
		} else {
			val succ = deleteMin(parent, node, node.left)
			if (succ.color == Black)
				rebalanceLeftDelete(granny, parent, succ)
			succ
		}
	}


	/** Attempts to rebalance the `parent` tree after a black node was deleted under its left child.
	  * It should be called for the same arguments from the recursive `deleteLeft` if the rebalancing was not completed.
	  * This means that at the current state, black paths going down `parent.left` are shorter by one than they used
	  * to be, and one shorter than the other paths in the tree. The single step of rebalancing will try to either
	  * modify the tree to introduce a black node under `parent.left` or, failing that, remove one from `parent.right`
	  * so that all paths going down `parent` are of equal length. Any part of the tree under `parent` may have
	  * its color or children adjusted to restore the invariants, including swapping `parent` for another node
	  * as the child of `granny`, separating the pair by `parent.right` via a rotation.
	  * The success of the operation is signaled by changing the color of the returned deleted node `res` to red.
	  * Otherwise further rebalancing will be needed up the tree, performed by another call to either
	  * of `rebalanceLeftDelete` or `rebalanceRightDelete` with current `parent` as the `node` argument.
	  * @param granny the parent node of `parent`, always not null (root needs no rebalancing).
	  * @param parent the node whose left child marks the root of the tree where a black node was deleted.
	  * @param res the node with the deleted value, as returned by `deleteLeft`. Its color indicates if rebalancing
	  *            was completed or not needed (red) or if the paths going through `parent.left` are shorter by
	  *            one black node than the paths going through parent.right.
	  */
	@unspecialized
	protected final def rebalanceLeftDelete(granny :Node[K, V], parent :Node[K, V], res :Node[K, V]) :Unit = {
		val node = parent.left //can be null!
		if (node != null && node.color == Red) {
			node.color = Black  //paint it black and we are done
			res.color = Red
		} else {
			//black paths through parent.left are one shorter than those through parent.right
			var anchor = granny //updated real parent of `parent`
			var sibling = parent.right //can't be null because parent.left was black - the black path lengths would differ
			if (sibling.color == Red) { //parent must be black;
				sibling.color = Black //sibling's children can't be null as node was black (paths of at least length 2)
				parent.color = Red
				granny.rotateLeft(parent)  //black path length to sibling.right and sibling.left is preserved
				anchor = sibling //the node between granny and parent
				sibling = parent.right //must be black because it was the left child of red sibling
				//we extended the path, introducing another node between granny and parent
				//we'll color parent black in a moment, but need to decrease the black path under sibling to match node
			} //sibling is black and not null
			val l = sibling.left; var r = sibling.right
			if ((l == null || l.color == Black) && (r == null || r.color == Black)) {
				sibling.color = Red //if parent is red, we will recolor it to black in the next call.
				//if we introduced the previous sibling as a node between granny and parent we must do another step
				if (!(granny eq anchor))
					rebalanceLeftDelete(anchor, parent, res)
				//The balancing may not be complete, but the next stack frame will be at the right spot to continue
			} else {
				if (r == null || r.color == Black) {
					l.color = Black //l can't be null because the previous if would trigger
					sibling.color = Red
					sibling = sibling.rotateRight
					parent.right = sibling
					r = sibling.right
				} //now r = sibling.right is red
				sibling.color = parent.color
				parent.color = Black
				r.color = Black
				anchor.rotateLeft(parent)
				res.color = Red //fixed! signal upper stack frames no further action is needed.
			}
		}
	}



	/** Attempts to rebalance the `parent` tree after a black node was deleted under its right child.
	  * It should be called for the same arguments from the recursive `deleteRight` if the rebalancing was not completed.
	  * This means that at the current state, black paths going down `parent.right` are shorter by one than they used
	  * to be, and one shorter than the other paths in the tree. The single step of rebalancing will try to either
	  * modify the tree to introduce a black node under `parent.right` or, failing that, remove one from `parent.left`
	  * so that all paths going down `parent` are of equal length. Any part of the tree under `parent` may have
	  * its color or children adjusted to restore the invariants, including swapping `parent` for another node
	  * as the child of `granny`, separating the pair by `parent.left` via a rotation.
	  * The success of the operation is signaled by changing the color of the returned deleted node `res` to red.
	  * Otherwise further rebalancing will be needed up the tree, performed by another call to either
	  * of `rebalanceLeftDelete` or `rebalanceRightDelete` whith current `parent` as the `node` argument.
	  * @param granny the parent node of `parent`, always not null (the root needs no rebalancing).
	  * @param parent the node whose right child marks the root of the tree where a black node was deleted.
	  * @param res the node with the deleted value, as returned by `deleteRight`. Its color indicates if rebalancing
	  *            was completed or not needed (red) or if the paths going through `parent.right` are shorter by
	  *            one black node than the paths going through parent.right.
	  */
	@unspecialized
	protected final def rebalanceRightDelete(granny :Node[K, V], parent :Node[K, V], res :Node[K, V]) :Unit = {
		val node = parent.right
		if (node != null && node.color == Red) {//node might not be parent.left anymore, but in that case it will be red.
			node.color = Black  //paint it black and we are done
			res.color = Red
		} else {
			var anchor = granny //actual parent of `parent`
			var sibling = parent.left //can't be null because node is black - the length of the black path would differ
			if (sibling.color == Red) { //parent must be black;
				sibling.color = Black //sibling's children can't be null as node was black
				parent.color = Red
				granny.rotateRight(parent)  //black path length to sibling.right and sibling.left is preserved
				anchor = sibling
				sibling = parent.left //must be black because it was the child of red sibling
				//we extended the path, introducing another node before granny and parent
				//we'll color parent black in a moment, but need to decrease the black path under sibling to match node
			} //sibling is black and non-null
			var l = sibling.left; val r = sibling.right
			if ((l == null || l.color == Black) && (r == null || r.color == Black)) {
				sibling.color = Red //if parent is red, we will recolor it in the next call
				if (!(granny eq anchor)) //we introduced another node that the recursion and need an extra step
					rebalanceRightDelete(anchor, parent, res)
				//unwind the stack and continue rebalancing granny->parent or granny->parent.left (both black)
			} else {
				if (l == null || l.color == Black) {
					r.color = Black //l can't be null because the previous if would trigger
					sibling.color = Red
					sibling = sibling.rotateLeft
					parent.left = sibling
					l = sibling.left
				} //now l = sibling.left is red
				sibling.color = parent.color
				parent.color = Black
				l.color = Black
				anchor.rotateRight(parent)
				res.color = Red //fixed! signal upper stack frames no further action is needed.
			}
		}
	}



}






/** A mutable Red-Black Tree implementation trait extended by collection classes optimized for value type keys
  * organised using their natural ordering.
  * Each tree node, apart from the left and right children pointers, stores only the key value in its binary format.
  * The colour of the node is stored in place of the sign bit of the key. After masking the sign bit, the values
  * can be compared as normal as long as both are of the same sign. To make up for the loss of sign information,
  * the root tree node always represents the zero value, meaning all negative values are in the left subtree and
  * all positive values are in the right subtree. We loosen the balancing requirement to apply only to each subtree
  * separately. Finally, we declare the colour of the root to be black by definition and use its sign bit instead
  * to represent whether the zero key is actually present in the set, or serves only as the artificial separator.
  *
  * Additionally, we force extending collection classes to extend also the
  * [[net.turambar.palimpsest.specialty.maps.RedBlackTree.Node Node]] base class and thus serve as the tree root
  * themselves. As in the classic collection implementation we would need to store a pointer to the root node anyway,
  * this doesn't even extend the length of the path to the node measured in the number of accessed objects.
  * In the end, the only cost of obtaining the most compact representation possible, is slightly slower key comparison
  * caused by the need for masking the sign/colour bit each time (and optionally Int-to-Float and Long-to-Double cast);
  * this is negligible in the presence of already incurred key getter method cost.
  *
  * @author Marcin Mościcki
  */
private[palimpsest] trait RedBlackTree[@specialized(RawKeyTypes) K, @specialized(RawValueTypes) V]
	extends RedBlackTreeKeySpecialization[K, V]
{ root :Node[K, V] =>

	override def color :Color = Black

	override def color_=(color :Color) :Unit = ()

	protected def compareRaw(k1 :K, k2 :K) :Int

	protected def red(key :K, value :V) :Node[K, V]

	/** The number of keys in this tree. */
	protected var keyCount :Int

//	override def count :Int = keyCount


/*

	def firstRawKey :K = {
		if (left != null) { //negative keys present
			var node = left
			while (node.left != null)
				node = node.left
			node.key(Negative)
		} else if (right == null) { //no negative and no positive keys in the tree
			if (color == 0) //the zero key is not part of the collection
				throw new NoSuchElementException(this + ".firstKey")
			key(0)
		} else if (color != 0) { //the zero key is part of the collection and thus the smallest key
			key(0)
		} else { //only positive keys present in the tree
			var node = right
			while (node.left != null)
				node = node.left
			node.key(0)
		}
	}



	def lastRawKey :K = {
		if (right != null) { //positive keys present
			var node = right
			while (node.right != null)
				node = node.right
			node.key(0)
		} else if (left == null) { //no negative and no positive keys in the tree
			if (color == 0) //the zero key is not part of the collection
				throw new NoSuchElementException(this + ".lastKey")
			key(0)
		} else if (color != 0) { //the zero key is part of the collection and thus the largest key
			key(0)
		} else {
			var node = left
			while (node.right != null) {
				node = node.right
			}
			node.key(Negative)
		}
	}
*/




	def containsRaw(key :K) :Boolean = {
		var k = this.key(0)
		var cmp = compareRaw(k, key)
		if (cmp == 0) //key == 0
			colorBit != 0
		else {
			var sign = Positive
			var node = right
			if (cmp < 0) {
				sign = Negative
				node = left
			}
			while (node != null) {
				k = node.key(sign)
				cmp = compareRaw(k, key)
				if (cmp > 0)
					node = node.right
				else if (cmp < 0)
					node = node.left
				else
					return true
			}
			false
		}

	}



	def rawKeysIterator :AptIterator[K] = iterator(keys)

	def rawKeysIteratorFrom(start :K) :AptIterator[K] = iteratorFrom(keys)(start)



	def insertRaw(key :K, value :V): ?[V] = {
		val cmp = compareRaw(key, root.key(0))
		//no rebalancing needed as root remains always black
		if (cmp < 0)
			insertLeft(key, value, Negative)(null, root)
		else if (cmp > 0)
			insertRight(key, value, Positive)(null, root)
		else //key == 0
			if (colorBit == 0) { //zero key is not present, add it to the tree
				this.value = value
				colorBit = Red
				keyCount += 1
				Blank
			} else { //zero already present, just swap the value
				val res = Sure(this.value)
				this.value = value
				res
			}
	}



	/** Recursively descend down the left subtree of `parent` in search for the place to insert `key` -> `value`.
	  * Rebalancing may swap the `parent` node as the child of `granny` and change the color of `parent`
	  * and any nodes beneath it. It will not modify the color of `granny` or its other child (the sibling of parent)
	  * and the length of the black path is preserved.
	  * After the method returns, the Red-Black invariant may be invalidated in that both the node occupying the place of
	  * `parent` under `granny` and its left child can be red; fixing this is the responsibility of the caller.
	  * @param key the key mapping to the inserted value.
	  * @param value the inserted value.
	  * @param sign a mask for the sign (highest) bit shared by all elements in the subtree of `parent`.
	  * @param parent the node of which the left child is the current node.
	  * @param granny the parent node of parent; may be null when invoked for the tree root.
	  * @return the value previously associated with `key`, if it was present in the tree.
	  */
	private def insertLeft(key :K, value :V, sign :Int)(granny :Node[K, V], parent :Node[K, V]): ?[V] = {
		var node = parent.left
		if  (node == null) { //key wasn't present, create a new node under parent with key -> value
			node = red(key, value)
			parent.left = node
			keyCount += 1
			Blank
		} else {
			val k = node.key(sign)
			val cmp = compareRaw(key, k)
			if (cmp < 0) {
				val res = insertLeft(key, value, sign)(parent, node)
				rebalanceLeftInsert(granny, parent, false)
				res
			} else if (cmp > 0) {
				val res = insertRight(key, value, sign)(parent, node)
				rebalanceLeftInsert(granny, parent, true)
				res
			} else { //the key already present, just swap the value
				val prev = node.sure
				node.value = value
				prev
			}
		}
	}


	/** Recursively descend down the right subtree of `parent` in search for the place to insert `key` -> `value`.
	  * Rebalancing may swap the `parent` node as the child of `granny` and change the color of `parent` and any nodes
	  * beneath it. It will not modify the color of `granny` or its other child (the sibling of parent)
	  * and the length of the black path is preserved.
	  * After the method returns, the Red-Black invariant may be invalidated in that both the node occupying the place
	  * of `parent` under `granny` and its right child can be red; fixing this is the responsibility of the caller.
	  * @param key the key mapping to the inserted value.
	  * @param value the inserted value.
	  * @param sign a mask for the sign (highest) bit shared by all elements in the subtree of `parent`.
	  * @param parent the node of which the left child is the current node.
	  * @param granny the parent node of parent.
	  * @return the value previously associated with `key`, if it was present in the tree.
	  */
	private def insertRight(key :K, value :V, sign :Int)(granny :Node[K, V], parent :Node[K, V]): ?[V] = {
		var node = parent.right
		if  (node == null) { //key wasn't present, create a new node under parent with key -> value
			node = red(key, value)
			parent.right = node
			keyCount += 1
			Blank
		} else {
			val k = node.key(sign)
			val cmp = compareRaw(key, k)
			if (cmp < 0) {
				val res = insertLeft(key, value, sign)(parent, node)
				rebalanceRightInsert(granny, parent, true)
				res
			} else if (cmp > 0) {
				val res = insertRight(key, value, sign)(parent, node)
				rebalanceRightInsert(granny, parent, false)
				res
			} else { //the key already present, just swap the value
				val prev = node.sure
				node.value = value
				prev
			}
		}
	}



	/** Perform tree rebalancing after possible element insertion under the left subtree of the `parent` tree.
	  * The `parent.left` node here is guaranteed to be non-null and the `grandchildIsRight` flag indicates
	  * which child further down was modified.
	  * This can move and recolor any nodes under the `parent` tree, including swapping `parent` with another node
	  * as the child of `granny`. After method returns, black paths are preserved under the whole subtree,
	  * but both `parent` (or its replacement) and its left child can be red, requiring further fixing
	  * up the tree.
	  * @param granny the parent node of `parent`, may be null if `parent` is the root node.
	  * @param parent the parent of the node under which an element was potentially inserted.
	  * @param grandchildIsRight `true` ''iff'' insertion happened at or under `parent.left.right` node.
	  */
	@unspecialized
	private def rebalanceLeftInsert(granny :Node[K, V], parent :Node[K, V], grandchildIsRight :Boolean) :Unit = {
		var node = parent.left
		val l = node.left
		if (node.color == Red && l != null && l.color == Red) { //parent.color must be black
			val sibling = parent.right
			if (sibling != null && sibling.color == Red) {
				node.color = Black
				sibling.color = Black
				parent.color = Red
				//skip the preceding stack frame and continue fixing when node == current granny
			} else { //sibling.color == Black
				if (grandchildIsRight) {
					node = node.rotateLeft
					parent.left = node
				}
				node.color = Black
				if (granny != null) {
					parent.color = Red
					granny.rotateRight(parent)
				} //fixed!
			}
		}
	}



	/** Perform tree rebalancing after possible element insertion under the right subtree of the `parent` tree.
	  * The `parent.right` node here is guaranteed to be non-null and the `grandchildIsLeft` flag indicates
	  * which child further down was modified.
	  * This can move and recolor any nodes under the `parent` tree, including swapping `parent` with another node
	  * as the child of `granny`. After method returns, black paths are preserved under the whole subtree,
	  * but both `parent` (or its replacement) and its right child can be red, requiring further fixing
	  * up the tree.
	  * @param granny the parent node of `parent`, may be null if `parent` is the root node.
	  * @param parent the parent of the node under which an element was potentially inserted.
	  * @param grandchildIsLeft `true` ''iff'' insertion happened at or under `parent.right.left` node.
	  */
	@unspecialized
	private def rebalanceRightInsert(granny :Node[K, V], parent :Node[K, V], grandchildIsLeft :Boolean) :Unit = {
		var node = parent.right
		val r = node.right
		if (node.color == Red && r != null && r.color == Red) { //parent.color must be black
			val sibling = parent.left
			if (sibling != null && sibling.color == Red) {
				node.color = Black
				sibling.color = Black
				parent.color = Red
				//skip the preceding stack frame and continue fixing when node == current granny
			} else { //sibling.color == Black
				if (grandchildIsLeft) {
					node = node.rotateRight
					parent.right = node
				}
				node.color = Black
				if (granny != null) {
					parent.color = Red
					granny.rotateLeft(parent)
				} //fixed!
			}
		}
	}






	def deleteRaw(key :K): ?[V] = {
		val cmp = compareRaw(key, this.key(0))
		val prev =
			if (cmp < 0)
				deleteLeft(key, Negative)(null, root)
			else if (cmp > 0)
		        deleteRight(key, Positive)(null, root)
			else //key == 0
				return {
					if (colorBit == 0) //zero key is not part of the tree
						Blank
					else {
						colorBit = 0
						keyCount -= 1
						sure
					}
				}
		if (prev == null) Blank
		else prev.sure
	}


	protected final def deleteNode(sign :Int)(parent :Node[K, V], node :Node[K, V]) :Node[K, V] = {
		var found = node
		keyCount -= 1
		val l = node.left; val r = node.right
		if (l == null) {
			parent.replace(node, r)
		} else if (r == null) {
			parent.replace(node, l)
		} else { //node has both children, swap the key with its successor
			found = deleteMin(node, r, r.left)
			if (found.color == Black)
				rebalanceRightDelete(parent, node, found)
			val old = node.value
			node.key_=(found.key(sign))
			node.value = found.value
			found.value = old
		}
		found
	}




}






private[palimpsest] object RedBlackTree {
	type Color = Int
	@inline final val Black = 0
	@inline final val Red  = 0x80000000

	@inline final val Positive = 0
	@inline final val Negative = 0x80000000

	@inline private[this] final val IntKeyMask = 0x7fffffff
	@inline private[this] final val IntSignBit = 0x80000000
	@inline private[this] final val LongKeyMask = 0x7fffffffffffffffL
	@inline private[this] final val LongSignBit = 0x8000000000000000L

	final val RawKeyTypes = new Specializable.Group((Int, Long))
	final val RawValueTypes = new Specializable.Group((Int, Long, Unit))

	trait EntryLens[K, V, @specialized(ItemTypes) +T] {
		def element(node :Node[K, V], sign :Int) :T
	}



	private[maps] class RedBlackIterator[K, V, @specialized(ItemTypes) +T](stack :ArrayBuffer[Node[K, V]], private[this] var sign :Int)
                                                            (lens :EntryLens[K, V, T])
		extends AptIterator[T]
	{

		private[this] var last :Node[K, V] = _

		override def hasNext :Boolean = stack.nonEmpty

		override def head :T = lens.element(stack(stack.length - 1), sign)

		override def next() :T = {
			skip()
			lens.element(last, sign)
		}

		override def skip() :Unit = {
			val depth = stack.length - 1
			var node = stack(depth)
			last = node

			if (node.right == null) {
				stack.remove(depth)
				if (depth == 1 & sign == Negative && { node = stack(0); node.isInstanceOf[RedBlackTree[_, _]]}) {
					sign = 0 //root node, changing the sign
					if (node.colorBit == 0) //the zero key is not really in the tree, skip the root
						skip()
				}
			} else {
				node = node.right
				stack(depth) = node
				while (node.left != null) {
					node = node.left
					stack += node
				}
			}

		}

	}



	private[maps] class ReverseRedBlackIterator[K, V, @specialized(ItemTypes) +T]
	                                           (stack :ArrayBuffer[Node[K, V]], private[this] var sign :Int)
		                                       (lens :EntryLens[K, V, T])
		extends AptIterator[T]
	{

		private[this] var last :Node[K, V] = _

		override def hasNext :Boolean = stack.nonEmpty

		override def head :T = lens.element(stack(stack.length - 1), sign)

		override def next() :T = {
			skip()
			lens.element(last, sign)
		}

		override def skip() :Unit = {
			val depth = stack.length - 1
			var node = stack(depth) //returned node
			if (depth == 0 & sign == Positive & last != null & last.isInstanceOf[RedBlackTree[_, _]])
				sign = Negative
			last = node
			//advance
			if (node.left == null) {
				stack.remove(depth)
				if (depth == 1 & sign == Positive) { //quick check before more expensive one
					node = stack(0)
					if (node.isInstanceOf[maps.RedBlackTree[_, _]] && node.colorBit == 0)
						skip() //the zero key is not rally in the tree, skip the root

				}
			} else {
				node = node.left
				stack(depth) = node
				while (node.right != null) {
					node = node.right
					stack += node
				}
			}
		}

	}






	trait Node[@specialized(RawKeyTypes) K, @specialized(RawValueTypes) V]
		extends BinaryTree[Node[K, V]]
	{
		def key(sign :Int)  :K
		def key_=(k :K) :Unit
		var value :V
		def sure :Sure[V] = Sure(value)

		var color :Color

		/** An alias for the `color` variable for the use by `RedBlackTree` and its descendants, as it uses this
		  * variable for a different purpose and oveerrides the `color` accessors to fix the color to Black. */
		def colorBit :Color = color
		/** An alias for the `color` variable for the use by `RedBlackTree` and its descendants, as it uses this
		 * variable for a different purpose and oveerrides the `color` accessors to fix the color to Black. */
		def colorBit_=(sign :Color) :Unit = color = sign

		def keys :EntryLens[K, V, K]
		def vals :EntryLens[K, V, V]

		def red(key :K, value :V) :Node[K, V]
	}



	object Node {

		private final val SetValues :EntryLens[Any, Unit, Unit] = { (node :Node[Any, Unit], sign :Int)  => () }
		private final val IntValues :EntryLens[Any, Int, Int] = { (node :Node[Any, Int], sign :Int) => node.value }
		private final val LongValues :EntryLens[Any, Long, Long] = { (node :Node[Any, Long], sign :Int) => node.value }
		private final val AnyValues :EntryLens[Any, Any, Any] = { (node :Node[Any, Any], sign :Int) => node.value }
		private[this] final val SureUnit = Sure(())


		trait SetNode[K] extends Node[K, Unit] {
			override def value :Unit = ()
			override def value_=(ignored :Unit) :Unit = ()
			override def sure :Sure[Unit] = SureUnit

			override def vals :EntryLens[K, Unit, Unit] = SetValues.asInstanceOf[EntryLens[K, Unit, Unit]]
		}

		trait IntValueNode[K] extends Node[K, Int] {
			override def vals :EntryLens[K, Int, Int] = IntValues.asInstanceOf[EntryLens[K, Int, Int]]
		}

		trait LongValueNode[K] extends Node[K, Long] {
			override def vals :EntryLens[K, Long, Long] = LongValues.asInstanceOf[EntryLens[K, Long, Long]]
		}

		trait AnyValueNode[K, V] extends Node[K, V] {
			override def vals :EntryLens[K, V, V] = AnyValues.asInstanceOf[EntryLens[K, V, V]]
		}



		private final val IntKeys :EntryLens[Int, _, Int] = { (node :Node[Int, _], sign :Int) => node.key(sign) }

		abstract class IntKeyNode[V](private[this] var k :Int, l :Node[Int, V], r :Node[Int, V])
			extends BinaryTree(l, r) with Node[Int, V]
		{

			override def key(sign :Int) :Int = k & 0x7fffffff | sign

			override def key_=(key :Int) :Unit = k = k & 0x80000000 | key & 0x7fffffff

			override def color :Color = k & 0x80000000

			override def color_=(color :Color) :Unit = k = (k & 0x7fffffff) | color

			override def keys :EntryLens[Int, V, Int] = IntKeys.asInstanceOf[EntryLens[Int, V, Int]]
		}

		class IntSetNode(k :Int, l :Node[Int, Unit] = null, r :Node[Int, Unit] = null)
			extends IntKeyNode[Unit](k, l, r) with Node[Int, Unit] with SetNode[Int]
		{
			override def red(key :Int, value :Unit) = new IntSetNode(key, null, null)
		}

		class IntIntMapNode(k :Int, override final var value :Int, l :Node[Int, Int] = null, r :Node[Int, Int] = null)
			extends IntKeyNode[Int](k, l, r) with Node[Int, Int] with IntValueNode[Int]
		{
			override def red(key :Int, value :Int) :Node[Int, Int] = new IntIntMapNode(key, value, null, null)
		}

		class IntLongMapNode(k :Int, override final var value :Long, l :Node[Int, Long] = null, r :Node[Int, Long] = null)
			extends IntKeyNode[Long](k, l, r) with Node[Int, Long] with LongValueNode[Int]
		{
			override def red(key :Int, value :Long) :Node[Int, Long] = new IntLongMapNode(key, value, null, null)
		}

		class IntAnyMapNode[V](k :Int, override final var value :V, l :Node[Int, V] = null, r :Node[Int, V] = null)
			extends IntKeyNode[V](k, l, r) with Node[Int, V] with AnyValueNode[Int, V]
		{
			override def red(key :Int, value :V) :Node[Int, V] = new IntAnyMapNode(key, value, null, null)
		}



		private final val LongKeys :EntryLens[Long, _, Long] = { (node :Node[Long, _], sign :Int) => node.key(sign) }

		abstract class LongKeyNode[@specialized(RawValueTypes) V](private[this] var k :Long, l :Node[Long, V], r :Node[Long, V])
			extends BinaryTree(l, r) with Node[Long, V]
		{

			override def key(sign :Int) :Long = k & LongKeyMask | (sign.toLong << 32)

			override def key_=(key :Long) :Unit = k = k & LongSignBit | key & LongKeyMask

			override def color :Color = ((k & LongSignBit) >> 32).toInt

			override def color_=(color :Color) :Unit = k = (k & LongKeyMask) | (color.toLong << 32)

			override def keys :EntryLens[Long, V, Long] = LongKeys.asInstanceOf[EntryLens[Long, V, Long]]
		}

		class LongSetNode(k :Long, l :Node[Long, Unit] = null, r :Node[Long, Unit] = null)
			extends LongKeyNode[Unit](k, l, r) with Node[Long, Unit] with SetNode[Long]
		{
			override def red(key :Long, value :Unit) :Node[Long, Unit] = new LongSetNode(key, null, null)
		}

		class LongIntMapNode(k :Long, override final var value :Int, l :Node[Long, Int] = null, r :Node[Long, Int] = null)
			extends LongKeyNode[Int](k, l, r) with Node[Long, Int] with IntValueNode[Long]
		{
			override def red(key :Long, value :Int) :Node[Long, Int] = new LongIntMapNode(key, value, null, null)
		}

		class LongLongMapNode(k :Long, override final var value :Long, l :Node[Long, Long] = null, r :Node[Long, Long] = null)
			extends LongKeyNode[Long](k, l, r) with Node[Long, Long] with LongValueNode[Long]
		{
			override def red(key :Long, value :Long) :Node[Long, Long] = new LongLongMapNode(key, value, null, null)
		}

		class LongAnyMapNode[V](k :Long, override final var value :V, l :Node[Long, V] = null, r :Node[Long, V] = null)
			extends LongKeyNode[V](k, l, r) with Node[Long, V] with AnyValueNode[Long, V]
		{
			override def red(key :Long, value :V) :Node[Long, V] = new LongAnyMapNode(key, value, null, null)
		}



		private final val AnyKeys :EntryLens[Any, Any, Any] = { (node :Node[_, _], sign :Int) => node.key(sign) }

		abstract class AnyKeyNode[K, V]
				(final var key :K, override var color :Color, l :Node[K, V] = null, r :Node[K, V] = null)
			extends BinaryTree(l, r) with Node[K, V]
		{
			override def key(sign :Int) :K = key

			override def keys :EntryLens[K, V, K] = AnyKeys.asInstanceOf[EntryLens[K, V, K]]
		}

		class AnySetNode[K](k :K, color :Color, l :Node[K, Unit] = null, r :Node[K, Unit] = null)
			extends AnyKeyNode[K, Unit](k, color, l, r) with Node[K, Unit] with SetNode[K]
		{
			override def red(key :K, value :Unit) :Node[K, Unit] = new AnySetNode(key, Red, null, null)
		}

		class AnyAnyMapNode[K, V](k :K, override final var value :V, color :Color, l :Node[K, V] = null, r :Node[K, V] = null)
			extends AnyKeyNode[K, V](k, color, l, r) with Node[K, V] with AnyValueNode[K, V]
		{
			override def red(key :K, value :V) :Node[K, V] = new AnyAnyMapNode(key, value, Red, null, null)
		}


	}
}
