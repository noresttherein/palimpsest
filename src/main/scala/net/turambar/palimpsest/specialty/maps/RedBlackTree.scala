package net.turambar.palimpsest.specialty.maps

import java.lang.Double.{doubleToLongBits, longBitsToDouble}
import java.lang.Float.{floatToIntBits, intBitsToFloat}

import scala.annotation.tailrec

import net.turambar.palimpsest.specialty.{?, Blank, Sure}
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.maps.RedBlackTree.{KeysIterator, Negative, Node, Positive}
import net.turambar.palimpsest.specialty.ordered.{OrderedBy, ValOrdering}
import scala.collection.mutable.ArrayBuffer


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
  * this doesn't even extend the length of the path to the node measured in the number of accessed object.
  * In the end, the only cost of obtaining the most compact representation possible, is slightly slower key comparison
  * caused by the need for masking the sign/colour bit each time (and optionally Int-to-Float and Long-to-Double cast);
  * this is negligible in the presence of already incurred key getter method cost.
  *
  * @author Marcin Mościcki
  */
trait RedBlackTree[@specialized(RawKeyTypes) K, @specialized(RawValueTypes) V, This] /*extends (This OrderedBy K)*/ {
	root :Node[K, V] =>

//	protected implicit def ordering :ValOrdering[K]
	protected def compareRaw(k1 :K, k2 :K) :Int

	/** The number of keys in this tree. */
	private[maps] var keyCount :Int
	private[maps] def size_++() :Unit = keyCount = keyCount + 1
	private[maps] def size_--() :Unit = keyCount = keyCount - 1

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
		
	
/*
	override def keyAt(n :Int) :K = {
		val i = keysIterator.drop(n)
		if (!i.hasNext)
			throw new IndexOutOfBoundsException(this + ".keyAt(" + n + ")")
		i.next()
	}
*/


	def containsRaw(key :K) :Boolean = {
		var k = this.key(0)
		var cmp = compareRaw(k, key)
		if (cmp == 0) //key == 0
			color != 0 
		else {
			var sign = 0
			var node =
				if (cmp > 0) {
					right
				} else {
					sign = Negative; left
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



	protected def rawKeysIterator :FitIterator[K] = {
		//the stack will contain all the nodes on the path to the smallest key
		val stack = new ArrayBuffer[Node[K, _]]
		val sign =
			if (left != null) {
				stack += this
				var top = left
				while (top.left != null) {
					top = top.left
					stack += top
				}
				Negative
			} else if (color != 0) { //zero (this.key) is in the tree and no negative elements.
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
		new KeysIterator(stack, sign)
	}



	def rawKeysIteratorFrom(start :K) :FitIterator[K] = {
		//the stack will contain all nodes on the path to the smallest key larger than start from which we descended left.
		val stack = new ArrayBuffer[Node[K, _]]
		var k = key(0)
		var cmp = compareRaw(start, k)

		/** Recursively go down the tree searching for the first key larger than start. */
		def descend(node :Node[K, V], sign :Int, stack :ArrayBuffer[Node[K, _]]) :FitIterator[K] = {
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
			new KeysIterator[K](stack, sign)
		}

		if (cmp < 0 && left != null) { //first iterator key is in the left subtree
			stack += this
			descend(left, Negative, stack)
		} else if (cmp <= 0 && color != 0) { //start <= 0 and zero key is present, hence we are the first node.
			stack += this
			new KeysIterator[K](stack, Positive)
		} else { //start > 0 || start >= 0 && !contains(0) ==> the first iterator node is in the right subtree
			descend(right, Positive, stack)
		}
	}



	def insert(key :K, value :V): ?[V] = {
		val cmp = compareRaw(key, root.key(0))
		if (cmp < 0)
			insertLeft(key, value, Negative)(root)
		else if (cmp > 0)
			insertRight(key, value, Positive)(root)
		else //key == 0
			if (color == 0) { //zero key is not present, add it to the tree
				this.value = value
				size_++()
				Blank
			} else { //zero already present, just swap the value
				val res = Sure(this.value)
				this.value = value
				res
			}
	}



	def remove(key :K): ?[V] = {
		val cmp = compareRaw(key, this.key(0))
		if (cmp < 0)
			removeLeft(key, Negative)(root)
		else if (cmp > 0)
			removeRight(key, Positive)(root)
		else //key == 0
			if (color == 0) //zero key is not part of the tree
				Blank
			else {
				color = 0
				size_--()
				Sure(value)
			}
	}



	/** Recursively descend down the left subtree of `node` in search for the place to insert key -> value.
	  * @param key the key associated with the inserted value
	  * @param value the inserted value
	  * @param sign a mask for the sign (highest) bit shared by all elements in the subtree of `parent`
	  * @param parent the node of which the left child is the current node
	  */
	private def insertLeft(key :K, value :V, sign :Int)(parent :Node[K, V]): ?[V] = {
		var node = parent.left
		if  (node == null) { //key wasn't present, create a new node under parent with key -> value
			node = leaf(key, value)
			parent.left = node
			//todo: rebalance
			root.size_++()
			Blank
		} else {
			val k = node.key(sign)
			val cmp = compareRaw(key, k)
			if (cmp < 0)
				insertLeft(key, value, sign)(node)
			else if (cmp > 0)
				insertRight(key, value, sign)(node)
			else { //the key already present, just swap the value
				val prev = Sure(node.value)
				node.value = value
				prev
			} 
		}
	}


	/** Recursively descend down the right subtree of `node` in search for the place to insert key -> value.
	  * @param key the key associated with the inserted value
	  * @param value the inserted value
	  * @param sign a mask for the sign (highest) bit shared by all elements in the subtree of parent`
	  * @param parent the node of which the right child is the current node
	  */
	private def insertRight(key :K, value :V, sign :Int)(parent :Node[K, V]): ?[V] = {
		var node = parent.right
		if  (node == null) {
			node = leaf(key, value)
			parent.right = node
			//todo: rebalance
			root.size_++()
			Blank
		} else {
			val k = node.key(sign)
			val cmp = compareRaw(key, k)
			if (cmp < 0)
				insertLeft(key, value, sign)(node)
			else if (cmp > 0)
				insertRight(key, value, sign)(node)
			else { //the key is already present, just swap the value
				val prev = Sure(node.value)
				node.value = value
				prev
			} 
		}
	}



	private def removeLeft(key :K, sign :Int)(parent :Node[K, V]): ?[V] = {
		val node = parent.left
		if (node == null) //the key was not present in this tree
			Blank
		else {
			val k = node.key(sign)
			val cmp = compareRaw(key, k)
			if (cmp < 0)
				removeLeft(key, sign)(node)
			else if (cmp > 0)
				removeRight(key, sign)(node)
			else { //the key is present in the tree, remove the current node
				root.size_--()
				val l = node.left; var r = node.right
				if (l == null) {
					if (r == null) {
						parent.left = null
					} else {
						parent.left = r
					}
					//todo: rebalance
					Sure(node.value)
				} else if (r == null) {
					parent.left = l
					//todo: rebalance
					Sure(node.value)
				} else { //node has both children, swap the key with its predecessor
					val res = Sure(node.value)
					while (r.left != null) {
						r = r.left
					}
					node.key_=(r.key(sign))
					node.value = r.value
					res
				}
			} 
		}
	}



	private def removeRight(key :K, sign :Int)(parent :Node[K, V]): ?[V] = {
		val node = parent.right
		if (node == null)
			Blank
		else {
			val k = node.key(sign)
			val cmp = compareRaw(key, k)
			if (cmp < 0)
				removeLeft(key, sign)(node)
			else if (cmp > 0)
				removeRight(key, sign)(node)
			else { //the key is present in the tree, remove the current node
				var l = node.left; val r = node.right
				if (l == null) {
					if (r == null) {
						parent.right = null
					} else {
						parent.right = r
					}
					//todo :rebalance
					Sure(node.value)
				} else if (r == null) {
					parent.right = l
					//todo: rebalance
					Sure(node.value)
				} else { //node has both children, swap its key with the successor node
					val res = Sure(node.value)
					while (l.right != null) {
						l = l.right
					}
					node.key_=(l.key(sign))
					node.value = l.value
					//todo: rebalance
					res
				}
			} 
		}
	}
	
	protected def leaf(key :K, value :V) :Node[K, V]

}






object RedBlackTree {
	type Color = Int
	@inline final val Black = 0
	@inline final val Red  = 0x80000000

	@inline private final val Positive = 0
	@inline private final val Negative = 0x80000000

	@inline private[this] final val IntKeyMask = 0x7fffffff
	@inline private[this] final val IntSignBit = 0x80000000
	@inline private[this] final val LongKeyMask = 0x7fffffffffffffffL
	@inline private[this] final val LongSignBit = 0x8000000000000000L

	final val RawKeyTypes = new Specializable.Group((Int, Long))
	final val RawValueTypes = new Specializable.Group((Int, Long, Unit))



	trait Node[@specialized(RawKeyTypes) K, @specialized(RawValueTypes) V]
		extends MutableBSTNode[Node[K, V]] //with ValOrdering[K]
	{
		def key(sign :Int)  :K
		def key_=(k :K) :Unit
		var value :V
		var color :Color
	}
	
	trait SetNode[K] extends Node[K, Unit] {
		override def value :Unit = ()
		override def value_=(ignored :Unit) :Unit = ()
	}


	abstract class IntKeyNode[V](private[this] var k :Int, l :Node[Int, V], r :Node[Int, V]) extends Node[Int, V] {
		left = l
		right = r 
		
		override def key(sign :Int) :Int = k & 0x7fffffff | sign
		override def key_=(key :Int) :Unit = k = k & 0x80000000 | key & 0x7fffffff

		override def color :Color = k & 0x80000000
		override def color_=(color :Color) :Unit = k = (k & 0x7fffffff) | color
	}
	
	class IntSetNode(k :Int, l :Node[Int, Unit] = null, r :Node[Int, Unit] = null) 
		extends IntKeyNode[Unit](k, l, r) with SetNode[Int]

	class IntIntMapNode(k :Int, override final var value :Int, l :Node[Int, Int] = null, r :Node[Int, Int] = null) 
		extends IntKeyNode[Int](k, l, r) with Node[Int, Int]

	class IntLongMapNode(k :Int, override final var value :Long, l :Node[Int, Long] = null, r :Node[Int, Long] = null)
		extends IntKeyNode[Long](k, l, r) with Node[Int, Long]

	class IntAnyMapNode[V](k :Int, override final var value :V, l :Node[Int, V] = null, r :Node[Int, V] = null)
		extends IntKeyNode[V](k, l, r) with Node[Int, V]


	abstract class LongKeyNode[V](private[this] var k :Long, l :Node[Long, V], r :Node[Long, V]) extends Node[Long, V] {
		left = l
		right = r

		override def key(sign :Int) :Long = k & LongKeyMask | (sign.toLong << 32)
		override def key_=(key :Long) :Unit = k = k & LongSignBit | key & LongKeyMask

		override def color :Color = ((k & LongSignBit) >> 32).toInt
		override def color_=(color :Color) :Unit = k = (k & LongKeyMask) | (color.toLong << 32)
	}

	class LongSetNode(k :Long, l :Node[Long, Unit] = null, r :Node[Long, Unit] = null)
		extends LongKeyNode[Unit](k, l, r) with SetNode[Long]

	class LongIntMapNode(k :Long, override final var value :Int, l :Node[Long, Int] = null, r :Node[Long, Int] = null)
		extends LongKeyNode[Int](k, l, r) with Node[Long, Int]

	class LongLongMapNode(k :Long, override final var value :Long, l :Node[Long, Long] = null, r :Node[Long, Long] = null)
		extends LongKeyNode[Long](k, l, r) with Node[Long, Long]

	class LongAnyMapNode[V](k :Long, override final var value :V, l :Node[Long, V] = null, r :Node[Long, V] = null)
		extends LongKeyNode[V](k, l, r) with Node[Long, V]
	


	abstract class AnyKeyNode[K, V](final var key :K, override final var color :Color, l :Node[K, V] = null, r :Node[K, V] = null)
		extends Node[K, V]
	{
		left = l
		right = r

		override def key(sign :Int) :K = key
	}

	class AnySetNode[K](k :K, color :Color, l :Node[K, Unit] = null, r :Node[K, Unit] = null)
		extends AnyKeyNode[K, Unit](k, color, l, r) with SetNode[K]

	class AnyAnyMapNode[K, V](k :K, override final var value :V, color :Color, l :Node[K, V] = null, r :Node[K, V] = null)
		extends AnyKeyNode[K, V](k, color, l, r)






	private class KeysIterator[K](stack :ArrayBuffer[Node[K, _]], private[this] var sign :Int) extends FitIterator[K] {

		override def hasNext :Boolean = stack.nonEmpty

		override def head :K = stack(stack.length - 1).key(sign)

		override def next() :K = {
			val depth = stack.length - 1
			var top = stack(depth)
			val res = top.key(sign)
			if (top.right == null) {
				stack.remove(depth)
				if (depth == 1 && sign != 0 && stack(0).color == 0) {
					sign = 0
					next() //skip the 'zero' key from the root if its not in the tree
				}
			} else {
				top = top.right
				stack(depth) = top.right
				while (top.left != null) {
					top = top.left
					stack += top
				}
			}
			res
		}

		override def skip() :Unit = next()

	}

}
