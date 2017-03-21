package net.turambar.palimpsest.specialty.maps

import net.turambar.palimpsest.specialty.Elements
import net.turambar.palimpsest.specialty.maps.LongTrie.Leaf
import net.turambar.palimpsest.specialty.ordered.OrderedAs

import scala.annotation.meta.{field, getter}
import scala.annotation.tailrec



/**
  * @author Marcin Mościcki
  */
sealed trait LongTrie[+V, +Trie] {
	/** A `Long` value consisting of the longest common prefix (from the highest bit) to all keys in this set. */
	def prefix :Long

	def key :Long

	def value :V = throw new UnsupportedOperationException(s"Generic trie $stringPrefix doesn't have an associated value.")

	/** Number of keys in this collection. */
	def size :Int

	/** Is this key the value of a leaf in this trie. */
	def hasKey(key :Long) :Boolean

	/** Can `key`, given the invariants of this trie, be an element of this collection?.
	  * Empty collections return `false`, leaf nodes simply compare it with the stored value,
	  * while inner-nodes check if the element's binary representation conforms to the shared path to this node.
	  */
	def belongs(key :Long) :Boolean

	/** Find the leaf corresponding to this `key` in this collection.
	  * @param key value to find in this collection
	  * @return either a [[LongTrie.Leaf]] holding the given key, if present, or [[LongTrie.EmptyTrie]]
	  *         if the given key is not found, including empty tries as a rule.
	  */
	def leafFor(key :Long) :Trie


	/** Add the given leaf element to this collection. */
	protected[this] def addIfAbsent(leaf :Leaf[V, Trie]) :Trie
	protected[this] def replace(leaf :Leaf[V, Trie]) :Trie
	protected[this] def add(leaf :Leaf[V, Trie]) :Trie
	protected[this] def addOrUpdate(leaf :Leaf[V, Trie])(update :Leaf[V, Trie] => Trie) :Trie
	protected[this] def modify(key :Long)(swap :Trie=>Trie) :Trie

	/** Remove any leaf associated with the given key from this collection. */
	def delete(key :Long) :Trie

	def empty :Trie
	def isEmpty :Boolean = false


/*
	protected[this] def branch(key1 :Long, s1 :Trie, key2 :Long, s2 :Trie) :Trie = {
		var diff = key1 ^ key2
		diff |= (diff >>  1)
		diff |= (diff >>  2)
		diff |= (diff >>  4)
		diff |= (diff >>  8)
		diff |= (diff >> 16)
		diff |= (diff >> 32)
		val prefixMask = ~diff //mask for the longest prefix where s1 equals s2 (from the highest bit)
		val firstDiffBit = diff - (diff >>> 1) //mask for the single first (highest) bit where s1 and s2 differ
		if ((key1 & firstDiffBit)==0)
			branch(prefixMask | firstDiffBit, s1, s2)
		else
			branch(prefixMask | firstDiffBit, s2, s1)
	}
*/


	protected[this] def branch(common :Long, left :Trie, right :Trie) :Trie //=

	def stringPrefix :String = "LongTrie"
}






object LongTrie {
	import java.lang.Long.MIN_VALUE

	trait TriePath[V, Trie] {
		def insertLeaf(key :Long) :Trie
		def updateLeaf(oldLeaf :Leaf[V, Trie]) :Trie
//		def updateLeaf[O>:V, S>:Trie](oldLeaf :Leaf[O, S]) :Leaf[V, Trie]
//		def applyPath[O>:V, T](mutant :Leaf[V, Trie] => T) :T
	}


	sealed trait StableTrie[+V, +Trie<:LongTrie[V, Trie]] extends LongTrie[V, Trie]

//	sealed trait Leaf[+V, +Trie] extends StableTrie[V, Trie] {
//		def key :Long
//		def value :V
//	}

	trait EmptyTrie[+Trie] extends LongTrie[Nothing, Trie] { this :Trie =>
		override def empty = this
		override def isEmpty = true
		def size = 0
		def prefix = MIN_VALUE
		def key = 0L
		override def value = throw new NoSuchElementException("LongTrie.Empty")

		def leafFor(key :Long) = empty
		def hasKey(key :Long) = false
		def belongs(key :Long) = false

		override protected[this] def addIfAbsent(leaf: Leaf[Nothing, Trie]): Trie = add(leaf)
		override protected[this] def replace(leaf: Leaf[Nothing, Trie]): Trie = empty
		override protected[this] def add(leaf: Leaf[Nothing, Trie]): Trie = leaf.asTrie
		override protected[this] def addOrUpdate(leaf: Leaf[Nothing, Trie])(merge: Leaf[Nothing, Trie] => Trie): Trie = leaf.asTrie
		override protected[this] def modify(key: Long)(swap: (Trie) => Trie): Trie = swap(this)

		override def delete(key: Long): Trie = this
	}

//	final val Empty = new EmptyTrie {}

	abstract class Leaf[+V, +Trie](elem :Long) extends LongTrie[V, Trie] { this :Trie =>
		@inline final def asTrie :Trie = this

		final def size = 1
		@inline final def prefix = elem
		@inline final def key = elem

		def hasKey(key :Long) = key == elem
		def belongs(key :Long) = key == elem

		override def leafFor(key: Long): Trie =
			if (key==elem) this else empty

		override def delete(key: Long): Trie =
			if (key == elem) empty else this

		override protected[this] def addIfAbsent(leaf: Leaf[V, Trie]): Trie =
			if (elem==leaf.key) asTrie
			else join(leaf)

		override protected[this] def replace(leaf: Leaf[V, Trie]): Trie =
			if (elem==leaf.key) leaf.asTrie
			else asTrie //branch(key, asTrie, leaf.key, asTrie)

		override protected[this] def add(leaf: Leaf[V, Trie]): Trie =
			if (elem==leaf.key) leaf.asTrie
			else join(leaf)


		override protected[this] def addOrUpdate(leaf: Leaf[V, Trie])(update: (Leaf[V, Trie]) => Trie): Trie =
			if (elem == leaf.key) update(this)
			else join(leaf)

		override protected[this] def modify(key: Long)(swap: (Trie) => Trie): Trie =
			if (elem == key) swap(this)
			else swap(empty) match {
				case other :Leaf[V, Trie] => join(other)
				case _ :EmptyTrie[Trie] => this
			}

		@inline protected[this] final def join(other :Leaf[V, Trie]) :Trie = {
			val prefix = delimitedPrefix(elem, other.key)
			if ((prefix & elem)==prefix)
				branch(prefix, other.asTrie, asTrie)
			else
				branch(prefix, asTrie, other.asTrie)
		}

	}

//	abstract class SingletonLongSet[+Trie](k :Long) extends Leaf[Unit, Trie](k) { this :Trie =>
//		@inline final override def value = ()
//	}
//
//	abstract class SingletonLongMap[@specialized(Elements) +V, +Trie](k :Long, private[this] var v :V) extends Leaf[V, Trie](k) {
//		this :Trie =>
//		@inline final override def value = v
//		@inline protected[this] final def value_=(newValue :V) :Unit = v = newValue
//	}


/*
	def Branch[V](s1 :LongTrie[V], s2 :LongTrie[V]) :Branch[V] = {
		val key1 = s1.key
		var diff = key1 ^ s2.key
		diff |= (diff >>  1)
		diff |= (diff >>  2)
		diff |= (diff >>  4)
		diff |= (diff >>  8)
		diff |= (diff >> 16)
		diff |= (diff >> 32)
		val prefixMask = ~diff //mask for the longest prefix where s1 equals s2 (from the highest bit)
		val firstDiffBit = diff - (diff >>> 1) //mask for the single first (highest) bit where s1 and s2 differ
		if ((key1 & firstDiffBit)==0)
			new Branch(prefixMask | firstDiffBit, s1, s2)
		else
			new Branch(prefixMask | firstDiffBit, s2, s1)
	}
*/




	//class Branch[+V](@(inline @getter) final val path :Long, @(inline @getter) final val left :LongTrie[V], @(inline @getter) final val right :LongTrie[V], @(inline @getter) final val leaves :Int)
	abstract class BranchTrie[+V, +Trie<:LongTrie[V, Trie]](private[this] var _path :Long, private[this] var _left :Trie, private[this] var _right :Trie, private[this] var leaves :Int)
		extends LongTrie[V, Trie]
	{
		private[maps] def this(path :Long, left :Trie, right :Trie) = this(path, left, right, left.size + right.size)

		def asTrie :Trie = this.asInstanceOf[Trie]

		@inline final def path = _path
		@inline final def left = _left
		@inline final def right = _right
		@inline final def size = leaves
		@inline final protected[this] def path_=(delimitedPrefix :Long) :Unit = _path = delimitedPrefix
		@inline final protected[this] def left_=(trie :Trie) :Unit = _left = trie
		@inline final protected[this] def right_=(trie :Trie) :Unit = _right = trie
		@inline final protected[this] def size_=(leafCount :Int) :Unit = leaves = leafCount

		def key = path


		def prefix = path & (path-1) //clears lowest set bit

		/** Mask for bits common to all elements in this set, formed by a sequence of any number of `1` bits starting from the highest bit,
		  * followed by zero bits in the remaining lower positions.
		  * @return value such that `(mask & k)==path` for all keys `k` in this set (including inner nodes)
		  */
		def mask = { val bit = path & -path; -bit ^ bit }

		/** Mask for the single bit separating the left and right subtrees. `left` is assumed to contain
		  * all values for which this bit is clear, while `right` to contain all values with this bit set.
		  */
		def diffBit = path & -path //mask for the single lowest set bit

		/** Value used to determine in which subtree of this set the given element belongs.
		  *
		  * @param elem any value to potentially add or remove from this set.
		  * @return `0` if the element belongs to the `right` tree,
		  *        [[diffBit]] if the element belongs to the `left` tree,
		  *        and any other value if the element doesn't share the common path of this set.
		  */
		protected[this] def assign(elem :Long) = {
			//left-hand xor operand is equivalent to a xor of elem and our path, clearing any bits lower than diffBit (lowest set bit in common),
			//but has the diffBit always set, as elem | -elem is always 1, and both -common and common have diffBit set.
			//right-hand xor operand clears the diffBit if it is set in elem.
			val wrongBitsSet = elem & -path
			(wrongBitsSet | -elem & path) ^ (wrongBitsSet & path)
		}


		override def belongs(key :Long) = (key & -path | ~key & path) == (path & -path) //(key & mask) == path

		override def hasKey(elem: Long): Boolean = {
			@tailrec def isIn(trie :LongTrie[V, Trie]) :Boolean = trie match {
				case b :BranchTrie[V, Trie] =>
					val p = b.path; val DiffBit = p & -p
					(elem & -p | -elem & p) ^ (elem & DiffBit) match {
						case 0 => isIn(b.right)
						case DiffBit => isIn(b.left)
						case _ => false
					}
				case s :Leaf[V, Trie] => s.key == elem
				case _ :EmptyTrie[Trie] => false //should never happen unless subclasses tamper with the structure of the trie
			}
			isIn(this)
		}

		override def leafFor(elem: Long): Trie = {
			@tailrec def locate(trie :LongTrie[V, Trie]) :Trie = trie match {
				case b :BranchTrie[V, Trie] =>
					val p = b.path; val DiffBit = p & -p
					(elem & -p | -elem & p) ^ (elem & DiffBit) match {
						case 0 => locate(b.right)
						case DiffBit => locate(b.left)
						case _ => empty
					}
				case leaf :Leaf[V, Trie] => leaf.asTrie
				case _ :EmptyTrie[Trie] => empty
			}
			locate(this)
		}




		/** Remove any leaf associated with the given key from this collection. */
		override final def delete(victim: Long): Trie = {
			def searchAndKill(trie :LongTrie[V, Trie]) :Trie = trie match {
				case b :BranchTrie[V, Trie] =>
					val p = b.path; val DiffBit = p & -p
					(victim & -p | -victim & p) ^ (victim & DiffBit) match {
						case 0 =>
							val r = searchAndKill(b.right)
							if (r eq right) b.asTrie
							else if (r.isEmpty) b.left
							else branch(b.path, b.left, r)

						case DiffBit =>
							val l = searchAndKill(b.left)
							if (l eq left) asTrie
							else if (l.isEmpty) b.right
							else branch(b.path, l, b.right)

						case _ => b.asTrie
					}
				case leaf :Leaf[V, Trie] =>
					if (leaf.key==victim) empty else leaf.asTrie

				case e :EmptyTrie[Trie] => empty
			}
			searchAndKill(this)

		}


		override protected[this] def addOrUpdate(leaf: Leaf[V, Trie])(update: (Leaf[V, Trie]) => Trie): Trie = {
			def searchAndUpdate(trie :LongTrie[V, Trie]) :Trie = trie match {
				case b :BranchTrie[V, Trie] =>
					val p = b.path; val DiffBit = p & -p; val k = leaf.key
					(k & -p | -k & p) ^ (k & DiffBit) match {
						case 0 =>
							val r = searchAndUpdate(b.right)
							if (r eq b.right) b.asTrie
							else branch(b.path, b.left, r)

						case DiffBit =>
							val l = searchAndUpdate(b.left)
							if (l eq b.left) b.asTrie
							else branch(b.path, l, b.right)

						case _ => branch(k, leaf.asTrie, p, b.asTrie)
					}
				case l :Leaf[V, Trie] =>
					val oldKey = l.key; val newKey = leaf.key
					if (oldKey==newKey) update(leaf)
					else branch(oldKey, l.asTrie, newKey, leaf.asTrie)

				case _ :EmptyTrie[Trie] => leaf.asTrie

			}
			searchAndUpdate(this)
		}

		override protected[this] def addIfAbsent(leaf: Leaf[V, Trie]): Trie = {
			def searchAndPlace(trie :LongTrie[V, Trie]) :Trie = trie match {
				case b :BranchTrie[V, Trie] =>
					val p = b.path; val DiffBit = p & -p; val k = leaf.key
					(k & -p | -k & p) ^ (k & DiffBit) match {
						case 0 =>
							val r = searchAndPlace(b.right)
							if (r eq b.right) b.asTrie
							else branch(b.path, b.left, r)

						case DiffBit =>
							val l = searchAndPlace(b.left)
							if (l eq b.left) b.asTrie
							else branch(b.path, l, b.right)

						case _ => branch(k, leaf.asTrie, p, b.asTrie)
					}

				case l :Leaf[V, Trie] =>
					val oldKey = l.key; val newKey = leaf.key
					if (oldKey == newKey) l.asTrie
					else branch(l.asTrie, leaf.asTrie)

				case e :EmptyTrie[Trie] => leaf.asTrie
			}
			searchAndPlace(this)
		}


		override protected[this] def add(leaf: Leaf[V, Trie]): Trie = addOrUpdate(leaf)(_.asTrie)

		override protected[this] def replace(leaf: Leaf[V, Trie]): Trie = addOrUpdate(leaf)(_ => leaf.asTrie)






		@inline protected[this] final def filtered(l :Trie, r :Trie) :Trie =
			if (l.isEmpty) r
			else if (r.isEmpty) l
			else copy(l, r)

		protected[this] def copy(l :Trie, r :Trie) :Trie = branch(path, l, r)

		@inline final protected[this] def branch(s1 :Trie, s2 :Trie) :Trie =
			branch(s1.key, s1, s2.key, s2)

		@inline final protected[this] def branch(key1 :Long, trie1 :Trie, key2 :Long, trie2 :Trie) :Trie = {
			val prefix = delimitedPrefix(key1, key2)
			if ((prefix & key2)==prefix)
				branch(prefix, trie1, trie2)
			else
				branch(prefix, trie2, trie1)
		}

		protected[this] def branch(common :Long, left :Trie, right :Trie) :Trie
/*
		protected[this] def branch(s1 :Trie, s2 :Trie) :Trie = {
			val key1 = s1.key
			var diff = key1 ^ s2.key
			diff |= (diff >>  1)
			diff |= (diff >>  2)
			diff |= (diff >>  4)
			diff |= (diff >>  8)
			diff |= (diff >> 16)
			diff |= (diff >> 32)
			val prefixMask = ~diff //mask for the longest prefix where s1 equals s2 (from the highest bit)
			val firstDiffBit = diff - (diff >>> 1) //mask for the single first (highest) bit where s1 and s2 differ
			if ((key1 & firstDiffBit)==0)
				branch(prefixMask | firstDiffBit, s1, s2)
			else
				branch(prefixMask | firstDiffBit, s2, s1)
		}


		protected[this] def branch(common :Long, left :Trie, right :Trie) :Trie //=
*/
//			new Branch(common, left, right)
	}

//	abstract class StableBranchLike[+V, +Trie<:StableTrie[V]]()








	/** Returns a `Long` which in the highest bits contains the longest common prefix
	  * of `path1` and `path2`, followed by a single set bit on the highest position
	  * where the values differ.
	  * If `path1==path2`, then their common value is returned.
	  * If their lowest bit i `1`, then this result is indistinguishable
	  * with the case where both values differ only on the lowest bit.
	  * Fot this reason the caller should assertain that `path1!=path2` before
	  * calling this method.
	  */
	@inline final private[palimpsest] def delimitedPrefix(path1 :Long, path2 :Long) :Long = {
		var diff = path1 ^ path2
		diff |= (diff >>  1)
		diff |= (diff >>  2)
		diff |= (diff >>  4)
		diff |= (diff >>  8)
		diff |= (diff >> 16)
		diff |= (diff >> 32)
		val prefixMask = ~diff //mask for the longest prefix where s1 equals s2 (from the highest bit)
		val firstDiffBit = diff - (diff >>> 1) //mask for the single first (highest) bit where s1 and s2 differ
		path1 & prefixMask | firstDiffBit
	}

}

