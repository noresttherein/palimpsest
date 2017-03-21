package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.LibraryError
import net.turambar.palimpsest.specialty.ordered.OrderedAs
import net.turambar.palimpsest.specialty.ordered.OrderedAs.EmptyOrderedTemplate
import net.turambar.palimpsest.specialty.sets.StableLongTrieSet
import net.turambar.palimpsest.specialty.tries.BinaryTrie.MutableTrieBranch
import net.turambar.palimpsest.specialty.tries.EmptyTrie.AbstractEmptyTrie
import net.turambar.palimpsest.specialty.tries.Trie.{DeleteTrie, LeafIterator, LeafKeyIterator, MutableTrieRoot, TrieCombinator, TrieOperator, TriePatch}
import net.turambar.palimpsest.specialty.tries.TrieLeaf.{AbstractTrieLeaf, SingletonLeaf}
import net.turambar.palimpsest.specialty.{FitIterator, IterableTemplate}

import scala.annotation.tailrec
import scala.collection.mutable



/** A binary (that is with all inner nodes having arity of `2`) [[Trie]] implementation with `Long` values
  * as keys, using their binary representation as the paths in the tree. Any node in the trie is associated
  * with a prefix of a `Long` value, that is a `Long` which `n` highest bit have specific values, while `64-n`
  * lower bits are set to zero and understood as 'undetermined'. Those prefixes are exposed as [[LongPrefixTrie#prefix]]
  * and are referred to as 'paths' in the trie, as for any two nodes, one is an ancestor of another iff its path
  * is a proper prefix of the path of the descendant. Also, key equality between two node references from a single trie
  * imply that they are the same node instance, so any descendant must extend the length of the common prefix.
  * Also, leaves are always associated with 'full' key values rather than key prefixes (that is, `n==64`), and
  * any branch instances ([[net.turambar.palimpsest.specialty.tries.LongPrefixTrie.LongTrieBranch]])
  * are assumed to always represent inner nodes (associated with a proper prefix of a key
  * and not its full extent). It follows then, that for any trie/subtrie root, the partial key associated with it is
  * a prefix of all keys present in this trie (as leave), and any key which starts with this prefix, if present in a trie,
  * must be a leaf descendant of that node.
  *
  * @author Marcin Mościcki
  */
trait LongPrefixTrie[+V, +This <: AnyRef] extends Trie[Long, V, This] with mutable.Cloneable[This] {
//	override implicit def ordering: Ordering[Long] = LongPrefixTrie.ordering

	/** The path in the larger trie to this node. In its higher bits, it contains the longest
	  * common prefix to all leaves in this trie. Any remaining lower bits are clear.
	  */
	def prefix :Long

	/** Mask for `n` highest bits in a key, where `n` is the length of the prefix forming the path to this node.
	  * In other words, for all leaves in this trie, `l.key & this.mask == this.prefix`.
	  * @return a mask with `1` bits on all relevant positions of `this.prefix`.
	  */
	def mask :Long

	/** The lower bound (inclusive) for all keys under this trie. Consists of the longest common
	  * prefix in the highest bits, followed by zeros - as such it is equal to `prefix`.
	  */
	def extremeLeft :Long = prefix

	/** The upper bound (inclusive) for all keys under this trie. Consists of the longest common
	  * prefix in the highest bits (equal to [[LongPrefixTrie#prefix]]), followed by `1` bits in any remaining
	  * lower positions.
	  */
	def extremeRight :Long

	@inline final private[palimpsest] def hasKey(key :Long) = hasLeaf(key)
	@inline final private[palimpsest] def select(key :Long) = leafFor(key)

	def leaf(n :Int) :This

	/** Analogue of [[net.turambar.palimpsest.specialty.sets.OrderedSet#from]] working on keys in this trie. Provides a basis for subclasses to implement
	  * [[net.turambar.palimpsest.specialty.ordered.OrderedVals]], but doesn't extend the trait itself here in order to allow the subclasses to
	  * provide the type parameter for the 'key' argument themselves, possibly different from `Long` (but likely with homomorphic ordering).
	  * @param start lower bound (inclusive) on the keys to select from this trie as a new trie.
	  * @return a trie of the same root type as `this` containing only leaves with keys larger than `start` (and all of them).
	  */
	def fromKey(start :Long) :This

	/** Analogue of [[net.turambar.palimpsest.specialty.sets.OrderedSet#until]] working on keys in this trie. Provides a basis for subclasses to implement
	  * [[net.turambar.palimpsest.specialty.ordered.OrderedVals]], but doesn't extend the trait itself here in order to allow the subclasses to
	  * provide the type parameter for the 'key' argument themselves, possibly different from `Long` (but likely with homomorphic ordering).
	  * @param end upper bound (exclusive) on the keys to select from this trie as a new trie.
	  * @return a trie of the same root type as `this` containing only leaves with keys smaller than `end` (and all of them).
	  */
	def untilKey(end :Long) :This = keyRange(0, end)

	/** Analogue of [[net.turambar.palimpsest.specialty.sets.OrderedSet#range]] working on keys in this trie. Provides a basis for subclasses to implement
	  * [[net.turambar.palimpsest.specialty.ordered.OrderedVals]], but doesn't extend the trait itself here in order to allow the subclasses to
	  * provide the type parameter for the 'key' argument themselves, possibly different from `Long` (but likely with homomorphic ordering).
	  * @param start lower bound (inclusive) on the keys to select from this trie as a new trie.
	  * @param end upper bound (exclusive) on the keys to select from this trie as a new trie.
	  * @return a trie of the same root type as `this` containing only leaves with keys falling in the given range.
	  */
	def keyRange(start :Long, end :Long) :This


	/** Analogue of [[net.turambar.palimpsest.specialty.sets.OrderedSet#rangeImpl]] working on keys in this trie. Provides a basis for subclasses to implement
	  * [[net.turambar.palimpsest.specialty.ordered.OrderedVals]], but doesn't extend the trait itself here in order to allow the subclasses to
	  * provide the type parameter for the 'key' argument themselves, possibly different from `Long` (but likely with homomorphic ordering).
	  * @param from lower bound (inclusive) on the keys to select from this trie as a new trie. If not present, no lower bound is imposed.
	  * @param until upper bound (exclusive) on the keys to select from this trie as a new trie. If not present, no upper bound is imposed.
	  * @return a trie of the same root type as `this` containing only leaves with keys falling in the given range.
	  */
	def keyRangeImpl(from: Option[Long], until: Option[Long]): This

	/** Create a new inner trie node for the given path, joining the given subtries as its children in the exact order.
	  * Method geared towards implementing classes, performing no validation, and acting as a simple virtual constructor.
	  * Enforcing the invariants is left to the caller.
 	  * @param rpath longest prefix common to all keys both in `left` and `right` trees, delimited by a single set bit.
	  *              Used as the value for [[net.turambar.palimpsest.specialty.tries.LongPrefixTrie.LongTrieBranch#center]] / [[net.turambar.palimpsest.specialty.tries.LongPrefixTrie.LongTrieBranch#key]].
	  * @param left the left, non-empty child of the created branch, which keys are equal to `rpath` on all bits downto and excluding the lowest set bit,
	  *             and equal to zero on the lowest set bit in `rpath`/
	  * @param right the right, non-empty child of the created branch, which keys are equal to `rpath` on all bits downto and including the lowest set bit.
	  * @return most likely a  [[net.turambar.palimpsest.specialty.tries.LongPrefixTrie.LongTrieBranch]] instance, implementing the public interface
	  *         refered to here as `This`.
	  */
	protected[this] def newBranch(rpath :Long, left :This, right :This) :This

	protected[this] def combine[O](operator :TrieOperator[This, O])(first :This, second :This) :O =
		throw new IllegalArgumentException(s"unexpected subtries (${first.getClass.getName}, ${second.getClass.getName}) encoutered by $typeStringPrefix.combine(): $first, $second")

	protected[this] def combine(combinator :TrieCombinator[This])(first :This, second :This) :This =
		combine(combinator :TrieOperator[This, This])(first, second)
//	protected[this] def disjoint(other: This): This
}


object LongPrefixTrie {
	import java.lang.Long.{MIN_VALUE => SignBit}


	/** Base trait for empty tries indexed by `Long` keys. All default [[LongPrefixTrie]] implementations provided here
	  * assume that it is never a part of a larger trie (that is, can never be a child of a [[LongTrieBranch]]) and
	  * always exists only on its own as a specific empty collection implementation or as a marker serving a role
	  * similiar to `None :Option[_]` when used instead of a [[LongTrieLeaf]].
	  * @tparam V value type of the trie interface used as the public interface for a particular extending trie implementation
	  * @tparam This public self type of a particular trie implementation being the base type of an extending class.
	  *              Not constrainted here for convenience, but concrete implementations will have
	  *              `this.type <: This <: LongPrefixTrie <: Trie[Long, _, _]`.
	  */
	abstract class EmptyLongTrie[+V, +E, +This <: TrieTemplate[Long, V, E, This]] extends AbstractEmptyTrie[Long, V, E, This] with LongPrefixTrie[V, This] { this :This =>
		@inline final def prefix = 0L
		@inline final def mask = 0L
		@inline final override def extremeLeft = 0L
		@inline final override def extremeRight = -1L //only `1` bits

		override def leaf(n: Int): This = this //throw new IndexOutOfBoundsException(s"$typeStringPrefix.rank($n)")

		override def fromKey(start: Long): This = this
		override def untilKey(end :Long) :This = this
		override def keyRange(start: Long, end: Long): This = this
		override def keyRangeImpl(from: Option[Long], until: Option[Long]): This = this
	}


	/** A leaf in a trie indexed by `Long` keys, storing exactly one key-value pair and terminating any path leading to it.
	  * @param k immutable value of the key associated with this leaf
	  * @tparam V value type stored in the leaves of this trie, not defined by this abstract class and left for subclasses to provide
	  * @tparam This self-type being the common root type of all node types in a larger trie implementation any extending class may be part of.
	  *              Enforced here to be self-type of this, that is `this.type <: This` for any instance of this class.
	  */
	abstract class LongTrieLeaf[+V, +E, +This<:LongPrefixTrie[V, This] with IterableTemplate[E, This]](k :Long)
		extends SingletonLeaf[Long, V, E, This] with LongPrefixTrie[V, This]
	{ this :This =>
		key = k

		@inline final override def asTrie :This = this//.asInstanceOf[This]
//		@inline final def key = k
		@inline final def flipped :Long = flipSign(key)
		@inline final def prefix = key
		@inline final def mask = -1L
		@inline final override def extremeLeft = key
		@inline final override def extremeRight = key

		@inline final override def leafFor(key :Long) :This = if (key==this.key) asTrie else empty
		@inline final override def hasLeaf(key: Long) = key == this.key

		override def leaf(n :Int) :This =
			if (n==0) this else empty //throw new IndexOutOfBoundsException(s"$typeStringPrefix.rank($n)")

		override def fromKey(start: Long): This = if (flipSign(start) <= flipSign(key)) this else empty
		override def untilKey(end :Long) :This = if (flipSign(key) < flipSign(end)) this else empty
		override def keyRange(start: Long, end: Long): This = {
			val v = flipSign(key)
			if (flipSign(start) <= v && v < flipSign(end)) this else empty
		}
		override def keyRangeImpl(from: Option[Long], until: Option[Long]): This = {
			val v = flipSign(key)
			if ((from.isEmpty || flipSign(from.get) <= v) && (until.isEmpty || v < flipSign(until.get)))
				this
			else empty
		}

		/** Default recursive implementation of [[Trie#combine(This, TrieCombinator[This] ]] using the divde & conquer appraoch. */
		protected[this] final def dividedCombine(trie :This, combinator :TrieCombinator[This]) :This = trie match {
			case branch :LongTrieBranch[V, This] =>
				val rpath = branch.center; val DiffBit = rpath & -rpath
				(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
					case 0 =>
						join(rpath, combinator emptyFirst branch.left, dividedCombine(branch.right, combinator))
					case DiffBit =>
						join(rpath, dividedCombine(branch.left, combinator), combinator emptyFirst branch.right)
					case _ =>
						combinator.disjoint(asTrie, trie)
				}
			case leaf :LongTrieLeaf[V, _, This] =>
				if (leaf.key==key) combinator.matched(asTrie, leaf.asTrie)
				else combinator.disjoint(asTrie, leaf.asTrie)

			case empty if empty.isEmpty => combinator.emptySecond(asTrie)

			case _ => combine(combinator)(this, trie)

		}


		override protected[this] def combine(other: This, combinator: TrieCombinator[This]): This =
			dividedCombine(other, combinator)



		/** Default recursive implementation of [[Trie#combine(This, TrieOperator[This, O] ]] using the divide & conquer approach. */
		protected[this] final def dividedCombine[O](trie :This, operator :TrieOperator[This, O]) :O = trie match {
			case branch :LongTrieBranch[V, This] =>
				val rpath = branch.center; val DiffBit = rpath & -rpath
				(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
					case 0 =>
						operator.reduced(trie)(operator emptyFirst branch.left, dividedCombine(branch.right, operator))
					case DiffBit =>
						operator.reduced(trie)(dividedCombine(branch.left, operator), operator emptyFirst branch.right)
					case _ =>
						operator.disjoint(asTrie, trie)
				}
			case leaf :LongTrieLeaf[V, _, This] =>
				if (leaf.key==key) operator.matched(asTrie, leaf.asTrie)
				else operator.disjoint(asTrie, leaf.asTrie)

			case empty if empty.isEmpty => operator.emptySecond(asTrie)

			case _ => combine(operator)(this, trie)

		}



		override protected[this] def combine[O](other: This, operator: TrieOperator[This, O]): O =
			dividedCombine(other, operator)





		override protected[this] def unionTrie(other: This) :This = other match {
			case branch :LongTrieBranch[V, This] =>
				val rpath = branch.center; val DiffBit = rpath & -rpath
				(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
					case 0 => this unionTrie branch.right match {
						case r if r eq branch.right => other
						case r => newBranch(rpath, branch.left, r)
					}

					case DiffBit => this unionTrie branch.left match {
						case l if l eq branch.left => other
						case l => newBranch(rpath, l, branch.right)
					}
					case _ => disjoint(other)
				}
			case leaf :LongTrieLeaf[V, E, This] =>
				if (key == leaf.key) other else disjoint(other) //would be better to keep this instead of other to reuse our parent as-is
				//todo: multi-value leaves?
			case empty if empty.isEmpty => this

		}


		override protected[this] def diffTrie(other: This) :This = {
			val trie :LongPrefixTrie[V, This] = other
			if (subtrieOf(other)) empty else asTrie
		}

		override protected[this] def intersectTrie(other: This) :This =
			if (subtrieOf(other)) asTrie else empty //value comparison?

		override protected[this] def subtrieOf(other: This) :Boolean =
			this == other.select(key)



		protected[this] def newBranch(rpath :Long, left :This, right :This) :This

		@inline final protected[this] def join(path :Long, left :This, right :This) :This =
			if (left.isEmpty) right
			else if (right.isEmpty) left
			else newBranch(path, left, right)

		@inline final override protected[this] def join(other :This) :This =
			if (other.isEmpty) this
			else disjoint(other)



//		protected[this] def disjoint(other: This): This = {
//			val joint = commonPath(key, other.key)
//			if ((joint & key)==joint) //checks the value of the first different bit for this in key (in joint it is set)
//				newBranch(joint, other, asTrie)
//			else
//				newBranch(joint, asTrie, other)
//		}

	}


	/** While all leaves in a [[LongPrefixTrie]] follow essentially unsigned ordering of their keys,
	  * it would often be more convenient that they followed natural (signed) ordering of the keys instead.
	  * It can be easily achieved by storing all keys with their sign (highest) bit flipped to the opposite value,
	  * thus swapping the left and right children of the root node responsible for the sign bit (if values of both signs are present in the trie).
	  * This trait is a mixin for leaves of such tries which wish to expose the keys as `key + Long.MinValue` instead.
	  * The `key` property is assumed to be already flipped, and all methods defined by [[Trie]]/[[LongPrefixTrie]] are assumed
	  * to work for keys with their sign bit already flipped to avoid problems resulting from flipping it several times.
	  * Instead, the [[OrderedAs]] methods flip the sign bit of all their key arguments. This provides a discrepancy between
	  * what's stored and exposed by [[Trie#key]], as well as potential other methods, and the values accepted by order-related methods.
	  * It is upto implementing classes to resolve and hide it.
	  */
	trait OrderedLongLeaf[+V, +This <: LongPrefixTrie[V, This] with OrderedAs[Long, This]]
		extends LongTrieLeaf[V, Any, This] with OrderedAs[Long, This]
	{ this :This =>

		/** Natural ordering on `Long`, that is one defined by `Ordering.Long`. */
		override implicit def ordering: Ordering[Long] = Ordering.Long

		/** The ordering of 'real' key values as stored in this trie, equivalent to unsigned ordering of `Long`.
		  * It is up to subclasses to override it with `Ordering.Long` to complete the illusion of natural ordering
		  */
//		implicit override def ordering :Ordering[Long] = LongPrefixTrie.ordering

		/** The `n-th` 'virtual' key in this set, with the only valid value of `n` being `0` as a leaf
		  * is essentially a singleton collection.
		  * @param n index of the required element in the collection. If not `0`, an `IndexOutOfBoundsException` will be thrown.
		  * @return `key + Long.MinValue`
		  */
		override def keyAt(n: Int): Long =
			if (n==0) flipSign(key) else throw new IndexOutOfBoundsException(s"$typeStringPrefix.rank($n)")

		/** An iterator over 'virtual' key(s), flipping the sign of the actual key for purpose of comparison with `start`. */
		override def keysIteratorFrom(start: Long): FitIterator[Long] = {
			val v = flipSign(key)
			if (start <= v) FitIterator(v) else FitIterator.empty
		}

		override def rangeImpl(from: Option[Long], until: Option[Long]): This = {
			val v = flipSign(key)
			if ((from.isEmpty || flipSign(from.get) <= v) && (until.isEmpty || v < flipSign(until.get)))
				this
			else empty
		}

		override def from(from: Long) = if (from <= flipSign(key)) this else empty

		override def until(until: Long) = if (flipSign(key) < until) this else empty

		override def range(from: Long, until: Long) = {
			val v = flipSign(key)
			if (from <= v && v < until) this else empty
		}
	}


	/** An inner node in a trie which keys (paths to the leaves) follow the binary representation of `Long` values from the highest bit.
	  * @param path a `Long` value which, in its highest bits, is equal to all the values (leaves) in this trie. Therefore its `n`
	  *             highest bits, `0<=n<64` is the 'path' in any larger trie to this node (although it can exists as an independent root itself)
	  *             and defines the longest common prefix to all values in this trie. Bit `n+1` (from the highest bit) is set, and is followed
	  *             by zeros in the lowest positions, thus the lowest set bit of this value denotes the end of the prefix common to both children
	  *             under this node, while at the same time being equal to all values in the `right` subtrie on its `n+1` highest bits
	  *             (including the terminating bit).
	  * @param zero A non-empty trie containing all leaves in this trie which keys have zero on the position of the splitting bit
	  * @param one A non-empty trie containing all leaves in this trie which keys have one on the position of the splitting bit.
	  * @tparam V Value type associated with leaves in this trie, completely ignored by this class.
	  * @tparam This self-type defining the 'public' common trait of all node types in a given trie implementations.
	  *              Most always it will be `this.type <: This`, but it isn't enforced or assumed by this class, although it does
	  *              make use of the virtual [[Trie#asTrie]] method for a self reference.
	  */
	abstract class LongTrieBranch[+V, +This <: LongPrefixTrie[V, This]](private[this] var path :Long, zero :This, one :This)
		extends BinaryTrie[Long, V, This](zero, one) with LongPrefixTrie[V, This]
	{ //self :This =>

		def this(left :This, right :This) = this(commonPath(left.key, right.key), left, right)



		/** A 'middle' value of this set when treated as the longest possible range of values with common highest bits.
		  * Consists of the bits common to all keys in this set followed by a single set bit in the first position (from the highest bit)
		  * for which there are keys in this set with both values. It carries full information about the path to this node:
		  * both the value of the common prefix and effectively its length.
		  *
		  * @see [[splitBit]]
		  * @return value equivalent to [[prefix]] `|` [[splitBit]]
		  */
		@inline final def center :Long = path

		@inline final protected[this] def center_=(delimitedPrefix :Long) :Unit = path = delimitedPrefix

		/** Partial binary representation of a `Long` value, with highest bits set to the values which are common to all values in this set.
		  * All bits below this common prefix are clear. Represents the 'path' to this node in a larger trie in the sense that
		  * for all ancestor nodes `x.prefix` is a proper prefix of this node.
		  */
		def prefix = center & (center-1) //clears lowest set bit

		/** Mask for the single bit separating the left and right subtrees. `left` is assumed to contain
		  * all values for which this bit is clear, while `right` to contain all values with this bit set.
		  * It is the highest bit for which there are elements of both values in this set, i.e. all elements
		  * are equal on the higher bits.
		  */
		def splitBit = center & -center //mask for the single lowest set bit

		/** Mask for bits common to all elements in this set, formed by a sequence of any number of `1` bits starting from the highest bit,
		  * followed by zero bits in the remaining lower positions. The length of the mask represents the real span of the [[prefix]] value.
		  * @return value such that `(mask & k)==prefix` for all keys `k` in this set (including inner nodes)
		  */
		def mask = { val bit = center & -center; -bit ^ bit }

		/** Delimited path to this node equivalent to [[LongTrieBranch#center]], consisting of the longest prefix
		  * common to all keys in this trie, followed by a single `1` bit, and then only zeros in any remaning lowest positions.
		  * @return `prefix | splitBit`
		  */
		def key = path

		/** Lowest possible key value in this set (treated as unsigned), i.e. the prefix to this node followed exclusively by zeros. */
		@inline final override def extremeLeft :Long = center & (center-1)

		/** Highest possible key value in this set (treated as unsigned), i.e. the prefix to this node followed exclusively by 'one' bits. */
		@inline final override def extremeRight :Long = center | (center-1)





		/** Value used to determine in which subtree of this set the given element belongs.
		  *
		  * @param elem any value to potentially add or remove from this set.
		  * @return `0` if the element belongs to the `right` tree,
		  *         [[splitBit]] if the element belongs to the `left` tree,
		  *         and any other value if the element doesn't share the common path of this set.
		  */
		protected[this] def assign(elem :Long) = {
			//left-hand xor operand is equivalent to a xor of elem and our path, clearing any bits lower than diffBit (lowest set bit in common),
			//but has the diffBit always set, as elem | -elem is always 1, and both -common and common have diffBit set.
			//right-hand xor operand clears the diffBit if it is set in elem.
			val wrongBitsSet = elem & -center
			(wrongBitsSet | -elem & center) ^ (wrongBitsSet & center)
		}


		override def belongs(key :Long) = (key & -center | ~key & center) == (center & -center) //(key & mask) == path



		override def leaf(idx: Int) :This =
			if (idx<0)
				empty
//				throw new IndexOutOfBoundsException(s"$stringPrefix<$size>.nth($idx)")
			else { //todo: verify hasFastSize ?
				var result :This = empty
				def skip(trie :LongPrefixTrie[V, This], n :Int) :Int = trie match {
					case branch :LongTrieBranch[V, This] =>
						val skipped = skip(branch.left, n)
						if (skipped <= idx)
							skip(branch.right, skipped)
						else skipped
					case leaf :LongTrieLeaf[V, _, This] =>
						if (n==idx)
							result = leaf.asTrie
						n + 1
					case _ :EmptyTrie[Long, V, This] => n
				}
				val skipped = skip(left, idx)
				if (skipped <= idx)
					skip(right, idx - skipped)
//					if (skipped + skip(right, idx - skipped) <= idx)
//						throw new IndexOutOfBoundsException(s"$typeStringPrefix.rank($idx)")
				result
			}


		override def keyRangeImpl(from: Option[Long], until: Option[Long]): This =
			until match {
				case Some(ub) => keyRange(from getOrElse Long.MinValue, ub)
				case _ => from match {
					case Some(lb) => fromKey(lb)
					case _ => repr
				}
			}

		/** Selects the subtrie of the given trie containing all leaves with keys between `lo` and `hi`, using natural ordering.
		  * @param trie trie to select range of, with all leaves having keys of the same signum
		  * @param lo lower bound (inclusive) for keys to be included, must be of the same signum as all keys in `trie`.
		  * @param hi higher bound (exclusive) for keys to be included, must be of the same signum as all keys in `trie`.
		  */
		protected[this] final def keyRange(trie :LongPrefixTrie[V, This], lo :Long, hi :Long) :This =
			trie match {
				case branch :LongTrieBranch[V, This] =>
					val split = branch.center
					if (lo <= branch.extremeLeft && hi > branch.extremeRight) branch.clone
					else if (hi <= split) keyRange(branch.left, lo, hi)
					else if (lo >= split) keyRange(branch.right, lo, hi)
					else {
						val l = keyRange(branch.left, lo, hi)
						val r = keyRange(branch.right, lo, hi)
						if (l.isEmpty) r
						else if (r.isEmpty) l
						else if ((l eq branch.left) && (r eq branch.right)) branch.clone()
						else newBranch(split, l, r)
					}

				case leaf : LongTrieLeaf[V, _, This] =>
					val v = leaf.key
					if (lo <= v && v < hi) trie.clone else empty
				case other =>
					throw new IllegalStateException(s"$stringPrefix contains node other than leaf and branch: $other :${other.getClass.getName}")
			}

		/** Selects the subtrie of the given trie containing all leaves with keys greater or equal `lo`, using natural ordering.
		  * @param trie to to select range of, with all keys of the same signum as `lo`
		  * @param lo lower bound on the keys to select
		  */
		protected[this] final def keysFrom(trie :LongPrefixTrie[V, This], lo :Long) :This = trie match {
			case branch :LongTrieBranch[V, This] =>
				val split = branch.center
				if (lo <= branch.extremeLeft) branch.clone
				else if (lo >= split) keysFrom(branch.right, lo)
				else keysFrom(branch.left, lo) match {
					case e if e.isEmpty => branch.right.clone
					case l if l eq branch.left => branch.clone
					case l => newBranch(split, l, branch.right.clone)
				}
			case leaf :LongTrieLeaf[V, _, This] => if (lo <= leaf.key) leaf.clone else empty
			case other => throw new IllegalStateException(s"OrderedLongTrie contains node other than leaf and branch: $other :${other.getClass.getName}")
		}


		/** Selects the subtrie of this trie with keys greater or equal to from, based on '''unsigned''' ordering.
		  * Binary prefix ordering follows ''unsigned'' ordering of `Long` values. This ordering is equivalent
		  * to natural ordering based on signed `Long` values for values of the same sign, but all negative keys
		  * compare greater than any non-negative key.
		  *
		  * Note that this implementation is consistent with default ordering of long tries, which is equivalent
		  * to comparing them when interpreted as unsigned values. Subclasses may change this ordering and thus
		  * require a change to this method.
		  *
		  * Taking advantage of the above mentioned facts, this method resigns to natural comparing of the keys
		  * after narrowing down the result to the branch corresponding to the appropriate sign, if needed.
		  */
		override def fromKey(from: Long) :This =
			if (center == SignBit) //left child contains keys >= 0 (0 sign bit), right child contains keys < 0 (1 sign bit)
				if (from < 0) keysFrom(right, from) //narrow the search down to the right subtrie
//				else filteredLeft(keysFrom(left, from)) //narrow the left subtrie and keep the right as-is
				else filtered(keysFrom(left, from), right.clone())
			else if (center < 0) //all keys start with `1` sign bit
				if (from >= 0) clone() else keysFrom(this, from)
			else //all keys start with `0` sign bit
			if (from < 0) empty else keysFrom(this, from)


		override def untilKey(until: Long) = keyRange(0, until)

		override def keyRange(from: Long, until: Long) :This =
			if (center==SignBit) //left contains keys starting with 0, right contains keys starting with 1
				if (from < 0)
					if (until >= 0 || until <= from) empty
					else keyRange(right, from, until)
				else if (until >= 0)
					if (until <= from) empty
					else keyRange(left, from, until)
				else filtered(keysFrom(left, from), keyRange(right, SignBit, until))
			else if (center < 0) //all keys start with `1` bit
				if (until >= 0) empty
				else if (from >= 0) keyRange(this, SignBit, until)
				else if (until <= from) empty
				else keyRange(this, from, until)
			else //center >= 0, that is has `0` first bit
			if (from < 0) empty
			else if (until < 0) keysFrom(this, from)
			else if (until<=from) empty
			else keyRange(this, from, until)


		def leafIteratorFromKey(start: Long): FitIterator[This] =
			if (center >= 0 && start < 0) //all our keys are non-negative
				FitIterator.empty
			else if (center < 0 && center != SignBit && start >= 0) //all our keys start with `1` bit and `start` starts with `0`
				new LeafIterator(this, 64)
			else {
				val stack = new Array[Trie[Long, V, This]](64)
				//numerical comparisons of keys are equivalent to unsigned ordering of a trie iff compared numbers have the same sign.
				//Therefore, we need to ensure that start is of the same sign as values in the node we descend to.
				val depth =
				if (center == SignBit && start >= 0) { //a singularity, center is the smallest long and not larger than left trie keys
					stack(0) = this //push ourselves so the iterator retraces back to the right subtrie once it finishes the left one
					fillIteratorStack(left, start, stack, 1) //force search in the left subtree (containing keys with `0` sign bit like start)
				} else //now we are sure that keyStart will have the same sign as all values within requested range
					fillIteratorStack(this, start, stack, 0) //handles also case path=0x8000000000000000L && start >=0
				if (depth<0)
					FitIterator.empty
				else
					new LeafIterator[This, Long, V, This](stack, depth)
			}

		protected[this] def fillIteratorStack(root :LongPrefixTrie[V, This], fromKey :Long, stack :Array[Trie[Long, V, This]], stackSize :Int) :Int = {
			val path = root.key
			if (path >= 0 && fromKey < 0) //all our keys have the highest bit  `0`, while fromKey has `1`
				-1
			else if (path < 0 && path != SignBit && fromKey >= 0) {
				//all our keys are negative
				stack(stackSize) = root; stackSize
			} else {
				/** Build a stack for the iterator representing a path to the first iterated leaf.
				  * The stack is implemented as an array of size 64 (maximum depth of a Long-keyed trie) and index in the array
				  * of the top element on the stack.
				  * The path will cut the trie in two, with the right-hand side containing all the elements to be iterated over.
				  * If there are no elements to iterate, the stack will be empty. Otherwise on its top resides a singleton leaf
				  * with the first/next element of the created iterator. Below it are all Branch nodes on the path leading to it,
				  * from which we descended into the left tree, and thus their whole right subtree remains to be iterated.
				  * advancing the iterator retreats on the stack to take most-recent possible right turn
				  * (removing the node to which we retreated from the stack, and advancing again with a preference for the left child.
				  *
				  * The implementation performs normal numeric comparisons on the key fragments, taking advantage of two facts:
				  *   - the binary prefix common to all elements in the trie, when interpreted as a numerical value, is the set's lower bound;
				  *   - for branch nodes, their delimited path (common prefix followed by a single set bit separating the values of its left and right subtree)
				  * is the lower bound of the right subtree and upper bound of the left subtrie, as all elements in the left trie
				  * have the terminator bit clear, while all elements in the right subtrie have the bit set.
				  */
				@tailrec def buildStack(trie: Trie[Long, V, This], top: Int): Int = trie match {
					case branch: LongTrieBranch[V, This] =>
						if (branch.center <= fromKey)
							buildStack(branch.right, top)
						else {
							stack(top) = branch
							buildStack(branch.left, top + 1)
						}
					case leaf: LongTrieLeaf[V, _, This] =>
						if (fromKey <= leaf.key) {
							stack(top) = leaf //we've found the minimal leaf greater than start
							top + 1
						} else if (top == 0)
							-1 //end of the road, all leaves are smaller than start
						else
							buildStack(stack(top).asInstanceOf[LongTrieBranch[V, This]].right, top - 1) //retreat back to the right tree

					//this can happen only if a subclass introduces empty leaves into the trie
					case empty if empty.isEmpty =>
						if (top == 0) -1
						else {
							val next = stack(top).asInstanceOf[LongTrieBranch[V, This]].right
							stack(top) = null
							buildStack(next, top - 1)
						}
					//todo: leaf and empty nodes which don't implement corresponding traits
					//				case weirdo =>
				}

				buildStack(root, 0)
			}
		}







		override def hasLeaf(key: Long) = leafFor(key).nonEmpty

		override def leafFor(key: Long): This = {
			@tailrec def dive(trie :This) :This = trie match {
				case branch :LongTrieBranch[V, This] =>
					val rpath = branch.center; val DiffBit = rpath & -rpath
					(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
						case 0 => dive(branch.right)
						case DiffBit => dive(branch.left)
						case _ => empty
					}
				case leaf :LongTrieLeaf[V, _, This] =>
					if (leaf.key == key) trie
					else empty
				case e => e
			}
//			dive(self)
			val DiffBit = center & -center //inlined manually to avoid enforcing this <: This
			(key & -center | ~key & center) ^ (key & DiffBit) match {
				case 0 => dive(right)
				case DiffBit => dive(left)
				case _ => empty
			}
		}





		final protected[this] def patchSubtrie(root: MutableTrieRoot[This], key: Long, mutant: TriePatch[Long, V, This])(trie: This with LongTrieBranch[V, This]):This = {
			val path = trie.center; val DiffBit = path & -path
			(key & -path | ~key & path) ^ (key & DiffBit) match {
				case 0 => trie.right match {
					case branch: LongTrieBranch[V, This] with This => patchSubtrie(root, key, mutant)(branch) match {
						case same if same eq branch => trie
						case patched => newBranch(path, trie.left, patched)
					}
					case leaf: LongTrieLeaf[V, _, This] =>
						if (key == leaf.key) mutant.updateLeaf(leaf) match {
							case e if e.isEmpty => root.size_--(); trie.left
							case same if same eq leaf => trie
							case replaced => newBranch(path, trie.left, replaced)
						} else mutant.notFound(key, leaf) match {
							case e if e.isEmpty => trie
							case newLeaf => root.size_++(); newBranch(path, trie.left, disjoint(trie.right, newLeaf))
						}
				}
				case DiffBit => trie.left match {
					case branch :LongTrieBranch[V, This] with This => patchSubtrie(root, key, mutant)(branch) match {
						case same if same eq branch => trie
						case patched => newBranch(path, patched, trie.right)
					}
					case leaf :LongTrieLeaf[V, _, This] =>
						if (key == leaf.key) mutant.updateLeaf(leaf) match {
							case e if e.isEmpty => root.size_--(); trie.right
							case same if same eq leaf => trie
							case replaced => newBranch(path, replaced, trie.right)
						} else mutant.notFound(key, leaf) match {
							case e if e.isEmpty => trie
							case newLeaf => root.size_++(); newBranch(path, disjoint(trie.left, newLeaf), trie.right)
						}
				}
				case _ => mutant.notFound(key, trie) match {
					case e if e.isEmpty => trie
					case newLeaf => root.size_++(); disjoint(trie, newLeaf)
				}
			}

		}

		override protected[this] def patch(root: MutableTrieRoot[This], key: Long, mutant: TriePatch[Long, V, This]): This =
			patchSubtrie(root, key, mutant)(this.asInstanceOf[LongTrieBranch[V, This] with This])


		override protected[this] def mutate(root: MutableTrieRoot[This], parent: MutableTrieRoot[This], key: Long, mutant: TriePatch[Long, V, This]): Unit =
			parent.hang(patchSubtrie(root, key, mutant)(this.asInstanceOf[LongTrieBranch[V, This] with This]))



//		protected[this] def combine(other :This)(combinator :(This, This) => This) :This =
//			combine(other, TrieCombinator(combinator))

		protected[this] override def combine(other :This, combinator :TrieCombinator[This]) :This =
			if (other.isEmpty) combinator emptySecond asTrie
			else {

				def divideLeft(first :This, leaf :This, key :Long) :This = first match {
					case branch :LongTrieBranch[V, This] =>
						val rpath = branch.center; val DiffBit = rpath & -rpath
						(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
							case 0 =>
								join(rpath, combinator emptySecond branch.left, divideLeft(branch.right, leaf, key))
							case DiffBit =>
								join(rpath, divideLeft(branch.left, leaf, key), combinator emptySecond branch.right)
							case _ =>
								combinator.disjoint(first, leaf)
						}
					case _ if first.key == key => combinator.matched(first, leaf)

					case _ => combinator.disjoint(first, leaf)
				}

				def divideRight(key :Long, leaf :This, second :This) :This = second match {
					case branch :LongTrieBranch[V, This] if branch.plurality==2 =>
						val rpath = branch.center; val DiffBit = rpath & -rpath
						(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
							case 0 =>
								join(rpath, combinator emptyFirst branch.left, divideRight(key, leaf, branch.right))
							case DiffBit =>
								join(rpath, divideRight(key, branch.left, leaf), combinator emptyFirst branch.right)
							case _ =>
								combinator.disjoint(leaf, second)
						}
					case _ if second.key == key => combinator.matched(leaf, second)
					case _ => combinator.disjoint(leaf, second)
				}

				def divide(first :This, second :This) :This = first match {
					case branch1 :LongTrieBranch[V, This] if branch1.plurality==2 => second match {
						case branch2 :LongTrieBranch[V, This] if branch2.plurality==2 =>
							val path1 = branch1.center; val path2 = branch2.center
							val diff = path1 ^ path2
							if (diff==0L)
								join(path1, divide(branch1.left, branch2.left), divide(branch1.right, branch2.right))
							else {
								val point1 = path1 & -path1 //lowest set bit in path1, that is the bit dividing branch1.left and branch1.right
								val mask1 = -point1 ^ point1 //mask for the common prefix of all elements in branch1
								if ((diff & mask1) == 0L) //path1 is prefix of path2 because path1 is equal to path2 on the span of path1
									if ((path2 & point1) == 0L) //branch2 belongs with the left child of branch1
										join(path1, divide(branch1.left, second), combinator emptySecond branch1.right)
									else
										join(path1, combinator emptySecond branch1.left, divide(branch1.right, second))
								else {
									val point2 = path2 & -path2; val mask2 = -point2 ^ point2
									if ((diff & mask2)==0L) //path2 is prefix of path1
										if ((path1 & point2) == 0L)
											join(path2, divide(first, branch2.left), combinator emptyFirst branch2.right)
										else
											join(path2, combinator emptyFirst branch2.left, divide(first, branch2.right))
									else //sets are disjoint, as they differ in the common span of path1 and path2
										combinator.disjoint(first, second)
								}
							}
						case leaf :LongTrieLeaf[V, _, This] => divideLeft(first, second, leaf.key)

						case _ => combine(combinator)(first, second)
					}
					case leaf :LongTrieLeaf[V, _, This] => divideRight(leaf.key, first, second)

					case _ => combine(combinator)(first, second)
				}
				divide(asTrie, other) //inline? self type?
			}


		override protected[this] def combine[O](other: This, operator: TrieOperator[This, O]) :O =
			dividedCombine(asTrie, other, operator)

		protected[this] final def divideLeft[O](first :This, leaf :This, key :Long, operator :TrieOperator[This, O]) :O = first match {
			case branch :LongTrieBranch[V, This] =>
				val rpath = branch.center; val DiffBit = rpath & -rpath
				(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
					case 0 =>
						operator.reduced(first)(operator emptySecond branch.left, divideLeft(branch.right, leaf, key, operator))
					case DiffBit =>
						operator.reduced(first)(divideLeft(branch.left, leaf, key, operator), operator emptySecond branch.right)
					case _ =>
						operator.disjoint(first, leaf)
				}
			case l :LongTrieLeaf[V, _, This] =>
				if (l.key == key) operator.matched(first, leaf)
				else operator.disjoint(first, leaf)

			case _ => combine(operator)(first, leaf)
		}

		protected[this] final def divideRight[O](key :Long, leaf :This, second :This, operator :TrieOperator[This, O]) :O = second match {
			case branch :LongTrieBranch[V, This] =>
				val rpath = branch.center; val DiffBit = rpath & -rpath
				(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
					case 0 =>
						operator.reduced(second)(operator emptyFirst branch.left, divideRight(key, leaf, branch.right, operator))
					case DiffBit =>
						operator.reduced(second)(divideRight(key, branch.left, leaf, operator), operator emptyFirst branch.right)
					case _ =>
						operator.disjoint(leaf, second)
				}

			case l :LongTrieLeaf[V, _, This] =>
				if (leaf.key == key) operator.matched(leaf, second)
				else operator.disjoint(leaf, second)

			case _ => combine(operator)(leaf, second)
		}

		protected[this] final def dividedCombine[O](first :This, second :This, operator :TrieOperator[This, O]) :O = first match {
			case branch1 :LongTrieBranch[V, This] => second match {
				case branch2 :LongTrieBranch[V, This] =>
					val path1 = branch1.center; val path2 = branch2.center
					val diff = path1 ^ path2
					if (diff==0L)
						operator.reduced(first)(dividedCombine(branch1.left, branch2.left, operator), dividedCombine(branch1.right, branch2.right, operator))
					else {
						val point1 = path1 & -path1 //lowest set bit in path1, that is the bit dividing branch1.left and branch1.right
						val mask1 = -point1 ^ point1 //mask for the common prefix of all elements in branch1
						if ((diff & mask1) == 0L) //path1 is prefix of path2 because path1 is equal to path2 on the span of path1
							if ((path2 & point1) == 0L) //branch2 belongs with the left child of branch1
								operator.reduced(first)(dividedCombine(branch1.left, second, operator), operator emptySecond branch1.right)
							else
								operator.reduced(first)(operator emptySecond branch1.left, dividedCombine(branch1.right, second, operator))
						else {
							val point2 = path2 & -path2; val mask2 = -point2 ^ point2
							if ((diff & mask2)==0L) //path2 is prefix of path1
								if ((path1 & point2) == 0L)
									operator.reduced(second)(dividedCombine(first, branch2.left, operator), operator emptyFirst branch2.right)
								else
									operator.reduced(second)(operator emptyFirst branch2.left, dividedCombine(first, branch2.right, operator))
							else //sets are disjoint, as they differ in the common span of path1 and path2
								operator.disjoint(first, second)
						}
					}
				case leaf :LongTrieLeaf[V, _, This] => divideLeft(first, second, leaf.key, operator)

				case _ => combine(operator)(first, second)
			}
			case leaf :LongTrieLeaf[V, _, This] => divideRight(leaf.key, first, second, operator)

			case _ => combine(operator)(first, second)
		}


		override protected[this] def without(key: Long): This = patch(key, DeleteKey[V, This])


		override protected[this] def unionTrie(other: This) = combine(other, new UnionCombinator)

		override protected[this] def diffTrie(other: This) = combine(other, new DifferenceCombinator)

		override protected[this] def intersectTrie(other: This) = combine(other, new IntersectionCombinator)

		override protected[this] def subtrieOf(other: This) :Boolean =
			if (other.isEmpty) false
			else {
				def hasAllLeaves(leaves :This, superset :This) :Boolean = leaves match {
					case branch1 :LongTrieBranch[V, This] if branch1.plurality==2 => superset match {
						case branch2 :LongTrieBranch[V, This] if branch2.plurality==2 =>
							val path1 = branch1.center; val path2 = branch2.center
							val diff = path1 ^ path2
							if (diff == 0L)
								hasAllLeaves(branch1.left, branch2.left) && hasAllLeaves(branch1.right, branch2.right)
							else {
								val point1 = path1 & -path1
								val point2 = path2 & -path2; val mask2 = -point2 ^ point2
								((diff & mask2) == 0L) && hasAllLeaves(leaves, if ((path1 & point2) == 0L) branch2.left else branch2.right)
							}
						case _ => false
					}
					case leaf => leaf == superset.select(leaf.key)
				}
//				hasAllLeaves(asTrie, other) //inline asTrie=>this ? self type?
				hasAllLeaves(left, other) && hasAllLeaves(right, other)
			}






		override protected[this] def copy(l: This, r: This): This = newBranch(center, left, right)

//		@inline protected[this] final def branch(split :Long, left :This, right :This) :This =
		protected[this] def newBranch(rpath :Long, left :This, right :This) :This




		protected[this] def join(path :Long, left :This, right :This) :This =
			if (left.isEmpty) right
			else if (right.isEmpty) left
			else newBranch(path, left, right)

		protected[this] def join(s1 :This, s2 :This) :This =
			if (left.isEmpty) right
			else if (right.isEmpty) left
			else disjoint(s1, s2)


		protected[this] def disjoint(first :This, second :This) :This = {
			val lkey = first.key
			val joint = commonPath(lkey, second.key)
			if ((joint & lkey) == joint)
				newBranch(joint, second, first)
			else
				newBranch(joint, first, second)
		}

//		@inline final protected[this] def join(other :This) :This =
//			if (other.isEmpty) asTrie
//			else disjoint(other)
//
//		protected[this] def disjoint(other: This): This = {
//			val joint = commonPath(key, other.key)
//			if ((joint & key)==joint)
//				newBranch(joint, other, asTrie)
//			else
//				newBranch(joint, asTrie, other)
//		}


//		override def leafIterator: FitIterator[This] = new LeafIterator(asTrie, 64)

		protected[this] abstract class AbstractCombinator extends TrieCombinator[This] {
			final protected[this] val Empty = empty
			override final def reduce(res1: This, res2: This): This = LongTrieBranch.this.disjoint(res1, res2)
			override def reduced(original: This)(left: This, right: This) =
				join(original.asInstanceOf[LongTrieBranch[V, This]].center, left, right)
		}

		protected[this] class UnionCombinator extends AbstractCombinator {
			override def emptyFirst(right: This): This = right

			override def emptySecond(left: This): This = left

			override def disjoint(left: This, right: This): This = LongTrieBranch.this.disjoint(left, right)

			override def matched(left: This, right: This): This = left
		}

		protected[this] class DifferenceCombinator extends AbstractCombinator {
			override def emptyFirst(right: This): This = Empty

			override def emptySecond(left: This): This = left

			override def disjoint(left: This, right: This): This = left

			override def matched(left: This, right: This): This = Empty
		}

		protected[this] class IntersectionCombinator extends AbstractCombinator {
			override def emptyFirst(right: This): This = Empty

			override def emptySecond(left: This): This = Empty

			override def disjoint(left: This, right: This): This = Empty

			override def matched(left: This, right: This): This = left
		}
	}



	/** An implementation-oriented interface for [[LongPrefixTrie]] branches which can be modified after creation.
	  * As it allows for arbitrary substitution of nodes which can possibly violate the invariants of the implemented
	  * interface, it shouldn't be exposed to the outside world (that is, it should be extended only by non-public
	  * classes rather than their public interfaces).
	  */
	trait MutableLongTrie[V, This <: LongPrefixTrie[V, This]] extends LongTrieBranch[V, This] with MutableTrieBranch[Long, V, This] {
		@inline final def hang(replacement :This) :Unit =
			if ((replacement.key & center) == center)
				right = replacement
			else left = replacement

		@inline final override protected def hangRight(trie :This) :Unit = right = trie
		@inline final override protected def hangLeft(trie :This) :Unit = left = trie


		override protected[this] def newBranch(path :Long, left :This, right :This) :This with MutableLongTrie[V, This]

		protected[this] def mutateOther(root :MutableTrieRoot[This], key :Long, mutant :TriePatch[Long, V, This])(parent :This, trie :This) :Unit =
			(trie :This) match {
				case branch :LongTrieBranch[V, This] with This => patchSubtrie(root, key, mutant)(branch)
				case unexpected => throw new LibraryError(s"Unexpected subtrie instance under a MutableLongTrie: $unexpected :${unexpected.getClass.getName}")
			}

		@inline final override protected[this] def mutate(root :MutableTrieRoot[This], key: Long, mutant: TriePatch[Long, V, This]): Unit =
			mutate(root, root, key, mutant)

		override protected[this] def mutate(root :MutableTrieRoot[This], parent :MutableTrieRoot[This], key: Long, mutant: TriePatch[Long, V, This]): Unit = {
			@tailrec def modify(parent :MutableTrieRoot[This], trie :MutableLongTrie[V, This] with This) :Unit = {
				val rpath = trie.center; val DiffBit = rpath & -rpath
				(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
					case 0 => trie.right match {
						case branch: MutableLongTrie[V, This] with This  => modify(trie, branch)
						case leaf: LongTrieLeaf[V, _, This] =>
							if (leaf.key == key) mutant.updateLeaf(leaf) match {
								case e if e.isEmpty => root.size_--(); parent.hang(trie.left)
								case replaced => trie.hangRight(replaced)
							} else mutant.notFound(key, leaf) match {
								case e if e.isEmpty => ()
								case newLeaf => root.size_++(); trie.hangRight(disjoint(trie.right, newLeaf))
							}
						case other => mutateOther(root, key, mutant)(trie, other)
					}
					case DiffBit => trie.left match {
						case branch: MutableLongTrie[V, This] with This  => modify(trie, branch)
						case leaf: LongTrieLeaf[V, _, This] =>
							if (leaf.key == key) mutant.updateLeaf(leaf) match {
								case e if e.isEmpty => root.size_--(); parent.hang(trie.right)
								case replaced => trie.hangLeft(replaced)
							} else mutant.notFound(key, leaf) match {
								case e if e.isEmpty => ()
								case newLeaf => root.size_++(); trie.hangLeft(disjoint(trie.left, newLeaf))
							}
						case other => mutateOther(root, key, mutant)(trie, other)
					}
					case _ => mutant.notFound(key, trie) match {
						case e if e.isEmpty => ()
						case newLeaf => root.size_++(); parent.hang(disjoint(trie, newLeaf))
					}
				}
			}
			modify(parent, this.asInstanceOf[MutableLongTrie[V, This] with This])
		}



	}


	/** This variant of the `LongPrefixTrie` treats all keys as if they had their sign (highest) bit flipped.
	  * This essentially swaps the 'negative' and 'positive' branch positions, with negative values
	  * (now stored with the sign bit of `0`) sorting before all positive values. In effect, it changes
	  * the [[OrderedLongTrie#ordering]] to natural `Ordering.Long` instead of unsigned comparison
	  * default to `LongPrefixTrie`. All ordering-related methods inherited from `Sorted` accept
	  * the 'virtual' key values, which have their sign bit flipped before delegating to implementations
	  * inherited from `LongTrieBranch`.
	  */
	trait OrderedLongTrie[+V, +This <: LongPrefixTrie[V, This] with OrderedAs[Long, This]]
		extends LongTrieBranch[V, This] with OrderedAs[Long, This]
	{
//		override implicit def ordering: Ordering[Long] = LongPrefixTrie.ordering
		/** Natural ordering on `Long`, that is one defined by `Ordering.Long`. */
		override implicit def ordering: Ordering[Long] = Ordering.Long

		protected[this] def newBranch(splitPath :Long, left :This, right :This) :This

		/** n-th key in this collection treated as a sequence sorted by 'virtual' (flipped) keys.
		  * @param n index of the leaf/element to retrieve.
		  * @return `flipSign(leaf(idx).key)`
		  */
		override def keyAt(n: Int) :Long = flipSign(leaf(n).key)


		override def rangeImpl(from: Option[Long], until: Option[Long]): This =
			until match {
				case Some(ub) => range(from getOrElse Long.MinValue, ub)
				case _ => from match {
					case Some(lb) => this.from(lb)
					case _ => repr
				}
			}

		override def from(from: Long) :This = fromKey(flipSign(from))

		override def until(until: Long) = keyRange(SignBit, flipSign(until))

		override def range(from: Long, until: Long) :This = keyRange(flipSign(from), flipSign(until))

		/** An iterator over 'virtual' key(s), flipping the sign of the actual key for purpose of comparison with `start`. */
		override def keysIteratorFrom(start: Long): FitIterator[Long] =
			if (center >= 0 && start >= 0) //all our keys are non-negative
				FitIterator.empty
			else if (center < 0 && center != SignBit && start < 0) //all our keys start with `1` bit and `start` starts with `0`
				new FlippedKeyIterator(this)
			else {
				val keyStart = flipSign(start)
				val stack = new Array[Trie[Long, V, This]](64)
				//numerical comparisons of keys are equivalent to unsigned ordering of a trie iff compared numbers have the same sign.
				//Therefore, we need to ensure that start is of the same sign as values in the node we descend to.
				val depth =
				if (center == SignBit && keyStart >= 0) { //a singularity, center is the smallest long and not larger than left trie keys
					stack(0) = this //push ourselves so the iterator retraces back to the right subtrie once it finishes the left one
					fillIteratorStack(left, keyStart, stack, 1) //force search in the left subtree (containing keys with `0` sign bit like start)
				} else //now we are sure that keyStart will have the same sign as all values within requested range
					fillIteratorStack(this, keyStart, stack, 0) //handles also case path=0x8000000000000000L && start >=0
				if (depth<0)
					FitIterator.empty
				else
					new FlippedKeyIterator[V, This](stack, depth)
			}




	}

	/** An iterator over keys in a `LongPrefixTrie`, flipping the sign of each key before returning it.
	  * Extracted here to facilitate reuse.
	  */
	class FlippedKeyIterator[V, T<:Trie[Long, V, T]](stack :Array[Trie[Long, V, T]], top :Int) extends LeafKeyIterator[Long, V, T](stack, top) {
		def this(trie :LongPrefixTrie[V, T]) = this(Trie.initStack(trie, 64), 0)
		override def head = flipSign(topNode.key)
		override def next() = { val res = flipSign(topNode.key); skip(); res }
	}


	/** Unsigned ordering of `Long` values (treating the highest bit as another decimal position rather then sign, and thus
	  * all values as non-negative). This is the order in which `Long` keys are stored internally in the `LongPrefixTrie`.
	  */
	object ordering extends Ordering[Long] {
		override def compare(x: Long, y: Long): Int = flipSign(x) compare flipSign(y)
	}


	@inline final def DeleteKey[V, T<:LongPrefixTrie[V, T]] :TriePatch[Long, V, T] = ReusableDeleteKey.asInstanceOf[TriePatch[Long, V, T]]
	private[this] final val ReusableDeleteKey = new DeleteTrie[Long, Long, StableLongTrieSet]


	/** Returns a `Long` which in the highest bits contains the longest common prefix
	  * of `path1` and `path2`, followed by a single set bit on the highest position
	  * where the values differ.
	  * If `path1==path2`, then their common value is returned.
	  * If their lowest bit i `1`, then this result is indistinguishable
	  * with the case where both values differ only on the lowest bit.
	  * Fot this reason the caller should assertain that `path1!=path2` before
	  * calling this method.
	  */
	@inline final private[palimpsest] def commonPath(path1 :Long, path2 :Long) :Long = {
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


	@inline final private[palimpsest] def flipSign(key :Long) :Long = key ^ 0x8000000000000000L
}
