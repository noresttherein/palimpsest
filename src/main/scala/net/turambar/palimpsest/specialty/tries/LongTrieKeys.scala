package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.{?, Elements, Sure}
import net.turambar.palimpsest.specialty.iterators.FitIterator
import net.turambar.palimpsest.specialty.tries.BinaryTrie._
import net.turambar.palimpsest.specialty.tries.EmptyTrie.EmptyTrieFoundation
import net.turambar.palimpsest.specialty.tries.LongTrieKeys.LongKeyBranch
import net.turambar.palimpsest.specialty.tries.Trie._
import net.turambar.palimpsest.specialty.tries.TrieElements.ElementOf
import net.turambar.palimpsest.specialty.tries.TrieFriends.{EmptyFriend, LeafFriend, TrieOp, TriePredicate}
import net.turambar.palimpsest.specialty.tries.TrieLeaf.TrieLeafFoundation

import scala.annotation.tailrec



/** A binary (that is with all inner nodes having arity of `2`) [[Trie]] implementation with `Long` values
  * as keys, using their binary representation as the paths in the tree. Any node in the trie is associated
  * with a prefix of a `Long` value, that is a `Long` which `n` highest bits have specific values, while `64-n`
  * lower bits are set to zero and understood as 'undetermined'. Those prefixes are exposed as [[LongTrieKeys#prefix]]
  * and are referred to as 'paths' in the trie, as for any two nodes, one is an ancestor of another iff its path
  * is a proper prefix of the path of the descendant. Also, key equality between two node references from a single trie
  * implies that they are the same node instance, so any descendant must extend the length of the common prefix.
  * Furthermore, leaves are always associated with 'full' key values rather than key prefixes (that is, `n==64`), and
  * any branch instances ([[net.turambar.palimpsest.specialty.tries.LongTrieKeys.LongKeyBranch]])
  * are assumed to always represent inner nodes (associated with a proper prefix of a key
  * and not its full extent). It follows then that, for any trie/subtrie root, the partial key associated with it is
  * a prefix of all keys present in this trie (as leaves), and any key which starts with this prefix, if present in a trie,
  * must be a leaf descendant of that node.
  *
  * Default, lexicographic ordering of `Long` values translates to 'unsigned' ordering which, when interpreted as
  * signed values in two's complement encoding, puts all negative values after all positive values,
  * while retaining the ordering between values of the same sign. Thus key `k1` is ordered in a trie before key `k2`
  * iff `flip(k1) <= flip(k2)`, where `flip()` changes the sign bit of the argument.
  *
  * This trait as well as its direct descendants focus on the trie structure, ignoring its values and intended usage
  * (like what type of collection it is and if it is mutable or immutable). Concrete implementations will inherit
  * that behaviour from other base traits.
  *
  * @author Marcin Mościcki
  */
//todo: make This a member type declaration
trait LongTrieKeys[+S <: LongTrieKeys[S, S] with LongTrie, +This <: LongTrieKeys[S, This] with S]
	extends GenericBinaryTrie[Long, S, This] with TrieFriends[Long, LongTrie, S]
{ this :This with LongTrie with BinaryTrieNode =>
	/* Certain properties of binary two's complement number representation this implementation takes advantage of:
	 *   - -n == ~n + 1
	 *   - n & -n : lowest set bit in n
	 *   - n & (n-1) : clears lowest set bit in n
	 *   - bit ^ -bit : mask for the highest k-1 bits, where bit has only the k-th highest bit set.
	 */


	/** Longest common prefix (higher bits) to all keys in this trie; any remaining lower bits are clear.
	  * Empty tries should return simply `0L`.
	  */
	def prefix :Long

	/** Mask for `n` highest bits in a key, where `n` is the length of the prefix forming the path to this node.
	  * In other words, for all leaves in this trie, `l.key & this.mask == this.prefix`. Empty tries return `0L`.
	  * @return a mask with `1` bits on all relevant positions of `this.prefix`.
	  */
	def mask :Long

	/** Path to this node in the trie. For leaves, it is equal to [[Trie#key]]. For branches, it is the center value:
	  * [[prefix]] followed by a single `1` bit and zeros on all lower positions. Empty tries should throw
	  * an `UnsupportedOperationException`.
	  */
	def label :Long

	/** The lower bound (inclusive) for all keys under this trie. Consists of the longest common
	  * prefix in the highest bits, followed by zeros - as such it is equal to `prefix`.
	  */
	def lowerBound :Long = prefix

	/** The upper bound (inclusive) for all keys under this trie. Consists of the longest common
	  * prefix in the highest bits (equal to [[LongTrieKeys#prefix]]), followed by `1` bits in any remaining
	  * lower positions.
	  */
	def upperBound :Long




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

	/** Analogue of [[net.turambar.palimpsest.specialty.sets.OrderedSet#to]] working on keys in this trie. Provides a basis for subclasses to implement
	  * [[net.turambar.palimpsest.specialty.ordered.OrderedVals]], but doesn't extend the trait itself here in order to allow the subclasses to
	  * provide the type parameter for the 'key' argument themselves, possibly different from `Long` (but likely with homomorphic ordering).
	  * @param end upper bound (inclusive) on the keys to select from this trie as a new trie.
	  * @return a trie of the same root type as `this` containing only leaves with keys larger than `start` (and all of them).
	  */
	def toKey(end :Long) :This

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
	def keyRangeImpl(from: ?[Long], until: ?[Long]): This


	/** Returns an iterator traversing all elements starting with the given key. */
	def iteratorFrom[@specialized(Elements) E](elems :ElementOf[E, S])(start :Long) :FitIterator[E]




	/** Primary target method of [[net.turambar.palimpsest.specialty.tries.TrieFriends.juxtapose]]. Recursively descends
	  * down two tries simultaneously, comparing their structure and invoking appropriate callbacks of the operator.
	  * @param first a subtrie of left-hand side argument of the operator (this).
	  * @param second a subtrie of right-hnd side argument of the operator (argument of the public `juxtapose` method).
	  * @param op operator implementation
	  */
	protected[this] final def juxtaposeBoth[U >: S <: Trie[Long, U] with LongTrie, @specialized(TrieOpRes) O]
	                                       (first :U, second :U)(op :TrieOp[U, O]) :O =
		first match {
			case branch1 :LongKeyBranch[U] => second match {
				case branch2 :LongKeyBranch[U] =>
					val path1 = branch1.center; val path2 = branch2.center
					val diff = path1 ^ path2
					if (diff==0L)
						op.reduce(first)(juxtaposeBoth(branch1.left, branch2.left)(op), juxtaposeBoth(branch1.right, branch2.right)(op))
					else {
						val point1 = path1 & -path1 //lowest set bit in path1, that is the bit dividing branch1.left and branch1.right
						val mask1 = -point1 ^ point1 //mask for the common prefix of all elements in branch1
						if ((diff & mask1) == 0L) //path1 is prefix of path2 because path1 is equal to path2 on the span of path1
							if ((path2 & point1) == 0L) //branch2 belongs with the left child of branch1
								op.reduce(first)(juxtaposeBoth(branch1.left, second)(op), op.mapFirst(branch1.right, emptyTrie))
							else
								op.reduce(first)(op.mapFirst(branch1.left, emptyTrie), juxtaposeBoth(branch1.right, second)(op))
						else {
							val point2 = path2 & -path2; val mask2 = -point2 ^ point2
							if ((diff & mask2)==0L) //path2 is prefix of path1
								if ((path1 & point2) == 0L)
									op.reduce(second)(juxtaposeBoth(first, branch2.left)(op), op.mapSecond(emptyTrie, branch2.right))
								else
									op.reduce(second)(op.mapSecond(first, branch2.left), juxtaposeBoth(emptyTrie, branch2.right)(op))
							else //sets are disjoint, as they differ in the common span of path1 and path2
								op.mapMismatch(first, second)
						}
					}

				case _ =>
					juxtaposeFirst(first, second, second.key)(op)
			}
			case _  =>
				juxtaposeSecond(first.key, first, second)(op)
		}


	/** An implementation method used by [[net.turambar.palimpsest.specialty.tries.TrieFriends.juxtapose]]. Invoked when
	  * the second argument is reduced to a leaf - either from [[LongTrieKeys#juxtaposeBoth]] or is a singleton trie
	  * to start with. Recursively descends down the `first` trie to find the leaf for the given key, if it exists.
	  * @param first a subtrie of the left-hand side argument of the operator (this).
	  * @param leaf a leaf from the right-hand side argument of the operator (trie argument of `juxtapose`).
	  * @param key key stored in `leaf` (to avoid repeated calls to the accessor)
	  * @param op operator implementation.
	  */
	protected[this] final def juxtaposeFirst[U >: S <: Trie[Long, U] with LongTrie, @specialized(TrieOpRes) O]
	                                        (first :U, leaf :U, key :Long)(op :TrieOp[U, O]) :O =
		first match {
			case branch :LongKeyBranch[U] =>
				val rpath = branch.center; val DiffBit = rpath & -rpath
				(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
					case 0 =>
						op.reduce(first)(op.mapFirst(branch.left, emptyTrie), juxtaposeFirst(branch.right, leaf, key)(op))
					case DiffBit =>
						op.reduce(first)(juxtaposeFirst(branch.left, leaf, key)(op), op.mapFirst(branch.right, emptyTrie))
					case _ =>
						op.mapMismatch(first, leaf)
				}

			case _ if first.key == key => op.mapMatch(first, leaf)

			case _ => op.mapMismatch(first, leaf)
		}


	/** An implementation method used by [[net.turambar.palimpsest.specialty.tries.TrieFriends.juxtapose]]. Invoked when
	  * the first argument is reduced to a leaf - either from [[LongTrieKeys#juxtaposeBoth]] or is a singleton trie
	  * to start with. Recursively descends down the `second` trie to find the leaf for the given key, if it exists.
	  * @param leaf a leaf from the left-hand side argument of the operator (this).
	  * @param second a subtrie of the right-hand side argument of the operator (trie argument of `juxtapose`).
	  * @param key key stored in `leaf` (to avoid repeated calls to the accessor)
	  * @param op operator implementation.
	  */
	protected[this] final def juxtaposeSecond[U >: S <: Trie[Long, U] with LongTrie, @specialized(TrieOpRes) O]
	                                         (key :Long, leaf :U, second :U)(op :TrieOp[U, O]) :O =
		second match {
			case branch :LongKeyBranch[U] =>
				val rpath = branch.center; val DiffBit = rpath & -rpath
				(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
					case 0 =>
						op.reduce(second)(op.mapSecond(emptyTrie, branch.left), juxtaposeSecond(key, leaf, branch.right)(op))
					case DiffBit =>
						op.reduce(second)(juxtaposeSecond(key, branch.left, leaf)(op), op.mapSecond(emptyTrie, branch.right))
					case _ =>
						op.mapMismatch(leaf, second)
				}
			case _ if second.key == key => op.mapMatch(leaf, second)

			case _ => op.mapMismatch(leaf, second)
		}




	protected[this] final def correlateBoth[U >: S <: Trie[Long, U] with LongTrie]
	                                       (first :U, second :U)(op :TriePredicate[U]): Boolean =
		first match {
			case branch1 :LongKeyBranch[U] => second match {
				case branch2 :LongKeyBranch[U] =>
					val path1 = branch1.center; val path2 = branch2.center
					val diff = path1 ^ path2
					if (diff == 0L) {
						correlateBoth(branch1.left, branch2.left)(op) && correlateBoth(branch1.right, branch2.right)(op)
					} else {
						val point1 = path1 & -path1 //lowest set bit in path1, that is the bit dividing branch1.left and branch1.right
						val mask1 = -point1 ^ point1 //mask for the common prefix of all elements in branch1
						if ((diff & mask1) == 0L) //path1 is prefix of path2 because path1 is equal to path2 on the span of path1
							if ((path2 & point1) == 0L) //branch2 belongs with the left child of branch1
								op.mapFirst(branch1.right, second) && correlateBoth(branch1.left, second)(op)
							else
								op.mapFirst(branch1.left, second) && correlateBoth(branch1.right, second)(op)
						else {
							val point2 = path2 & -path2; val mask2 = -point2 ^ point2
							if ((diff & mask2)==0L) //path2 is prefix of path1
								if ((path1 & point2) == 0L)
									op.mapSecond(first, branch2.right) && correlateBoth(first, branch2.left)(op)
								else
									op.mapSecond(first, branch2.left) && correlateBoth(first, branch2.right)(op)
							else //sets are disjoint, as they differ in the common span of path1 and path2
								op.mapMismatch(first, second)
						}
					}
				case _ =>
					correlateFirst(first, second, second.key)(op)
			}
			case _ =>
				correlateSecond(first.key, first, second)(op)
		}


	@tailrec
	protected[this] final def correlateFirst[U >: S <: Trie[Long, U] with LongTrie]
	                                        (first :U, leaf :U, key :Long)(op :TriePredicate[U]): Boolean =
		first match {
			case branch :LongKeyBranch[U] =>
				val rpath = branch.center; val DiffBit = rpath & -rpath
				(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
					case 0L =>
						if (op.mapFirst(branch.left, leaf)) correlateFirst(branch.right, leaf, key)(op)
						else false
					case DiffBit =>
						if (op.mapFirst(branch.right, leaf)) correlateFirst(branch.left, leaf, key)(op)
						else false
					case _ =>
						op.mapMismatch(first, leaf)
				}
			case _ if first.key == key => op.mapMatch(first, leaf)

			case _ => op.mapMismatch(first, leaf)
		}


	@tailrec
	protected[this] final def correlateSecond[U >: S <: Trie[Long, U] with LongTrie]
	                                         (key :Long, leaf :U, second :U)(op :TriePredicate[U]): Boolean =
		second match {
			case branch :LongKeyBranch[U] =>
				val rpath = branch.center; val DiffBit = rpath & -rpath
				(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
					case 0L =>
						if (op.mapSecond(leaf, branch.left)) correlateSecond(key, leaf, branch.right)(op)
						else false
					case DiffBit =>
						if (op.mapSecond(leaf, branch.right)) correlateSecond(key, branch.left, leaf)(op)
						else false
					case _ =>
						op.mapMismatch(leaf, second)
				}
			case _ if second.key == key => op.mapMatch(leaf, second)

			case _ => op.mapMismatch(leaf, second)
		}




	def canEqual(that :Any) :Boolean = that.isInstanceOf[LongTrieKeys[_, _]]

}






object LongTrieKeys {
	import java.lang.Long.{MIN_VALUE => SignBit, hashCode => keyHash}



	/** Base trait for empty tries indexed by `Long` keys. All default [[LongTrieKeys]] implementations provided here
	  * assume that it is never a part of a larger trie (that is, can never be a child of a [[LongKeyBranch]]) and
	  * always exists only on its own as a specific empty collection implementation or as a marker serving a role
	  * similar to `None :Option[_]` when used instead of a [[LongKeyLeaf]].
	  * @tparam This public self type of a particular trie implementation being the base type of an extending class.
	  *              Not constrained here for convenience, but concrete implementations will have
	  *              `this.type &lt;: This &lt;: LongTrieKeys &lt;: Trie[Long, _, _]`.
	  */
	abstract class GenericEmptyLongKeys[+S <: LongTrieKeys[S, S] with LongTrie, +This <: LongTrieKeys[S, This] with S]
		extends EmptyTrieFoundation[Long, This] with GenericEmptyBinaryTrie[Long, S, This] with EmptyFriend[Long, LongTrie, S]
		   with LongTrieKeys[S, This]
	{ this :This with LongTrie =>

		override final def label :Nothing = throw new UnsupportedOperationException("EmptyLongKeys.path")

		@inline final override def prefix = 0L
		@inline final override def mask = 0L
		@inline final override def lowerBound = 0L
		@inline final override def upperBound :Long = -1L //only `1` bits


		override def fromKey(start: Long): This = this
		override def untilKey(end :Long) :This = this
		override def toKey(end :Long) :This = this
		override def keyRange(start: Long, end: Long): This = this
		override def keyRangeImpl(from: ?[Long], until: ?[Long]): This = this

		override def iteratorFrom[@specialized(Elements) E](elements :ElementOf[E, S])(key :Long) :FitIterator[E] =
			FitIterator.empty[E]


		override def equals(that :Any) :Boolean = that.isInstanceOf[GenericEmptyLongKeys[_, _]]

		override def hashCode :Int = 31
	}



	type EmptyLongKeys[+T <: GenericEmptyLongKeys[T, T] with LongTrie] = GenericEmptyLongKeys[T, T]




	/** A leaf in a trie indexed by `Long` keys, storing exactly one key-value two and terminating any path leading to it.
	  * @param k immutable value of the key associated with this leaf
	  * @tparam This self-type being the common root type of all node types in a larger trie implementation any extending class may be part of.
	  *              Enforced here to be self-type of this, that is `this.type <: This` for any instance of this class.
	  */
	abstract class GenericLongKeyLeaf[+S <: LongTrieKeys[S, S] with LongTrie, +This <: LongTrieKeys[S, This] with S](k :Long)
		extends TrieLeafFoundation[Long, This] with GenericBinaryTrieLeaf[Long, S, This] with LeafFriend[Long, LongTrie, S]
		   with LongTrieKeys[S, This]
	{ this :This with LongTrie =>

		@inline final override def key :Long = k
		@inline final override def prefix :Long = k
		@inline final override def label :Long = k
		@inline final override def mask :Long = -1L
		@inline final override def lowerBound :Long = k
		@inline final override def upperBound :Long = k


		override def fromKey(start: Long): This = if (flipSign(start) <= flipSign(key)) this else emptyTrie
		override def untilKey(end :Long) :This = if (flipSign(key) < flipSign(end)) this else emptyTrie
		override def toKey(end :Long) :This = if (flipSign(key) <= flipSign(end)) this else emptyTrie

		override def keyRange(start: Long, end: Long): This = {
			val v = flipSign(key)
			if (flipSign(start) <= v && v < flipSign(end)) this else emptyTrie
		}

		override def keyRangeImpl(from: ?[Long], until: ?[Long]): This = {
			val v = flipSign(key)
			if ((from.isEmpty || flipSign(from.get) <= v) && (until.isEmpty || v < flipSign(until.get)))
				this
			else emptyTrie
		}

		override def iteratorFrom[@specialized(Elements) E](elems :ElementOf[E, S])(start :Long) :FitIterator[E] =
			if (flipSign(start) <= flipSign(key)) FitIterator.one(elems.elementOf(this))
			else FitIterator.empty[E]


		override def juxtapose[T >: S <: Trie[Long, T] with LongTrie, @specialized(TrieOpRes) O](other :T)(op :TrieOp[T, O]) :O =
			if (other.isEmpty) op.mapFirst(this, other)
			else juxtaposeSecond[T, O](key, this, other)(op)

		override def correlated[T >: S <: Trie[Long, T] with LongTrie](other :T)(pred :TriePredicate[T]) :Boolean =
			if (other.isEmpty) pred.mapFirst(this, other)
			else correlateSecond(key, this, other)(pred)



		override def canEqual(that :Any) :Boolean = that.isInstanceOf[GenericLongKeyLeaf[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case leaf :LongKeyLeaf[_] => (leaf eq this) || leaf.canEqual(this) && leaf.key == key
			case _ => false
		}

		override def hashCode :Int = keyHash(key)
	}


	type LongKeyLeaf[+T <: LongTrieKeys[T, T] with LongTrie] = GenericLongKeyLeaf[T, T]






	/** An inner node in a trie which keys (paths to the leaves) followDown the binary representation of `Long` values
	  * from the highest bit.
	  * @param path a `Long` value which, in its highest bits, is equal to all the values (leaves) in this trie.
	  *             Therefore its `n` highest bits, `0<=n<64` is the 'path' in any larger trie to this node
	  *             (although it can exists as an independent root itself) and defines the longest common prefix
	  *             to all values in this trie. Bit `n+1` (from the highest bit) must be set, and is followed by zeros
	  *             in the lowest `63-n` positions, thus the lowest set bit of this value denotes the end of the prefix
	  *             common to both children under this node, while at the same time being equal to all values
	  *             in the `right` subtrie on its `n+1` highest bits (including the terminating bit).
	  *             This value is later exposed via [[GenericLongKeyBranch#center]] - see its documentation to see how to extract
	  *             all in this information by bitwise operations.
	  * @param zero A non-empty trie containing all leaves in this trie which keys have zero on the position of the splitting bit.
	  * @param one  A non-empty trie containing all leaves in this trie which keys have one on the position of the splitting bit.
	  * @tparam This self-type defining the 'public' common trait of all node types in a given trie implementations.
	  */
	abstract class GenericLongKeyBranch[+S <: LongTrieKeys[S, S] with LongTrie, +This <: LongTrieKeys[S, This] with S]
	                                   (private[this] var path :Long, zero :S, one :S)
		extends GenericBinaryTrieBranch[Long, S, This](zero, one) with LongTrieKeys[S, This]
	{ self :This with LongTrie =>

		/** Create a new trie node with the given children and calculate the common center label. */
		def this(left :S, right :S) = this(centerLabel(left.label, right.label), left, right)

		/** Supertype of all branch nodes in this trie. */
		protected[this] type LongBranch = GenericLongKeyBranch[S, S]

		/** Same as [[GenericLongKeyBranch#center]] - the upper (exclusive) bound for keys in the left subtrie and lower bound
		  * (inclusive) for keys in the right subtrie.
		  * @return `prefix | splitBit`
		  * @see [[GenericLongKeyBranch#center]]
		  */
		@inline final override def label :Long = path

		/** A 'middle' value of this set when treated as the longest possible range of values with common highest bits.
		  * Consists of the bits common to all keys in this set followed by a single set bit in the first position
		  * (from the highest bit) for which there are keys in this set with both values. It is a central value in the sense
		  * that it is both the higher bound (exclusive) for all values in the left subtrie and lower bound (inclusive)
		  * for values in the right subtrie. It carries full information about the path to this node: both the value
		  * of the common prefix and effectively its length.
		  *
		  * The split bit (position of the bit in which values from the left and right children differ) is the lowest 'one'
		  * bit of this value and is calculated as `center & -center` (as `-center == ~(center-1)`, a mask equal to `center`
		  * only from the `splitBit` position down). At the same time, clearing this bit on `center` yields the prefix
		  * common to all keys. In turn, the mask for the significant (high) bits of `center`, including `prefix` and
		  * `splitBit` is `~splitBit + 1` (or, equivalent, `-splitBit`) and clearing the split bit in it yields the mask
		  * for only the significant higher bits in `prefix`: `splitBit ^ -splitBit`.
		  *
		  * @see [[splitBit]]
		  * #see [[key]]
		  * @return value equivalent to [[prefix]] `|` [[splitBit]]
		  */
		@inline final def center :Long = path

		@inline final protected def center_=(delimitedPrefix :Long) :Unit = path = delimitedPrefix

		/** Partial binary representation of a `Long` value, with highest bits set to the values which are common
		  * to all keys in this trie. All bits below this common prefix are clear. Represents the 'path' to this node
		  * in a larger trie in the sense that for all ancestor nodes `x.prefix` is a proper prefix of this node.
		  */
		@inline final def prefix :Long = path & (path-1L) //clears lowest set bit

		/** Mask for the single highest bit separating the keys from left and right subtrees. `left` is assumed to contain
		  * all values for which this bit is clear, while `right` to contain all values with this bit set.
		  * It is the highest bit for which there are keys of both values in this trie, i.e. all elements
		  * are equal on the higher bits.
		  */
		@inline final def splitBit :Long = path & -path //mask for the single lowest set bit

		/** Mask for bits common to all keys in this trie, formed by a sequence of any number of `1` bits starting
		  * from the highest bit, followed by zero bits in the remaining lower positions.
		  * The length of the mask represents the real span of the [[prefix]] value.
		  * @return value such that `(mask & k)==prefix` for all keys `k` in this set (including inner nodes)
		  */
		@inline final def mask :Long = { val bit = path & -path; -bit ^ bit }



		/** Lowest possible key value in this set (treated as unsigned), i.e. the prefix to this node followed exclusively by zeros. */
		@inline final override def lowerBound :Long = center & (center-1)

		/** Highest possible key value in this set (treated as unsigned), i.e. the prefix to this node followed exclusively by 'one' bits. */
		@inline final override def upperBound :Long = center | (center-1)



		/** Value used to determine in which subtree of this set the given element belongs.
		  *
		  * @param elem any value to potentially add or remove from this set.
		  * @return `0` if the element belongs to the `right` tree,
		  *         [[splitBit]] if the element belongs to the `left` tree,
		  *         and any other value if the element doesn't share the common path of this set.
		  */
		@inline final protected[this] def assign(elem :Long) :Long = {
			val wrongBitsSet = elem & -path //bits set in elem at or above split bit which are clear in prefix
			(wrongBitsSet | -elem & path) ^ //mask for differing bits between elem and path at or above the split bit, but with split bit set
				(wrongBitsSet & path) //right-hand is simply elem & (-path & path), that is split bit value on elem
			//result has opposite value to elem at the split bit and additionally contains mask for differing positions above it
		}


		/** A mask for bits in `key` which differ from `prefix` above `splitBit`. */
		@inline final def mismatchedBits(key :Long) :Long = {
			//(bits in key absent in path from splitBit up) | (mask for clear bits in key set in  prefix) then clear split bit
			(key & -path | ~key & path) ^ path & -path
		}


		/** A key belongs under a branch node if `key & mask == prefix` (or, equivalent, `mismatchedBits(key) == splitBit`). */
		override def belongs(key :Long) :Boolean =
			//'one bits in `key` above the `splitBit` which aren't set in `center`, when combined with one bits in `center`
			// which aren't set in `key`, total to exactly the mask for the split bit => `key & mask == prefix`':
			(key & -center | ~key & center) == (center & -center)




		override def keyRangeImpl(from: ?[Long], until: ?[Long]): This = until match {
			case ub :Sure[Long] => from match {
				case lb :Sure[Long] =>
					keyRange(lb.value, ub.value)
				case _ => export(keyRange(0L, ub.value))
			}
			case _ => from match {
				case lb :Sure[Long] => export(fromKey(lb.value))
				case _ => copy
			}
		}


		/** Selects the subtrie of the given trie containing all leaves with keys between `lo` and `hi`, using natural ordering.
		  * @param trie trie to select range of, with all leaves having keys of the same signum
		  * @param lo lower bound (inclusive) for keys to be included, must be of the same signum as all keys in `trie`.
		  * @param hi higher bound (exclusive) for keys to be included, must be of the same signum as all keys in `trie`.
		  */
		protected[this] final def keyRange(trie :S, lo :Long, hi :Long) :This =
			trie match {
				case branch :LongBranch =>
					val split = branch.center
					if (lo <= branch.lowerBound && hi > branch.upperBound) share(trie)
					else if (hi <= split) keyRange(branch.left, lo, hi)
					else if (lo >= split) keyRange(branch.right, lo, hi)
					else {
						val l = keyRange(branch.left, lo, hi)
						val r = keyRange(branch.right, lo, hi)
						if (l.isEmpty) r
						else if (r.isEmpty) l
						else patchBranch(branch)(l, r)
					}

				case _ => //leaf
					val v = trie.key
					if (lo <= v && v < hi) likeLeaf(trie) else emptyTrie
			}


		/** Selects the subtrie of the given trie containing all leaves with keys greater or equal `lo`, using natural ordering.
		  * @param trie to to select range of, with all keys of the same signum as `lo`
		  * @param lo lower bound on the keys to select
		  */
		protected[this] final def keysFrom(trie :S, lo :Long) :This = trie match {
			case branch :LongBranch =>
				val split = branch.center
				if (lo <= branch.lowerBound) share(trie)
				else if (lo >= split) keysFrom(branch.right, lo)
				else {
					val l = keysFrom(branch.left, lo)
					if (l.isEmpty) share(branch.right)
					else patchLeft(branch, l)
				}
			case _ if lo <= trie.key => likeLeaf(trie)

			case _ => emptyTrie
		}


		/** Selects the subtrie of this trie with keys greater or equal to from, based on '''unsigned''' ordering.
		  * Binary prefix ordering follows ''unsigned'' ordering of `Long` values. This ordering is equivalent
		  * to natural ordering based on signed `Long` values for values of the same sign, but all negative keys
		  * compare greater than any non-negative key.
		  *
		  * Taking advantage of the above mentioned facts, this method resigns to natural comparing of the keys
		  * after narrowing down the result to the branch corresponding to the appropriate sign, if needed.
		  */
		override def fromKey(from: Long) :This =
			if (center == SignBit) //left child contains keys >= 0 (0 sign bit), right child contains keys < 0 (1 sign bit)
				if (from < 0) export(keysFrom(right, from)) //narrow the search down to the right subtrie
				else export(patchLeft(this, keysFrom(left, from)))
			else if (center < 0) //all keys start with `1` sign bit
				if (from >= 0) copy
				else export(keysFrom(this, from))
			else //all keys start with `0` sign bit
				if (from < 0) emptyTrie
				else export(keysFrom(this, from))


		override def untilKey(until: Long) :This = keyRange(0, until)


		override def toKey(end :Long) :This =
			if (end == -1L) this //only '1' bits, avoid overflow
			else if (end == Long.MaxValue) //'0' followed by '1's; avoid overflow
				if (center > 0) this
				else if (center == SignBit) export(share(left))
				else emptyTrie
			else export(keyRange(0, end + 1)) //safe from overflow now


		override def keyRange(from: Long, until: Long) :This =
			//verify the sign of from and until and delegate to the appropriate child(ren)
			if (center==SignBit) //left contains keys starting with 0, right contains keys starting with 1
				if (from < 0)
					if (until >= 0 || until <= from) emptyTrie
					else export(keyRange(right, from, until))
				else if (until >= 0)
					if (until <= from) emptyTrie
					else export(keyRange(left, from, until))
				else patchBranch(this)(keysFrom(left, from), keyRange(right, SignBit, until))
			else if (center < 0) //all keys start with `1` bit
				if (until >= 0) emptyTrie
				else if (from >= 0) export(keyRange(this, SignBit, until))
				else if (until <= from) emptyTrie
				else export(keyRange(this, from, until))
			else //center >= 0, that is has `0` first bit
				if (from < 0) emptyTrie
				else if (until < 0) export(keysFrom(this, from))
				else if (until<=from) emptyTrie
				else export(keyRange(this, from, until))




		def iteratorFrom[@specialized(Elements) E](elems :ElementOf[E, S])(start :Long) :FitIterator[E] =
			if (center >= 0 && (start < 0 || start > upperBound))
				FitIterator.empty[E] //all our keys start with '0' while start falls after our range
			else if (center < 0 && center != SignBit && (start >= 0 || start < lowerBound)) //all our keys start with `1` bit and `start` starts with `0`
			     new LeafElementIterator[E, Long, S](elems, this, 64) //iterate over the whole trie
			else {
				val stack = new Array[BinaryTrie[Long, S]](64)
				//numerical comparisons of keys are equivalent to unsigned ordering of a trie iff compared numbers have the same sign.
				//Therefore, we need to ensure that start is of the same sign as values in the node we descend to.
				val depth =
					if (center == SignBit && start >= 0) { //a singularity, center is the smallest long and not larger than left trie keys
						stack(0) = this //push ourselves so the iterator retraces back to the right subtrie once it finishes the left one
						fillIteratorStack(start, left, stack, 1) //force search in the left subtree (containing keys with `0` sign bit like start)
					} else //now we are sure that keyStart will have the same sign as all values within requested range
						fillIteratorStack(start, this, stack, 0) //handles also case path=0x8000000000000000L && start >=0
				if (depth < 0)
					FitIterator.empty[E]
				else
					new LeafElementIterator[E, Long, S](elems, stack, depth)
			}



		protected[this] def fillIteratorStack(fromKey :Long, stack :Array[BinaryTrie[Long, S]]) :Int =
			if (center >= 0 && (fromKey < 0 || fromKey > upperBound)) //all our keys start with '0' while fromKey starts with 1 or is larger than any possible key
				-1
			else if (center < 0 && center != SignBit && (fromKey >= 0 || fromKey < lowerBound)) {//all our keys start with `1` bit and `start` starts with `0`
				stack(0) = this //iterate over the whole trie
				0
			} else {
				val stack = new Array[BinaryTrie[Long, S]](64)
				//numerical comparisons of keys are equivalent to unsigned ordering of a trie iff compared numbers have the same sign.
				//Therefore, we need to ensure that start is of the same sign as values in the node we descend to.
				if (center == SignBit && fromKey >= 0) { //a singularity, center is the smallest long and not larger than left trie keys
					stack(0) = this //push ourselves so the iterator retraces back to the right subtrie once it finishes the left one
					fillIteratorStack(fromKey, left, stack, 1) //force search in the left subtree (containing keys with `0` sign bit like start)
				} else //now we are sure that keyStart will have the same sign as all values within requested range
					fillIteratorStack(fromKey, this, stack, 0) //handles also case path=0x8000000000000000L && start >=0
			}



		/** Build a stack for the iterator representing a path to the first iterated leaf.
		  * The stack is implemented as an array of size 64 (maximum depth of a Long-keyed trie) and index in the array
		  * of the top element on the stack.
		  * The path will cut the trie in two, with the right-hand side containing all the elements to be iterated over.
		  * If there are no elements to iterate, the stack will be empty. Otherwise on its top resides a singleton leaf
		  * with the first/next element of the created iterator. Below it are all branch nodes on the path leading to it,
		  * from which we descended into the left tree, and thus their whole right subtree remains to be iterated.
		  * advancing the iterator retreats on the stack to take most-recent possible right turn
		  * (removing the node to which we retreated from the stack, and advancing again with a preference for the left child.
		  *
		  * The implementation performs normal numeric comparisons on the key fragments to find the first leaf greater
		  * or equal to `fromKey` by binary search. It takes advantage of three facts:
		  *   - `fromKey` and all keys under `trie` are of the same sign and thus comparable (unsigned ordering
		  *     of nodes is consistent with natural ordering)
		  *   - the binary prefix common to all elements in the trie, when interpreted as a numerical value, is the set's lower bound;
		  *   - for branch nodes, their center value (delimited path :common prefix followed by a single set bit separating the values
		  *     of its left and right subtree) is the lower bound of the right subtree and upper bound of the left subtrie,
		  *     as all elements in the left trie have the terminator bit clear, while all elements in the right subtrie
		  *     have the bit set.
		  *  @param fromKey lower (inclusive) bound for keys to be iterated over
		  *  @param trie currently considered node on the path to first node for the iterator.
		  *              '''All keys must be of the same sign as fromKey'''.
		  *  @param stack array serving as iterator stack, containing all branch nodes which right children are to be iterated,
		  *               with later elements being those earlier in the iteration order.
		  *  @param stackSize current stack size (index of the first free cell in the array)
		  *  @return index of the top element of the stack or `-1` for an empty stack (and iterator).
		  */
		@tailrec private[this] final def fillIteratorStack(fromKey :Long, trie: S, stack :Array[BinaryTrie[Long, S]], stackSize: Int): Int =
			trie match {
				case branch: LongBranch =>
					if (branch.center <= fromKey) { //bin search our way to the leaf
						if (fromKey <= branch.upperBound) //belongs in the right subtrie
							fillIteratorStack(fromKey, branch.right, stack, stackSize) //don't put the `trie` node on the stack
						else {
							stack(stackSize) = null //belongs after current branch, lets go up and take first turn right, removing it from the stack
							if (stackSize == 0)
								-1 //no more nodes to search
							else
								fillIteratorStack(fromKey, stack(stackSize - 1).asInstanceOf[LongBranch].right, stack, stackSize - 1)
						}
					} else {
						stack(stackSize) = branch
						fillIteratorStack(fromKey, branch.left, stack, stackSize + 1)
					}
				case _ =>
					if (fromKey <= trie.key) {
						stack(stackSize) = trie //we've found the minimal leaf greater than start
						stackSize
					} else{
						stack(stackSize) = null.asInstanceOf[S]
						if (stackSize == 0)
							-1 //end of the road, all leaves are smaller than start
						else
							fillIteratorStack(fromKey, stack(stackSize - 1).asInstanceOf[LongBranch].right, stack, stackSize - 1) //retreat back to the right tree
					}
			}



		override def viewNode(key: Long): S = {
			@tailrec def dive(trie :S) :S = trie match {
				case branch :LongBranch =>
					val rpath = branch.center; val DiffBit = rpath & -rpath
					(key & -rpath | ~key & rpath) ^ (key & DiffBit) match {
						case 0 => dive(branch.right)
						case DiffBit => dive(branch.left)
						case _ => emptyTrie
					}
				case _  if trie.key == key => trie

				case _ => emptyTrie
			}
			dive(self)
		}


		override def nodeFor(key :Long) :This = likeLeaf(viewNode(key))




		override def foldPath[U >: S <: Trie[Long, U] with LongTrie, @specialized(TrieOpRes) O]
		                     (op: Trie.FoldPath[Long, U, O])(key: Long): O = {
			def descend(node :S) :O = node match {
				case branch :LongBranch =>
					val divider = branch.center
					val DiffBit = divider & -divider
					(key & -divider | ~key & divider) ^ (key & DiffBit) match {
						case 0 =>
							op.foldUp(key, node, branch.right, descend(branch.right))
						case DiffBit =>
							op.foldUp(key, node, branch.left, descend(branch.left))
						case _ =>
							op.whenNoKey(key, node)
					}
				case _ if node.key == key =>
					op.whenKeyExists(key, node)
				case _ =>
					op.whenNoKey(key, node)
			}
			descend(this)
		}



		protected[this] final def patchLeftChild[F >: S <: BinaryTrie[Long, F], U >: This <: Trie[Long, U]]
		                                        (op :BinaryTriePatch[Long, F, U], key :Long)(parent :LongBranch) :U =
			parent.left match {
				case branch :LongBranch =>
					val divider = branch.center
					val DiffBit = divider & -divider
					val replacement =
						(key & -divider | ~key & divider) ^ (key & DiffBit) match {
							case 0 => patchRightChild(op, key)(branch)
							case DiffBit => patchLeftChild(op, key)(branch)
							case _ => op.whenNoKey(key, branch.asTrie)
						}
					op.patchLeft(parent, replacement)
				case leaf if leaf.key == key =>
					val replacement = op.whenKeyExists(key, leaf)
					if (replacement.isEmpty)
						share(parent.right) //make sure it's not modifiable by becoming a child of a a mutable node
					else
						op.patchLeft(parent, replacement)
				case leaf =>
					op.patchLeft(parent, op.whenNoKey(key, leaf))
			}



		protected[this] final def patchRightChild[F >: S <: BinaryTrie[Long, F], U >: This <: Trie[Long, U]]
		                                         (op :BinaryTriePatch[Long, F, U], key :Long)(parent :LongBranch) :U =
			parent.right match {
				case branch :LongBranch =>
					val divider = branch.center
					val DiffBit = divider & -divider
					val replacement =
						(key & -divider | ~key & divider) ^ (key & DiffBit) match {
							case 0 => patchRightChild(op, key)(branch)
							case DiffBit => patchLeftChild(op, key)(branch)
							case _ => op.whenNoKey(key, branch.asTrie)
						}
					op.patchRight(parent, replacement)
				case leaf if leaf.key == key =>
					val replacement = op.whenKeyExists(key, leaf)
					if (replacement.isEmpty) //consider: moving share(S) to the patch class will loosen type constraints
						share(parent.left) //make sure it's not modifiable by becoming a child of a a mutable node
					else
						op.patchRight(parent, replacement)
				case leaf =>
					op.patchRight(parent, op.whenNoKey(key, leaf))
			}


		override def patchKey[F >: S <: BinaryTrie[Long, F], U >: This <: Trie[Long, U]]
		                     (op :BinaryTriePatch[Long, F, U])(key :Long) :U =
		{
			val DiffBit = path & -path
			(key & -path | ~key & path) ^ (key & DiffBit) match {
				case 0 => patchRightChild(op, key)(this)
				case DiffBit => patchLeftChild(op, key)(this)
				case _ => op.whenNoKey(key, this)
			}
		}

		override def juxtapose[T >: S <: Trie[Long, T] with LongTrie, @specialized(TrieOpRes) O]
		                      (other :T)(op :TrieOp[T, O]) :O =
			if (other.isEmpty) op.mapFirst(this, other)
			else juxtaposeBoth[T, O](this, other)(op)


		override def correlated[T >: S <: Trie[Long, T] with LongTrie](other :T)(op :TriePredicate[T]) :Boolean =
			if (other.isEmpty) op.mapFirst(this, other)
			else correlateBoth[T](this, other)(op)





		override protected def maxDepth = 64


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[GenericLongKeyBranch[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case branch :GenericLongKeyBranch[_, _] => //avoid checking canEqual for every inner node; leaves will differ anyway
				(this eq branch) || center == branch.center && left == branch.left && right == branch.right
			case _ => false
		}

		override def hashCode :Int = ((keyHash(path) * 31) + left.hashCode) * 31 + right.hashCode

	}




	type LongKeyBranch[+This <: LongTrieKeys[This, This] with LongTrie] = GenericLongKeyBranch[This, This]






	/** An implementation-oriented trait for [[LongTrieKeys]] branches which can be modified after creation.
	  * As it allows for arbitrary substitution of nodes which can possibly violate the invariants of the implemented
	  * interface, it shouldn't be exposed to the outside world (that is, it should be extended only by non-public
	  * classes rather than their public interfaces).
	  */
	trait MutableLongKeyBranch[S <: LongTrieKeys[S, S] with LongTrie, M <: LongTrieKeys[S, M] with S]
		extends GenericLongKeyBranch[S, M] with MutableBinaryTrie[Long, S, M] with MutableBinaryTrieBranch[Long, S, M]
	{ this :M =>

		private type MutableLongBranch = MutableLongKeyBranch[S, M] with S

		@inline final private[tries] def updateTrie(t :S) :Unit =
			if ((t.label & center) == center) right = t
			else left = t


		override protected def branchLike(branch :S)(l :S, r :S) :M with MutableLongKeyBranch[S, M]



		override def patchKey(patch :BinaryTriePatch[Long, S, M], root :MutableTrieOwner[M])(key :Long) :Boolean = {
			def replace(parent :MutableLongBranch, child :MutableLongBranch)(old :S, replacement :M, sibling :S) :Boolean =
				if (replacement eq old)
					false
				else {
					if (replacement.isEmpty)
						if (parent!=null)
							parent.replace(child.asTrie, sibling)
						else
							root.updateTrie(trieLike(sibling))
					else
						if (child.left eq old) child.setLeft(replacement)
						else child.setRight(replacement)
					true
				}

			@tailrec def mutate(parent :MutableLongBranch, child :MutableLongBranch) :Boolean = {
				val path = child.center; val DiffBit = path & -path
				(key & -path | ~key & path) ^ (key & path) match {
					case 0L => child.right match {
						//this downcast, to be really type safe, should read :MutableLongKeyBranch[_<:This]
						//it is safe because branch must have been created by this instance's factory method.
						//immutable patches as well as binary operators always return tries with immutable roots
						case branch :MutableLongKeyBranch[S, M] with S =>
							mutate(child, branch) //continue the loop

						case leaf :LongKeyLeaf[S] =>
							if (leaf.key == key) {
								val old = leaf.asTrie
								replace(parent, child)(old, patch.whenKeyExists(key, old), child.left)
							} else {
								val replacement = patch.whenNoKey(key, leaf.asTrie)
								if (!(replacement eq leaf)) {
									child.setRight(replacement)
									true
								} else
									false
							}

						case immutable =>
							replace(parent, child)(immutable, patchRightChild(patch, key)(child), child.left)
					}

					case DiffBit => child.left match {
						case branch :MutableLongKeyBranch[S, M] with S =>
							mutate(child, branch) //continue the loop

						case leaf :LongKeyLeaf[S] =>
							if (leaf.key == key) {
								val old = leaf.asTrie
								replace(parent, child)(old, patch.whenKeyExists(key, old), child.right)
							} else {
								val replacement = patch.whenNoKey(key, leaf.asTrie)
								if (!(replacement eq leaf)) {
									child.setLeft(replacement)
									true
								} else
									false
							}

						case immutable =>
							replace(parent, child)(immutable, patchLeftChild(patch, key)(child), child.right)

					}

					case _ =>
						val replacement = patch.whenNoKey(key, child)
						if (!(replacement eq child)) {
							if (parent != null)
								parent.replace(child, replacement)
							else
								root.updateTrie(replacement)
							true
						} else
							false
				}

			}

			mutate(null.asInstanceOf[MutableLongBranch], this)
		}



	}







	/** Returns a `Long` which in the highest bits contains the longest common prefix
	  * of `path1` and `path2`, followed by a single set bit on the highest position
	  * where the values differ.
	  * If `path1==path2`, then their common value is returned.
	  * If their lowest bit is `1`, then this result is indistinguishable
	  * with the case where both values differ only on the lowest bit.
	  * Fot this reason the caller should ascertain that `path1!=path2` before
	  * calling this method.
	  */
	@inline final private[tries] def centerLabel(path1 :Long, path2 :Long) :Long = {
		var diff = path1 ^ path2 //what matters is that it has zeros on all common high bits
		diff |= (diff >>  1) //set to one all bits below those high zeros
		diff |= (diff >>  2)
		diff |= (diff >>  4)
		diff |= (diff >>  8)
		diff |= (diff >> 16)
		diff |= (diff >> 32)
		val prefixMask = ~diff //mask for the longest prefix where s1 equals s2 (from the highest bit)
		val firstDiffBit = diff - (diff >>> 1) //mask for the single first (highest) bit where s1 and s2 differ
		path1 & prefixMask | firstDiffBit
	}


	/** Flips the highest bit of the given value. Note that with two's complement encoding this '''does not''' produce `-key`. */
	@inline final private[tries] def flipSign(key :Long) :Long = key ^ 0x8000000000000000L


}
