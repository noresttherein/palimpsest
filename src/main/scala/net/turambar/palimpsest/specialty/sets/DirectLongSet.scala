package net.turambar.palimpsest.specialty.sets
/*
import net.turambar.palimpsest.specialty.FitIterable.IterableMapping
import net.turambar.palimpsest.specialty.FitIterator.{BaseIterator, MappedIterator}
import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.iterables.{EmptyIterable, IterableFoundation, SingletonFoundation, SingletonSpecialization}
import net.turambar.palimpsest.specialty.{FitBuilder, FitIterator, FitTraversableOnce, IterableSpecialization, Specialized}
import net.turambar.palimpsest.specialty.Specialized.{Fun1, Fun1Res, Fun1Vals, Fun2, Fun2Vals}
import net.turambar.palimpsest.specialty.sets.ValSet.ImmutableSetBuilder
import net.turambar.palimpsest.specialty.sets.DirectLongSet.{Empty, LongTrieIterator, Singleton, StableBranch}

import scala.annotation.tailrec
import scala.collection.{GenSet, GenTraversableOnce, SetLike, mutable}
import scala.collection.generic.CanBuildFrom




/** Specialized implementation for immutable sets of `Long` values. Implemented as a `Trie` where the path
  * to the nodes is the binary representation of elements in the subtree from most significant bit to the least significant bit.
  * @author Marcin Mościcki
  */
@deprecated("to be replaced with LongTrieSet")
trait DirectLongSet extends ValSet[Long] with SetSpecialization[Long, DirectLongSet] {
	override protected[this] def mySpecialization = Specialized.OfLong
	override def hasFastSize = true

	override def empty :DirectLongSet = DirectLongSet.Empty

//	override protected def filter(p :Long=>Boolean, where :Boolean) :DirectLongSet

	/** Masked bits common to all elements in this set. For all values in this set `e`, `(e & mask) == path`.
	  * It is also the lower bound of all values in this set.
	  * In default trie implementation values are stored in a Trie where paths followDown the binary representation
	  * of the values from the most significant bit, so `path` will consist of any number of fixed highest bits,
	  * followed by zeros on positions which vary between elements in this.set
	  */
	private[sets] def path :Long

	/** Mask for the binary portion of the values which is the same for all elements in this set.
	  * In default implementation `mask` consists of any number of set bits in the highest positions,
	  * followed by a sequence of zero bits.
	  */
	private[sets] def mask :Long

	private[sets] def retain(p :Long=>Boolean) :Unit = ()

	/** Can `elem`, given the invariants of this trie, be an element of this set?.
	  * For Empty and singleton sets it's equivalent to `contains`, for inner-nodes checks if
	  * the element's binary representation conforms to the shared the path to this node.
	  */
	private[sets] def belongs(elem :Long) :Boolean = contains(elem)

	override def toIterator :FitIterator[Long] = new LongTrieIterator(this)




	override def stringPrefix = "Set[Long]"
}






object DirectLongSet {
	import java.lang.Long.{MIN_VALUE, highestOneBit}

	@inline final def empty :StableSet[Long] = Empty

	def apply(values :Long*) :StableSet[Long] = (empty /: values)(_ + _)

	def newBuilder :FitBuilder[Long, StableSet[Long]] = new ImmutableSetBuilder[Long, StableLongTrie](Empty)

	def singleton(value :Long) :StableSet[Long] = new Singleton(value)

	def mutable :MutableSet[Long] = new MutableDirectLongSet()


	/** Returns a `Long` which in the highest bits contains the longest common prefix
	  * of `path1` and `path2`, followed by a single set bit on the highest position
	  * where the values differ.
	  * If `path1==path2`, then their common value is returned.
	  * If their lowest bit i `1`, then this result is indistinguishable
	  * with the case where both values differ only on the lowest bit.
	  * Fot this reason the caller should assertain that `path1!=path2` before
	  * calling this method.
	  */
	private[sets] def delimitedPrefix(path1 :Long, path2 :Long) :Long = {
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



	/** Immutable implementation of a `Set[Long]` as a Trie following the binary format of its values from the highest bit. */
	sealed trait StableLongTrie extends DirectLongSet with StableSet[Long] with SetSpecialization[Long, StableLongTrie] {
		override def empty :StableLongTrie = Empty
//		override def filter(p :Long=>Boolean, where :Boolean) :StableLongTrie
		/** Overriden due to linearization bug forwarding it to [[SpecializableSet]] instead of [[SetSpecialization]]. */
		override def newBuilder = new ImmutableSetBuilder[Long, StableLongTrie](Empty)

		override def mutable :MutableSet[Long] = new MutableDirectLongSet ++= this
		override def clone() = this

		def ++(other :StableLongTrie) :StableLongTrie
		def --(other :StableLongTrie) :StableLongTrie //= (this /: other)(_ - _) //todo

		def &(other :StableLongTrie) :StableLongTrie
		def subsetOf(other :StableLongTrie) :Boolean

		override def ++(elems: FitTraversableOnce[Long]) :StableLongTrie = elems match {
			case other :StableLongTrie => this ++ other
			case _ if elems.isEmpty => this
			case _ => (this /: elems)(_ + _)
		}
		override def --(elems: FitTraversableOnce[Long]) :StableLongTrie = elems match {
			case other :StableLongTrie => this -- other
			case _ if elems.isEmpty => this
			case _ => (this /: elems)(_ - _)
		}



		override def intersect(that: GenSet[Long]) = that match {
			case trie :StableLongTrie => this & trie
			case _ => filter(that)
		}



		override def subsetOf(that: GenSet[Long]) :Boolean = that match {
			case trie :StableLongTrie => this subsetOf trie
			case _ => forall(that)
		}
	}



	/** A set which doesn't contain any values, and in particular no `Long` values whatsoever. */
	case object Empty extends EmptyIterable[Long, StableLongTrie] with StableLongTrie with EmptySetSpecialization[Long, StableLongTrie]
	{
		override def filter(p: (Long) => Boolean, ourTruth: Boolean) :this.type = this

		override def contains(elem: Long): Boolean = false
		override def +(elem: Long): Singleton = new Singleton(elem)
		override def -(elem: Long): this.type = this



		override def ++(elems: FitTraversableOnce[Long]) :StableLongTrie = elems match {
			case other :StableLongTrie => other
			case _ if elems.isEmpty => this
			case _ => (newBuilder ++= elems).result()
		}
		override def ++(other: StableLongTrie): StableLongTrie = other

		override def --(elems: FitTraversableOnce[Long]) :StableLongTrie = this
		override def --(other: StableLongTrie): StableLongTrie = this

		override def &(other: StableLongTrie): StableLongTrie = this
		override def subsetOf(other: StableLongTrie): Boolean = true

		override def path = 0L
		override def mask = 0L

		override def mutable :MutableSet[Long] = new MutableDirectLongSet

		override protected def uncheckedCopyTo(xs: Array[Long], start: Int, total: Int): Int = 0

		override def toString = "Set[Long]()"

		override def clone() = this
	}



	/** A simple set containing a single `Long` value, serving at the same time as the leaf in the Trie. */
	final case class Singleton(override val path :Long)
		extends SingletonFoundation[Long, StableLongTrie] with SingletonSpecialization[Long, StableLongTrie]
				with StableLongTrie
	{
		override def head = path

		override def mask = 0xffffffffffffffffL

		override def contains(elem: Long): Boolean = elem==head

		override def +(elem: Long): StableLongTrie =
			if (elem==head) this
			else StableBranch(this, new Singleton(elem))

		override def -(elem: Long): StableLongTrie =
			if (elem==head) Empty else this


		override def ++(other: StableLongTrie): StableLongTrie = other + path

		override def --(other: StableLongTrie): StableLongTrie =
			if (other(path)) Empty else this

		override def &(other: StableLongTrie): StableLongTrie =
			if (other(head)) this else Empty

		override def subsetOf(other: StableLongTrie): Boolean =
			other(head)

		override def mutable :MutableSet[Long] = new MutableDirectLongSet(head)

		override def toString = s"Set[Long]($head)"
	}

	object Singleton {
		private[DirectLongSet] final val mask = 0xffffffffffffffffL
	}




	/** An inner node in the `DirectLongSet` `Trie`.
	  */
	private[sets] abstract class BranchLike[+This<:DirectLongSet with SetSpecialization[Long, This]]
		extends IterableFoundation[Long, This] //with SetSpecialization[Long, This]
	{ this :DirectLongSet =>
		private[sets] def left :DirectLongSet
		private[sets] def right :DirectLongSet


		override def size = left.size + right.size
		override def isEmpty = false
		override def nonEmpty = true

		/** Mask for the single bit separating the left and right subtrees. `left` is assumed to contain
		  * all values for which this bit is clear, while `right` to contain all values with this bit set.
		  */
		private[sets] def diffBit :Long
		private[sets] def delimitedPath = path | diffBit

		override def head = left.head
		override def headOption :Option[Long] = Some(left.head)
		override def last = right.last
		override def lastOption :Option[Long] = Some(right.last)

		override private[sets] def belongs(elem :Long) = (elem & mask) == path

		/** Value used to determine in which subtree of this set the given element belongs.
		  *
		  * @param elem any value to potentially add or remove from this set.
		  * @return `0` if the element belongs to the `right` tree,
		  *        [[diffBit]] if the element belongs to the `left` tree,
		  *        and any other value if the element doesn't share the common path of this set.
		  */
		private[sets] def assign(elem :Long) =
			//left-hand xor operand is equivalent to a xor of elem and our path, clearing any bits lower than diffBit (lowest set bit in common),
			//but has the diffBit always set, as elem | -elem is always 1, and both -common and common have diffBit set.
			//right-hand xor operand cleares the diffBit if it is set in elem.
			(elem & -delimitedPath | -elem & delimitedPath) ^ (elem & diffBit)

		//override def belongs(elem :Long) = (elem & -common | ~elem & common) == (common & -common)


		override def contains(elem: Long): Boolean =
			belongs(elem) && (
				if ((elem & diffBit) == 0) left.contains(elem)
				else right.contains(elem)
			)

		override def foreach[@specialized(Unit) U](f: (Long) => U) = {
			left foreach f
			right foreach f
		}

		override def reverseForeach(f: (Long) => Unit): Unit = {
			right reverseTraverse f
			left reverseTraverse f
		}


		override def forall(p: (Long) => Boolean) = left.forall(p) && right.forall(p)

		override def exists(p: (Long) => Boolean) = left.exists(p) || right.exists(p)

		override def count(p: (Long) => Boolean) = left.count(p) + right.count(p)

		override def find(p: (Long) => Boolean) = left.find(p) orElse right.find(p)

		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, Long) => O) =
			right.foldLeft(left.foldLeft(z)(op))(op)

		override def foldRight[@specialized(Fun2) O](z: O)(op: (Long, O) => O) =
			left.foldRight(right.foldRight(z)(op))(op)


		protected[this] override def uncheckedCopyTo(xs: Array[Long], start: Int, total: Int) = {
			val lsize = left.size
			if (total < lsize)
				ValSet.friendCopy(left, xs, start, total)
			else {
				ValSet.friendCopy(left, xs, start, lsize)
				lsize + ValSet.friendCopy(right, xs, start+lsize, total-lsize)
			}
		}

		override def copyToBuffer[B >: Long](dest: collection.mutable.Buffer[B]) = {
			left.copyToBuffer(dest)
			right.copyToBuffer(dest)
		}



		override def toString = {
			val s = new StringBuilder(stringPrefix) append '('
			left.addString(s, ", ")
			s append ", "
			right.addString(s, ", ")
			s append ')'
			s.result()
		}

	}

	private type Branch = BranchLike[DirectLongSet] //with DirectLongSet





	/** Inner node in stable `Long` sets. Represents a non-empty set, split in half based on the value of a single
	  * discriminator bit in the binary representation of stored values.
	  * Left subtree contains all elements for which the value of the said bit is zero, and the right subtree
	  * contains all elements for which the discriminator bit is set. Both left and right subtrees must be non-empty
	  * (as otherwise this instance could be substituted by the non-empty child or [[DirectLongSet.Empty]]).
	  * This instance assumes the path to the leaves storing actual values follows their binary representation starting from
	  * the least significant bit, so `common` (and `path` and `mask` computed from it) must contain the common bits
	  * in its lowest positions
	  *
	  * @param common encoded path to this node. In highest positions, it contains the longest prefix common in the binary
	  *              representation of all elements in this set, which is then followed by a single set bit denoting
	  *              the position on which elements in the left subtree differ from the elements in the right subtree.
	  * @param left all elements in this set with the discriminator bit clear, i.e. `(e & diffBit) == 0`.
	  * @param right all elements in this set with the discriminator bit set, i.e. `(e & diffBit) == diffBit`.
	  */
	private[sets] final class StableBranch(common :Long, private[sets] val left :StableLongTrie, private[sets] val right :StableLongTrie)
		extends BranchLike[StableLongTrie] with StableLongTrie with OfKnownSize
	{
		def this(sharedPrefix :Long, diffBit :Long, left :StableLongTrie, right :StableLongTrie) =
			this(sharedPrefix | diffBit, left, right)

		override val size = left.size + right.size

		@inline override def delimitedPath :Long = common
		@inline def path = common & (common-1) //clear lowest set bit
		@inline def diffBit = common & -common //mask for the lowest set bit
		@inline def mask = { val bit = common & -common; -bit ^ bit } //mask for the shared prefix in the binary representation of all elements in this set

		override def belongs(elem :Long) = (elem & -common | ~elem & common) == (common & -common)

		override def tail = filtered(left.tail, right)
		override def init = filtered(left, right.init)



		override def +(elem: Long): StableLongTrie = {
			val DiffBit = common & -common
			(elem & -common | ~elem & common) ^ (elem & DiffBit) match {
				case 0 => //(elem & mask) == path && (elem & DiffBit)==1
					new StableBranch(common, left, right + elem)
				case DiffBit => //(elem & mask) == path && (elem & DiffBit)==0
					new StableBranch(common, left + elem, right)
				case _ => //(elem & mask) != path
					StableBranch(this, new Singleton(elem))
			}
		}

		override def -(elem: Long): StableLongTrie = {
			val DiffBit = common & - common
			(elem & -common | ~elem & common) ^ (elem & DiffBit) match {
				case 0 =>
					filtered(left, right - elem)
				case DiffBit =>
					filtered(left - elem, right)
				case _ => this
			}
		}


		override def ++(other :StableLongTrie) :StableLongTrie = other match {
			case Empty => this
			case s :Singleton => this + s.path
			case b :StableBranch =>
				val path1 = common; val path2 = b.delimitedPath
				val diff = path1 ^ path2
				if (diff==0L)
					new StableBranch(common, left ++ b.left, right ++ b.right)
				else {
					val point1 = path1 & -path1; val mask1 = -point1 ^ point1
					if ((diff & mask1) == 0L) //path1 is prefix of path2
						if ((path2 & point1) == 0L)
							new StableBranch(path1, left ++ b, right) //todo compare left++b with left for eq
						else
							new StableBranch(path1, left, right ++ b)
					else {
						val point2 = path2 & -path2; val mask2 = -point2 ^ point2
						if ((diff & mask2)==0L) //path2 is prefix of path1
							if ((path1 & point2) == 0L)
								new StableBranch(path2, this ++ b.left, b.right)
							else
								new StableBranch(path2, b.left, this ++ b.right)
						else //sets are disjoint, as they differ in the common span of path1 and path2
							StableBranch(this, b)
					}
				}
		}

		override def --(other :StableLongTrie) :StableLongTrie = other match {
			case Empty => this
			case s :Singleton => this - s.path
			case b :StableBranch =>
				val path1 = common; val path2 = b.delimitedPath
				val diff = path1 ^ path2
				if (diff==0L)
					new StableBranch(common, left -- b.left, right -- b.right)
				else {
					val point1 = path1 & -path1; val point2 = path2 & -path2
					val mask1 = -point1 ^ point1; val mask2 = -point2 ^ point2
					if ((diff & mask1) == 0L) //path1 is prefix of path2
						if ((path2 & point1) == 0L) left -- b match {
							case same if same eq left => this
							case empty if empty.isEmpty => right
							case l => new StableBranch(path1, l, right)
						} else right -- b match {
							case same if same eq right => this
							case empty if empty.isEmpty => left
							case r => new StableBranch(path1, left, r)
						}
					else {
						val point2 = path2 & -path2; val mask2 = -point2 ^ point2
						if ((diff & mask2) == 0L) //path2 is prefix of path1
							if ((path1 & point2) == 0L)
								this -- b.left
							else
								this -- b.right
						else //sets are disjoint, as they differ in the common span of path1 and path2
							this
					}
				}
		}

		override def &(other :StableLongTrie) :StableLongTrie = other match {
			case Empty => this
			case s :Singleton => if (contains(s.path)) s else empty
			case b :StableBranch =>
				val path1 = common; val path2 = b.delimitedPath
				val diff = path1 ^ path2
				if (diff==0L)
					new StableBranch(common, left & b.left, right & b.right)
				else {
					val point1 = path1 & -path1; val mask1 = -point1 ^ point1
					if ((diff & mask1) == 0L) //path1 is prefix of path2
						if ((path2 & point1) == 0L)
							left & b
						else
							right & b
					else {
						val point2 = path2 & -path2; val mask2 = -point2 ^ point2
						if ((diff & mask2)==0L) //path2 is prefix of path1
							if ((path1 & point2) == 0L)
								this & b.left
							else
								this & b.right
						else //sets are disjoint, as they differ in the common span of path1 and path2
							empty
					}
				}
		}

		override def subsetOf(other :StableLongTrie) :Boolean = other match {
			case Empty => false
			case s: Singleton => false
			case b: StableBranch =>
				val path1 = common; val path2 = b.delimitedPath
				val diff = path1 ^ path2
				if (diff == 0L)
					(left subsetOf b.left) && (right subsetOf b.right)
				else {
					val point1 = path1 & -path1
					val point2 = path2 & -path2; val mask2 = -point2 ^ point2
					((diff & mask2) == 0L) && subsetOf(if ((path1 & point2) == 0L) b.left else b.right)
				}
		}

/*
		override def ++(other: StableLongTrie): StableLongTrie = other match {
			case Empty => this
			case s :Singleton => this + s.path
			case b :StableBranch if common == b.delimitedPath =>
				new StableBranch(common, left ++ b.left, right ++ b.right)

			case b :StableBranch =>
				val bit1 = diffBit; val bit2 = b.diffBit
				if (bit1==bit2) //s1 and s2 have fixed prefixes of the same length, but unequal, ergo the sets are disjoint
					StableBranch(this, b)
				else {
					val commonOther = b.delimitedPath
					val joint = delimitedPrefix(common, commonOther)
					val divergencePoint = joint & -joint //the highest bit where (delimited!) paths of s1 and s2 differ
					//ordering of the below values is equivalent to ordering on the position of the set bit
					val s1Rank = bit1 + MIN_VALUE
					val s2Rank = bit2 + MIN_VALUE
					val diffRank = divergencePoint + MIN_VALUE
					if (diffRank > s1Rank && diffRank > s2Rank)
						//divergence point bit value is constant within s1 and s2, but different between them
						if ((common & divergencePoint) == 0L)
							new StableBranch(joint, this, b)
						else
							new StableBranch(joint, b, this)
					else if (s1Rank > s2Rank) //bit1 is higher
						if ((commonOther & bit1) == 0L) //s1.path is a strict prefix of s2.path
							new StableBranch(common, left ++ b, right)
						else
							new StableBranch(common, left, right ++ b)
					else //bit2 is higher, symmetrical case if (diffRank > s2Rank)
						if ((common & bit2) == 0L)
							new StableBranch(commonOther, this ++ b.left, b.right)
						else
							new StableBranch(commonOther, b.left, this ++ b.right)
				}

		}



		override def --(other: StableLongTrie) :StableLongTrie = other match {
			case Empty => this
			case s :Singleton => this - s.head
			case b :StableBranch if common == b.delimitedPath =>
				filtered(left -- b.left, right -- b.right)
			case b :StableBranch =>
				val bit1 = diffBit; val bit2 = b.diffBit
				if (bit1==bit2) //s1 and s2 have fixed prefixes of the same length, but unequal, ergo the sets are disjoint
					this
				else {
					val commonOther = b.delimitedPath
					val joint = delimitedPrefix(common, commonOther)
					val divergencePoint = joint & -joint //the highest bit where (delimited!) paths of s1 and s2 differ
					//ordering of the below values is equivalent to ordering on the position of the set bit
					val s1Rank = bit1 + MIN_VALUE
					val s2Rank = bit2 + MIN_VALUE
					val diffRank = divergencePoint + MIN_VALUE
					if (diffRank > s1Rank && diffRank > s2Rank)
						this
					else if (s1Rank > s2Rank) //bit1 is higher
						if ((commonOther & bit1) == 0L) //s1.path is a strict prefix of s2.path
							filtered(left -- b, right)
						else
							filtered(left, right -- b)
					else
						if ((common & bit2) == 0L)
							this -- b.left
						else
							this -- b.right
				}

		}


		override def &(other: StableLongTrie): StableLongTrie = other match {
			case Empty => Empty

			case s :Singleton => if (contains(s.head)) s else Empty

			case b :StableBranch if common == b.delimitedPath =>
				filtered(left & b.left, right & b.right)

			case b :StableBranch =>
				val bit1 = diffBit; val bit2 = b.diffBit
				if (bit1==bit2) //s1 and s2 have fixed prefixes of the same length, but unequal, ergo the sets are disjoint
					Empty
				else {
					val commonOther = b.delimitedPath
					val joint = delimitedPrefix(common, commonOther)
					val divergencePoint = joint & -joint //the highest bit where (delimited!) paths of s1 and s2 differ
					//ordering of the below values is equivalent to ordering on the position of the set bit
					val s1Rank = bit1 + MIN_VALUE
					val s2Rank = bit2 + MIN_VALUE
					val diffRank = divergencePoint + MIN_VALUE
					if (diffRank > s1Rank && diffRank > s2Rank)
						Empty
					else if (s1Rank > s2Rank) //bit1 is higher
						if ((commonOther & bit1) == 0L) //s1.path is a strict prefix of s2.path
							left & b
						else
							right & b
					else
						if ((common & bit2) == 0L)
							this & b.left
						else
							this & b.right
				}

		}


		override def subsetOf(other: StableLongTrie): Boolean = other match {
			case _ if other.size < size => false
			case b :StableBranch if common==b.delimitedPath =>
				(left subsetOf b.left) && (right subsetOf b.right)

			case b :StableBranch =>
				val bit1 = diffBit; val bit2 = b.diffBit

				(bit1 + MIN_VALUE < bit2 + MIN_VALUE) &&
					(highestOneBit(common ^ b.delimitedPath) + MIN_VALUE <= bit2 + MIN_VALUE) &&
					( if ((common & bit2) == 0L) subsetOf(b.left) else subsetOf(b.right) )

		}
*/


		override protected[this] def dropTake(from: Int, until: Int) :StableLongTrie =
			if (until<=from || from >= size)
				Empty
			else {
				val lsize = left.size
				if (from >= lsize)
					right.slice(from-lsize, until-lsize)
				else if (until <= lsize)
					left.slice(from, until)
				else new StableBranch(common, left.drop(from), right.take(until-lsize))
			}

		override def filter(p: (Long) => Boolean, ourTruth: Boolean): StableLongTrie =
			if (ourTruth) filter(p) else filterNot(p)
//			filtered(left.filter(p, where), right.filter(p, where))
		override def filter(p: (Long) => Boolean) :StableLongTrie = filtered(left.filter(p), right.filter(p))
		override def filterNot(p: (Long) => Boolean) = filtered(left.filterNot(p), right.filterNot(p))

		def filtered(l :StableLongTrie, r :StableLongTrie): StableLongTrie =
			if (l.isEmpty) r
			else if (r.isEmpty) l
			else new StableBranch(common, l, r)

		@inline private final def joinWith(prefix :Long, other :StableLongTrie, diffBit :Long) :StableLongTrie =
			if ((common & diffBit)==0L)
				new StableBranch(prefix, this, other)
			else
				new StableBranch(prefix, other,  this)

	}

	private object StableBranch {
//		private[DirectLongSet] def suffixJoin(s1 :StableLongSet, s2 :StableLongSet) :StableBranch = {
//			val suffix1 = s1.path
//			val diff = suffix1 ^ s2.path
//			val diffBit = diff & -diff
//			val sharedSuffix = (diffBit-1) & suffix1
//			if ((suffix1 & diffBit) == 0L)
//				new StableBranch(sharedSuffix, diffBit, s1, s2)
//			else
//				new StableBranch(sharedSuffix, diffBit, s2, s1)
//		}


		private[DirectLongSet] def apply(s1 :StableLongTrie, s2 :StableLongTrie) :StableBranch = {
			val path1 = s1.path
			val common = delimitedPrefix(path1, s2.path)
			if ((path1 & common & -common)==0L)
				new StableBranch(common, s1, s2)
			else
				new StableBranch(common, s2, s1)
		}



	}




	/** A mutable variant of a Trie holding `Long` values with their binary format as a path.
	  * Unlike [[StableLongTrie]], there is only one implementation and the set consists only
	  * of instances of this class and [[Empty]] as leaves.
	  * If this is an empty set, then `common` must equal zero, and both `zeros` and `ones` must be [[Empty]].
	  * If this is a singleton set, then `common` must equal the one element and both `zeros` and `ones` must be `Empty`
	  * If this set contains more then one element, then `common` represents the path to this node.
	  * and '''both''' `left` and `right` must be non-empty sets (as this is the divergence point between the two).
 	  *
	  * @param _size number of elements in this set
	  * @param common path to this node. If this is empty, must be zero; if this is singleton, must equal stored element.
	  *               In other cases, the lowest set bit denotes the position on which both children differ.
	  *               All bits above that are common to all elements in this set. In other words, common is `path | diffBit`.
	  * @param left non-empty subset containing all elements which have a clear bit on the position of `diffBit` (as obtained from `common`). Empty if `size`<=1.
	  * @param right non-empty subset containing all elements which have a set bit on the position of `diffBit` (as obtained from `common`). Empty if `size`<=1.
	  */
	private final class MutableDirectLongSet private[DirectLongSet](private[this] var _size :Int, private[this] var common :Long, private[sets] var left: DirectLongSet, private[sets] var right: DirectLongSet)
		extends BranchLike[MutableDirectLongSet] with OfKnownSize
				with MutableSet[Long] with SetSpecialization[Long, MutableDirectLongSet] with MutableSetSpecialization[Long, MutableDirectLongSet] with DirectLongSet
	{

		/** A singleton mutable set. */
		def this(singleton :Long) = this(1, singleton, Empty, Empty)

		/** An empty mutable set. */
		def this() = this(0, 0L, Empty, Empty)

		def this(common :Long, left: DirectLongSet, right: DirectLongSet) = this(left.size + right.size, common, left, right)

		@inline def path = if (_size==1) common else common & (common-1) //clear lowest set bit
		@inline def diffBit = if (_size==1) 0L else common & -common //mask for the lowest set bit
		@inline def mask = //mask for the shared prefix in the binary representation of all elements in this set
			if (_size==1) Singleton.mask
			else {
				val bit = common & -common; -bit ^ bit
			}

		@inline override private[sets] def delimitedPath = common
		override def size = _size

		override def belongs(elem :Long) = (elem & -common | ~elem & common) == (common & -common)

		override def isEmpty = size==0
		override def nonEmpty = size>0

		override def head = if (_size==1) common else left.head
		override def headOption = if (_size==1) Some(common) else left.headOption
		override def last = if (_size==1) common else right.last
		override def lastOption = if (_size==1) Some(common) else right.lastOption

		override def tail: MutableDirectLongSet =
			if (left.nonEmpty) filtered(left.tail, right)
			else if (size==1) empty
			else filtered(Empty, right.tail) //right is Empty, so will throw UnsupportedOperatoinException

		override def init: MutableDirectLongSet  =
			if (right.nonEmpty) filtered(left, right.init)
			else if (size==1) empty
			else filtered(left.init, Empty) //left is Empty, so will throw UnsupportedOperationException

		override def empty: MutableDirectLongSet = new MutableDirectLongSet(0, 0L, Empty, Empty)


		override def contains(elem: Long) = _size match {
			case n if n>1 =>
				val DiffBit = common & -common
				(elem & -common | ~elem & common) ^ (elem & DiffBit) match {
					case 0 => right.contains(elem)
					case DiffBit => left.contains(elem)
					case _ => false
				}
			case 1 => common == elem
			case _ => false
		}



		override def +(elem: Long) = _size match {
			case 0 => new MutableDirectLongSet(elem)
			case 1 =>
				if (common==elem) this
				else Mutable.join(this, new MutableDirectLongSet(elem))
			case _ =>
				val DiffBit = common & -common
				(elem & -common | ~elem & common) ^ (elem & DiffBit) match {
					case 0 => new MutableDirectLongSet(common, left, right + elem)
					case DiffBit => new MutableDirectLongSet(common, left+elem, right)
					case _ => Mutable.join(this, new MutableDirectLongSet(elem))
				}
		}

		override def -(elem: Long) = _size match {
			case 0 => this
			case 1 =>
				if (common==elem) new MutableDirectLongSet()
				else this
			case _ =>
				val DiffBit = common & -common
				(elem & -common | ~elem & common) ^ (elem & DiffBit) match {
					case 0 => filtered(left, right-elem)
					case DiffBit => filtered(left-elem, right)
					case _ => this
				}
		}

		override def +=(elem :Long) :this.type = { add(elem); this }

		override def add(elem: Long): Boolean = _size match {
			case 0 => become(elem); true
			case 1 => common!=elem && {
				become(new MutableDirectLongSet(common), new MutableDirectLongSet(elem))
				true
			}
			case total =>
				val DiffBit = common & -common
				(elem & -common | ~elem & common) ^ (elem & DiffBit) match {
					case 0 => (right.asInstanceOf[MutableDirectLongSet] add elem) && {
						_size = left.size + right.size; true
					}
					case DiffBit => (left.asInstanceOf[MutableDirectLongSet] add elem) && {
						_size = left.size + right.size; true
					}
					case _ =>
						become(new MutableDirectLongSet(total, common, left, right), new MutableDirectLongSet(elem))
						true
				}
		}


		override def -=(elem :Long) :this.type = { remove(elem); this }

		override def remove(elem: Long): Boolean = _size match {
			case 0 => false
			case 1 => common == elem && { clear(); true }
			case total =>
				val DiffBit = common & -common
				(elem & -common | ~elem & common) ^ (elem & DiffBit) match {
					case 0 => (right.asInstanceOf[MutableDirectLongSet] remove elem) && {
						update(); true
					}
					case DiffBit => (left.asInstanceOf[MutableDirectLongSet] remove elem) && {
						update(); true
					}
					case _ => false
				}
		}


		override def dropTake(from: Int, until: Int): MutableDirectLongSet =
			if (until<=from || from >= size)
				new MutableDirectLongSet()
			else if (size==1) new MutableDirectLongSet(common)
			else {
				val lsize = left.size
				if (until <= lsize)
					left.asInstanceOf[MutableDirectLongSet].dropTake(from, until)
				else if (from >= lsize)
					right.asInstanceOf[MutableDirectLongSet].dropTake(from-lsize, until-lsize)
				else new MutableDirectLongSet(
					common,
					left.asInstanceOf[MutableDirectLongSet].dropTake(from, lsize),
					right.asInstanceOf[MutableDirectLongSet].dropTake(0, until-lsize)
				)
			}

		override def filter(p: (Long) => Boolean, ourTruth: Boolean): MutableDirectLongSet =
			if (ourTruth) filter(p) else filterNot(p)
//			filtered(left.filter(p, where), right.filter(p, where))

		override def filter(p: (Long) => Boolean): MutableDirectLongSet = filtered(left.filter(p), right.filter(p))
		override def filterNot(p: (Long) => Boolean): MutableDirectLongSet = filtered(left.filterNot(p), right.filterNot(p))

		override def retain(p: (Long) => Boolean) :Unit = _size match {
			case 0 =>
			case 1 => if (!p(path)) clear()
			case _ =>
				left.retain(p); right.retain(p)
				update()
		}


		override def foreach[@specialized(Unit) U](f: (Long) => U) =
			if (_size>1) {
				left.foreach(f); right.foreach(f)
			} else if (size==1)
				f(common)

		override def reverseForeach(f: (Long) => Unit) =
			if (_size>1) {
				right.reverseTraverse(f); left.reverseTraverse(f)
			} else if (_size==1)
				f(common)


		override def forall(p: (Long) => Boolean) = _size match {
			case 0 => true
			case 1 => p(common)
			case _ => left.forall(p) && right.forall(p)
		}

		override def exists(p: (Long) => Boolean) = _size match {
			case 0 => false
			case 1 => p(common)
			case _ => left.exists(p) || right.exists(p)
		}

		override def count(p: (Long) => Boolean) = _size match {
			case 0 => 0
			case 1 => if (p(common)) 1 else 0
			case _ => left.count(p) + right.count(p)
		}

		override def find(p: (Long) => Boolean) = _size match {
			case 0 => None
			case 1 => if (p(common)) Some(common) else None
			case _ => left.find(p) orElse right.find(p)
		}

		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, Long) => O) = _size match {
			case 0 => z
			case 1 => op(z, common)
			case _ => right.foldLeft(left.foldLeft(z)(op))(op)
		}

		override def foldRight[@specialized(Fun2) O](z: O)(op: (Long, O) => O) = _size match {
			case 0 => z
			case 1 => op(common, z)
			case _ => left.foldRight(right.foldRight(z)(op))(op)
		}

		@inline override final def clear() :Unit = {
			_size = 0; common = 0L
			left = Empty; right = Empty
		}

		/** Callback for fixing up invariants after filtering subtrees.
		  * Must update size and, if any of subtrees became empty, reduce itself to the other tree.
		  */
		private def update() :Unit =
			if (left.isEmpty)
				if (right.isEmpty) clear()
				else become(right.asInstanceOf[MutableDirectLongSet])
			else if (right.isEmpty)
				become(left.asInstanceOf[MutableDirectLongSet])
			else
				_size = left.size + right.size


		@inline final private[this] def become(singleton :Long) :Unit = {
			common = singleton; _size = 1
		}

		@inline final private[this] def become(other: MutableDirectLongSet) :Unit = {
			common = other.delimitedPath; _size = other.size
			left = other.left; right = other.right
		}


		protected def become(x: MutableDirectLongSet, y: MutableDirectLongSet) :Unit = {
			val xPath = x.path
			common = delimitedPrefix(xPath, y.path)
			if ((xPath & common & -common) == 0L) {
				left = x; right = y
			} else {
				left = y; right = x
			}
			_size = x.size + y.size
		}


		def filtered(l: DirectLongSet, r: DirectLongSet): MutableDirectLongSet =
			if (l.isEmpty) r match {
				case _ if r.isEmpty => empty
				case m: MutableDirectLongSet => m
				case _ => throw new MatchError(s"Illegal child of a MutableDirectLongSet - neither a MutableDirectLongSet nor Empty: $l")
			} else if (r.isEmpty)
				l.asInstanceOf[MutableDirectLongSet]
			else
				new MutableDirectLongSet(common, l, r)



		override def clone(): MutableDirectLongSet = new MutableDirectLongSet(_size, common, left.clone(), right.clone())

		override def stringPrefix = "MutableSet[Long]"

		override def count: Int = size
	}






	object Mutable {
		def empty :MutableSet[Long] = new MutableDirectLongSet()
		def newBuilder :FitBuilder[Long, MutableSet[Long]] = new MutableDirectLongSet()
		def singleton(value :Long) :MutableSet[Long] = new MutableDirectLongSet() += value



		private[DirectLongSet] def join(x: MutableDirectLongSet, y: MutableDirectLongSet): MutableDirectLongSet = {
			val xPath = x.path
			val common = delimitedPrefix(xPath, y.path)
			if ((xPath & common & - common) == 0L)
				new MutableDirectLongSet(common, x, y)
			else
				new MutableDirectLongSet(common, y, x)
		}

	}




	/** A sorted, immutable set of `Long` values. This implementation takes advantage of the fact that a Trie
	  * ordered lexicographically by the binary format of two-complement encoded numbers is already ordered
	  * if it doesn't contain elements of opposite signa. For this reason, it is sufficient to provide an
	  * artificial, 'sticky' branch point as the root of the trie diverging on the sign bit (i.e. which path is length of zero)
	  * and having it's subtrees swapped. Left subtree of this set is a `StableLongSet` containing all negative values
	  * in this set, while the right subtree of this set contains all non-negative members.
	  *
	  * @param left subset of `this` containing all negative elements
	  * @param right subset of `this` containing all non-negative elements
	  */
	private class SortedTrie private[DirectLongSet](private[sets] val left: StableLongTrie, private[sets] val right: StableLongTrie)
		extends BranchLike[SortedTrie] with DirectLongSet with StableSet[Long] with SetSpecialization[Long, SortedTrie] with StableOrderedSet[Long] with OfKnownSize
	{

		override def newBuilder = new ImmutableSetBuilder(Sorted.Empty)

		override implicit def ordering = Ordering.Long
		override def stable = this

		override private[sets] def diffBit = 0x8000000000000000L
		override private[sets] def path = 0L
		override private[sets] def mask = 0L
		override private[sets] def belongs(elem :Long) = true

		override def hasFastSize = true
		override def size = left.size + right.size
		override def isEmpty = left.isEmpty && right.isEmpty
		override def nonEmpty = left.nonEmpty || right.nonEmpty

		override def head = if (left.nonEmpty) left.head else right.head
		override def headOption = left.headOption orElse right.headOption
		override def last = if (right.nonEmpty) right.last else left.last
		override def lastOption = right.lastOption orElse left.lastOption

		override def tail =
			if (left.nonEmpty) new SortedTrie(left.tail, right)
			else new SortedTrie(Empty, right.tail)

		override def init =
			if (right.nonEmpty) new SortedTrie(left, right.init)
			else new SortedTrie(left.init, Empty)

		override def empty = Sorted.Empty


		override def contains(elem: Long) =
			if (elem<0) left.contains(elem)
			else right.contains(elem)

		override def +(elem: Long): SortedTrie =
			if (elem<0) new SortedTrie(left+elem, right)
			else new SortedTrie(left, right+elem)

		override def -(elem: Long): SortedTrie =
			if (elem<0) new SortedTrie(left-elem, right)
			else new SortedTrie(left, right-elem)


		override protected[this] def dropTake(from: Int, until: Int) =
			if (until<=from || from >=size) Sorted.Empty
			else if (from >= left.size) new SortedTrie(Empty, right.slice(from-left.size, until-left.size))
			else if (until <= left.size) new SortedTrie(left.slice(from, until), Empty)
			else new SortedTrie(left.drop(from), right.take(until-left.size))

		override def filter(p: (Long) => Boolean, ourTruth: Boolean) =
			if (ourTruth)
				new SortedTrie(left.filter(p), right.filter(p))
			else
				new SortedTrie(left.filterNot(p), right.filterNot(p))


		override def keysIteratorFrom(start: Long): FitIterator[Long] =
			if (start==Long.MinValue || isEmpty) FitIterator.empty[Long]
			else {
				//a stack describing the path to the current element and all elements in this set following it.
				//If there are no more elements to iterate, the stack is empty. Otherwise on its top resides a singleton leaf
				//with the first/next element of the created iterator. Below it are all Branch nodes on the path leading to it,
				//from which we descended into the left tree, and thus their whole right subtree remains to be iterated.
				//advancing the iterator retreats on the stack to take most-recent possible right turn
				//(removing the node to which we retreated from the stack, and advancing again with a preference for the left child.
				var path = new Array[DirectLongSet](64)
				path(0) = this
				@tailrec def first(stack :Array[DirectLongSet] = path, top :Int=0) :Int =
					if (top < 0) top
					else stack(top) match {
						case b :Branch =>
							if (start < b.right.path) { //start belongs to the left subtree
								stack(top + 1) = b.left
								first(stack, top + 1)
							} else { //all elements in the left subtree are smaller than start
								stack(top) = b.right
								first(stack, top)
							}

						case s :Singleton =>
							if (start <= s.head) top
							else if (top==0) {
								stack(top) = null; -1
							} else {
								stack(top) = null
								stack(top-1) = stack(top).asInstanceOf[Branch].right
								first(stack, top-1)
							}
						//this shouldn't happen, but let's keep it for possible reuse in other tries
						case Empty =>
							if (top==0) -1
							else {
								stack(top) = null
								stack(top-1) = stack(top).asInstanceOf[Branch].right
								first(stack, top-1)
							}

					}
				new LongTrieIterator(path, first())
			}

		override def rangeImpl(from: ?[Long], until: ?[Long]): StableOrderedSet[Long] =
			if (until.isDefined) range(from getOrElse Long.MinValue, until.get)
			else if (from.isDefined) this.from(from.get)
			else this

		override def until(until: Long) = range(Long.MinValue, until)

		override def from(from: Long) =
			if (from==Long.MinValue) this
			else {
				def drop(trie: StableLongTrie): StableLongTrie = trie match {
					case Empty => Empty
					case s :Singleton =>
						if (s.head < from) Empty else s
					case b :StableBranch if !b.belongs(from) =>
						if (from < b.path) b else Empty
					case b :StableBranch =>
						if ((b.diffBit & from) == 0L)
							b.filtered(drop(b.left), b.right)
						else drop(b.right)
				}
				if (from<0) new SortedTrie(drop(left), right)
				else new SortedTrie(Empty, drop(right))
			}


		override def range(from: Long, until: Long) :SortedTrie =
			if (until<=from) Sorted.Empty
			else {
				def select(trie: StableLongTrie): StableLongTrie = trie match {
					case Empty => Empty
					case s :Singleton => if (from<=s.head && s.head < until) s else Empty
					case b :StableBranch =>
						val rightPath = b.right.path //lowest possible element in the right subtree
						if (until <= b.path) Empty
						else if (until <= rightPath) select(b.left)
						else if (from >= rightPath) select(b.right)
						else b.filtered(select(b.left), select(b.right))
				}
				if (from >= 0) new SortedTrie(Empty, select(right))
				else if (until < 0) new SortedTrie(select(left), Empty)
				else new SortedTrie(select(left), select(right))
			}

		override def stringPrefix = "SortedSet[Long]"
	}



	object Sorted {
		private[DirectLongSet] final val Empty = new SortedTrie(DirectLongSet.Empty, DirectLongSet.Empty)
		final val empty :StableOrderedSet[Long] = Empty

		def newBuilder :FitBuilder[Long, StableOrderedSet[Long]] = new ImmutableSetBuilder(empty)

	}






	/** An iterator traversing a trie using a manual stack implemented using an array.
	  * @param top index of the top element on the stack. Negative index means an empty iterator.
	  * @param stack path to the current node splitting the trie in two, with the 'right-hand' side containing remaining elements.
	  *              The top of the stack - `stack(top)`, providing `this` is not empty - is a singleton leaf node in the trie, being
	  *              the `head` of this iterator. Below it, are all inner nodes of type `Branch` from which we descended to the left child.
	  *              Thus, all right children of all inner nodes on the path remain to be iterated.
	  *              Whenever we descend right from an inner node, we remove it from the stack (replacing it with its child),
	  *              as there will be nothing more to do with that node.
	  */
	private[DirectLongSet] class LongTrieIterator(stack :Array[DirectLongSet], private[this] var top :Int)
		extends BaseIterator[Long] with FitIterator[Long]
	{
		override protected[this] def mySpecialization = Specialized.OfLong

		def this(trie: DirectLongSet) = {
			this(new Array[DirectLongSet](64), 0)
			stack(0) = trie
			descend()
		}

		override def hasDefiniteSize = true

		override def hasNext = top >= 0

		override def head: Long = stack(top).head

		override def next(): Long = {
			val res = stack(top).head
			skip()
			res
		}

		final def skip(): Unit = {
			stack(top) = null
			top -= 1
			if (top >= 0 && stack(top).size > 1) {
				stack(top) = stack(top).asInstanceOf[Branch].right
				descend()
			}
		}

		@tailrec private def descend(): Unit = stack(top).size match {
			case n if n > 1 => //descend into the left tree
				stack(top + 1) = stack(top).asInstanceOf[Branch].left
				top += 1
				descend()
			case 0 => //blind path, rewind and check the right subtree
				stack(top) = null
				top -= 1
				if (top >= 0) { //replace our parent with its right child, as we won't ever need to look at it again
					stack(top) = stack(top).asInstanceOf[Branch].right
					descend()
				}

			case _ => //found 'leftmost' singleton - minimum in the tree.
		}

		override final def drop(n: Int) = {
			if (n > 0 && top >= 0) {
				var left = n - 1
				stack(top) = null
				top -= 1
				while (top >= 0 && left > 0) {
					//the outer loop retreats up the tree dropping right subtrees of size smaller than remaining elements to drop
					val nextTree = stack(top).asInstanceOf[Branch].right
					if (nextTree.size <= left) {
						//covers singletons and empty sets
						left -= nextTree.size
						stack(top) = null
						top -= 1
					} else {
						//now the next right child up is larger than we need; go down left as long as the tree contains non-dropped elements
						stack(top) = nextTree//.asInstanceOf[Branch].left
						while (stack(top).size > left) {
							//left is still >=1, so stack(top) has to be an inner node to enter loop
							stack(top + 1) = stack(top).asInstanceOf[Branch].left
							top += 1
						} //now we are at a node in the tree which has to be dropped in its entirety, so we return to the main retreating loop
						left -= stack(top).size
						stack(top) = null
						top -= 1
					}
				} //stack(top) - if it exists - is a Branch
				if (top>=0) {
					stack(top) = stack(top).asInstanceOf[Branch].right
					descend()
				}
			}
			this
		}


		override def foreach[@specialized(Unit) U](f: (Long) => U) =
			if (top>=0) {
				f(stack(top).head)
				stack(top) = null; top -= 1
				while(top >= 0) {
					stack(top).asInstanceOf[Branch].right.foreach(f)
					stack(top) = null; top-=1
				}
			}

		override protected def forall(p: (Long) => Boolean, ourTruth: Boolean) =
			top <=0 || p(stack(top).head) == ourTruth && {
				stack(top) = null; top -=1
				if (ourTruth)
					while(top>=0 && stack(top).asInstanceOf[Branch].right.forall(p)) {
						stack(top) = null; top -= 1
					}
				else
					while(top>=0 && !stack(top).asInstanceOf[Branch].right.exists(p)) {
						stack(top) = null; top -= 1
					}
				top < 0
			}


		override def count(p: (Long) => Boolean) = {
			var res = 0
			foreach { l => if (p(l)) res+=1 }
			res
		}
	}



}
*/
