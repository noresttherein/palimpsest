package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
import net.turambar.palimpsest.specialty.iterables.{EmptyIterable, EmptyIterableTemplate}
import net.turambar.palimpsest.specialty.sets.MutableTrieValueSetFoundation
import net.turambar.palimpsest.specialty.tries.BinaryTrieFoundation.{MutableTrieBranch, ValueTrieTemplate}
import net.turambar.palimpsest.specialty.tries.Trie.{MutableTrieRoot, TrieCombinator, TriePatch}

import scala.annotation.{tailrec, unspecialized}
import scala.collection.mutable



/** Partial, protected interface of trie set implementations, providing methods needed for
  * interoperability between between stable trie sets and their mutable adaptations.
  * While not really required, extending classes will likely mix in also [[net.turambar.palimpsest.specialty.sets.TrieSetValues]]
  * to provide the 'public' set interface and a concrete [[Trie]] implementation serving as
  * the back-end storage of this set. They will also delegate any abstract methods in the former
  * to implementations provided by the trie.
  *
  * @see [[net.turambar.palimpsest.specialty.sets.TrieSetValues]]
  * @author Marcin Mościcki
  */
trait TrieKeys[@specialized(Int, Long) K, V, T <: TrieKeys[K, V, T]]
	extends Trie[K, V, T] with TriePatch[K, V, T]/* with TrieCombinator[T]*/ with mutable.Cloneable[T]
{
	/** Grants access to this instance's [[Trie#combine]] method for all members of the `sets` package. */
	@inline final private[palimpsest] def merge(other :T)(combinator :TrieCombinator[T]) :T =
		combine(other, combinator)


	/** An immutable copy of this trie. If this trie is already immutable, it can - and likely will -
	  * return itself. Mutable tries should create a fresh copy of their current contents to guarantee
	  * the returned trie won't be modified externally.
	  * @return an immutable copy of this trie, safe to expose to the outside world as an immutable structure.
	  */
	def stable :T = asTrie

	/** A variant of this trie which state can be mutated by external objects.
	  * Used as the basis for implementations of mutable collections such as [[net.turambar.palimpsest.specialty.sets.MutableSet]],
	  * but likely aren't fully mutable themselves, being restricted by the class of the root node.
	  * Proper immutable instances will create a (potentially lazy) copy of themselves using specific
	  * mutable nodes. If this instance is already a specialized mutable trie, it should return itself.
	  * For this reason, this method shouldn't be use to obtain a 'fresh' mutable copy for new a trie of unknown type.
	  * In order to obtain a mutable copy which is guaranteed to be independent of this trie, one should use
	  * `editable.clone()`, `clone().editable` or the shortcut of [[editableCopy]].
	  * This should copy the contents of the trie at most once, regardless of whether this instance is mutable or immutable.
	  * @return a trie set with the same elements and structure as this instance (possibly this very instance)
	  *         which can be used to store mutable contents by [[MutableTrieValueSetFoundation]].
	  * @see [[TrieKeys#editableCopy]]
	  */
	private[palimpsest] def editable :T = asTrie

	/** Equivalent to `clone().editable`, this method always creates a fresh copy of this set, intended
	  * for use by [[MutableTrieValueSetFoundation]].
	  * @see [[editable]]
	  */
	private[palimpsest] def editableCopy :T = clone().editable


	/** A trie with the same contents as this one, with a guarantee that it won't be modified externally
	  * (via other reference than the returned once), and that any modification made to it - if possible -
	  * won't affect any other trie, in particular this one. If the underlying trie is a view of another trie
	  * or maintains references to objects which are not part of the publicly visible content, preventing them
	  * from garbage collection, it is recommended - but not required - that this method creates a  'compacted'
	  * copy. Subclasses are however free to implement it according to their will within this requirements,
	  * in particular immutable tries can just return themselves.
	  * @return a referentially equivalent trie to this one.
	  */
	override def clone() :T = asTrie


//	override protected[this] def mutate(root: MutableTrieRoot[T], parent :MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
//		parent.hang(patch(root, key, mutant))
//
//	override protected[this] def mutate(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
//		root.hang(patch(root, key, mutant))


//
//
//	override protected[this] def unionTrie(other: T) = combine(other, this)
//
//	/** These methods are used to implement `++`/`union` via `combine`. */
//	override def emptyFirst(right: T): T = right.editable
//	override def emptySecond(left: T): T = left.editable
//	override def matched(left: T, right: T): T = left.editable
}




object TrieKeys {

/*
//	abstract class EmptyTrieKeys[@specialized(Int, Long) K, V, +E, T <: TrieKeys[K, V, T]]
//		extends EmptyIterable[E, T] with EmptyTrie[K, V, T]/* with EmptyIterableTemplate[V, T]*/ with TrieKeys[K, V, T]
	trait EmptyTrieKeys[@specialized(Int, Long) K, V, T <: TrieKeys[K, V, T]] extends EmptyTrie[K, V, T] with TrieKeys[K, V, T]
	{

		override protected[this] def patch(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): T =
			mutant.notFound(key, this) match {
				case e if e.isEmpty => e
				case leaf => root.size_++(); leaf
			}


		override protected[this] def mutate(root: MutableTrieRoot[T], key :K, mutant: TriePatch[K, V, T]) =
			mutant.notFound(key, this) match {
				case e if e.isEmpty => ()
				case newLeaf => root.size_++(); root.hang(newLeaf)
			}

		override protected[this] def mutate(root: MutableTrieRoot[T], parent :MutableTrieRoot[T], key :K, mutant: TriePatch[K, V, T]) =
			mutant.notFound(key, this) match {
				case e if e.isEmpty => ()
				case newLeaf => root.size_++(); parent.hang(newLeaf)
			}


//		override def empty :T = asTrie
//		override final def stable :T = asTrie
//		override final def editable :T = asTrie
//		override final def clone() :T = asTrie

	}


	trait TrieLeafKey[@specialized(Int, Long) K, V, T <: TrieKeys[K, V, T]]
		extends TrieLeaf[K, V, T] with TrieKeys[K, V, T]
	{ //this :T =>


		override final def stable :T = asTrie
		override final def editable :T = asTrie
		override final def clone() :T = asTrie

	}
*/




	/** Base class for all binary trie sets with at least two elements. In itself, it serves
	  * a niche function of adapting a stable (immutable) trie for use by mutable collections.
	  * When adding an immutable trie to a mutable one as an independent subtrie, tries of size above
	  * a predefined threshold do not clone their data into mutable/editable instances, but instead
	  * return an instance of this class, which marks any branches below it as immutable and prevents
	  * their modification. Only when an in-place update of this subtrie is requested, this instance
	  * is converted into an [[EditableTrieKeys]] reflecting the modified contents.
	  *
	  * Grouping both immutable and mutable sets under single base class allows for rather seamless interoperability
	  * between them, resulting in more efficient handling of cases such as adding an immutable set to a mutable one.
	  * It also limits class file size for all classes by reusing common set methods and static forwarders to implemented interfaces.
	  *
	  * @see [[EditableTrieKeys]]
	  * @see [[StableTrieKeys]]
	  */
	trait BinaryTrieKeys[@specialized(Int, Long) K, V, T <: TrieKeys[K, V, T]]
		extends BinaryTrie[K, V, T] with TrieKeys[K, V, T] with TrieCombinator[T]
	{
//		override def head = left.head
//		override def last = right.last


//todo: move it to a TrieValues trait...
/*
		protected override def verifiedCopyTo(xs: Array[V], start: Int, total: Int) :Int = {
			def cpy(trie :T, pos :Int, left :Int) :Int = trie match {
				case branch :BinaryTrieKeys[K, V, T] =>
					val l = cpy(branch.left, pos, left)
					if (l < left)
						l + cpy(branch.right, pos + l, left-l)
					else l
				//				case leaf :TrieValueSet1[K, V, T] => //xs(pos) = leaf.value
				//					ValSet.friendCopy(trie, xs, pos, left)
				case _ :TrieLeaf[K, V, T] =>
					xs(start) = trie.value; 1
				case _ :EmptyTrie[K, V, T] => 0
				case _ => //trie.copyToFitArray(xs, pos, left)
					ValSet.friendCopy(trie, xs, pos, left)
			}

			if (total>0) {
				cpy(asTrie, start, total); math.min(size, total)
			} else 0
		}
*/

//		override protected[this] def mutate(root: MutableTrieRoot[T], parent :MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
//			parent.hang(patch(root, key, mutant))
//
//		override protected[this] def mutate(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
//			root.hang(patch(root, key, mutant))
//
//
		override protected[this] def unionTrie(other: T) = combine(other, this)

		/** These methods are used to implement `++`/`union` via `combine`. */
		override def emptyFirst(right: T): T = right.editable
		override def emptySecond(left: T): T = left//.editable
		override def matched(left: T, right: T): T = left//.editable

//		override def disjoint(first: T, second: T): T = editableCopy(first.editable, second.editable)



		/** Creates a new lazy mutable trie reusing the immutable contents of this node.
		  * In this class, it means both of the branches, but subclasses representing other node types
		  * will override this method. The intent is that wherever this is an effectively immutable or mutable
		  * trie, the returned trie will be independent of this instance, with any modifications (if possible)
		  * to any of the copies invisible to the other.
		  */
		override def clone() :T = asTrie //copy(left, right) //create a new lazy trie instance, reusing the immutable branches.


		override def editable :T = asTrie

		//		protected[this] def editableCopy(l :T, r :T) :T
		//todo: all slice/drop methods from BinaryTrie may return directly any of our children (or their subtries)
		//it is not sufficient to override copy
		//		override def stable = new StableLongTrieSetN(center, left, right)


	}



	/** A non-immutable trie branch used internally by [[MutableTrieValueSetFoundation]]. By implementing [[MutableTrieBranch]],
	  * it allows in-place modification of its subtries. Note that it is not a full implementation of a mutable
	  * set as in [[net.turambar.palimpsest.specialty.sets.MutableSet]]/[[collection.mutable.Set]], and should be treated more as data than a
	  * first-class entity.
	  */
	trait EditableTrieKeys[@specialized(Int, Long) K, V, T <: TrieKeys[K, V, T]]
		extends BinaryTrieKeys[K, V, T] with MutableTrieBranch[K, V, T]
	{


		override def emptySecond(left: T): T = left
		override def matched(left: T, right: T): T = left

		//		override def disjoint(first: T, second: T): T


		//		override def stable = new StableLongTrieSetN(center, left.stable, right.stable)
		//		override def editable :T = this
		override def clone() :T = copy(left.clone(), right.clone())
	}




	/** Proper implementation of really immutable tries with at least two leaves,
	  * used wherever the application requires a reference to an immutable collection,
	  * unlike the other trie branch classes here, which are used only internally by mutable set implementations.
	  */
	trait StableTrieKeys[@specialized(Int, Long) K, V, T <: TrieKeys[K, V, T]]
//		extends BinaryTrieKeys[K, V, T] with OfKnownSize
		extends BinaryTrie[K, V, T] with TrieKeys[K, V, T] with TrieCombinator[T] with OfKnownSize
	{

		/** These methods are used to implement `++`/`union` via `combine`. */
		override def emptyFirst(right: T): T = right.stable
		override def emptySecond(left: T): T = left
		override def matched(left: T, right: T): T = left

		override def disjoint(first: T, second: T): T


		override def leaf(idx: Int) :T =
			if (idx<0 || idx>=size)
				empty
			else {
				@tailrec def skip(trie :T, n :Int) :T = trie match {
					case branch :BinaryTrie[K, V, T] =>
						val lsize = branch.left.size
						if (n < lsize) skip(branch.left, n)
						else skip(branch.right, n-lsize)
					case _ :TrieLeaf[K, V, T] => trie
				}
				if (idx < left.size)
					skip(left, idx)
				else
					skip(right, idx - left.size)
			}



		override def stable :T = asTrie //this

		//		override def editable =
		//			if (size <= 8) new EditableLongTrieSet(center, left.editable, right.editable)
		//			else new LongTrieSetNode(center, left, right)

	}



}