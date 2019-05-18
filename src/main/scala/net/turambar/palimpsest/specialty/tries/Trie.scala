package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.slang.Nullable
import net.turambar.palimpsest.specialty.iterators.BaseIterator
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.{Fun1, Fun2}
import net.turambar.palimpsest.specialty.tries.Trie._
import net.turambar.palimpsest.specialty.{?, Blank, RuntimeType, Specialize, Sure}
import net.turambar.palimpsest.specialty.Var
import net.turambar.palimpsest.specialty.iterables.IterableTemplate

import scala.annotation.{tailrec, unspecialized}
import scala.collection.{mutable, GenTraversableOnce}

/** Non-specialized base trait for [[Trie]] implementations. While the [[Trie]] trait is considered the 'proper' base type
  * for trie implementations and no concrete class is expected to extend only this trait, it is useful for two reasons.
  * Firstly, abstract non-specialized classes (and possibly traits) inherit only non-specialized trait implementations
  * (at least for generic formal type parameters); similarly, a specialized class can inherit only a generic version of
  * a specialized trait. These situations are problematic in themselves, but can also cause problems in class linearization,
  * when a generic trait is separated from its specialized variant by another trait. Such cases can break the wiring
  * responsible for specialization magic and cause difficult bugs.
  *
  * Secondly, tries are used as collections, but at this abstraction level we don't want to specify of what
  * element type exactly - it is most practical to add that as the last detail in the inheritance hierarchy and freely
  * reuse implementations. Nevertheless, having methods such as `tail` declared here makes a lot of sense,
  * as they are universally useful and can be potentially implemented without knowledge of the concrete element type,
  * but this would lead to double declarations and unnecessary delegations or inheritance conflicts .
  *
  * For this reason, this trait extends [[IterableTemplate]]`[Any, T]`. This provides
  * access to most of the useful collection methods without any need for an override further down. Understandingly,
  * this trait doesn't cover methods which would need specialization for the element type of this collection. We declare
  * several appropriately named analogues working with the whole nodes `T` containing the elements of this trie.
  * Methods such as [[TrieTemplate#headNode]] and [[TrieTemplate#forEachNode]] can be later easily adapted
  * for implementation of the corresponding `Iterable` methods by derived classes which will finally mix in
  * a collection trait for  the appropriate element and collection type.
  *
  * Unless otherwise stated by an implementing class, only leaves are expected to contain actual elements, with inner nodes
  * serving only navigational purpose.
  *
  * @tparam K key type of this trie, used to organize the inner structure of this trie
  * @tparam T self type of this trie; for all instances it will always be `this.type &lt;: T with Trie[K, T]`,
  *           but the type here is unbounded to simplify corner case implementations.
  * @see [[Trie]]
  */
trait TrieTemplate[+K, +T] { //extends IterableTemplate[Any, T] { //this :Trie[K, V, T] =>

	/** This instance returned as the self type `T`. */
	def asTrie :T

	/** An empty trie of the same type as this. For mutable classes this should return a new/non shared instance. */
 	def emptyTrie :T

//	/** An empty trie of common type `T`. */
//	def empty :T
//
	/** The size of a trie is by default understood as the number of different keys in it.
	  * In most cases this will equate the number of leaves, as only they would hold full key values.
	  */
	def size :Int

	def ofAtLeast(elems :Int) :Boolean

	def tail :T

	def init :T


	/** True if the trie contains at least one leaf and thus is not an empty trie, but either a leaf singleton or a branch node.  */
	def nonEmpty :Boolean

	def isEmpty :Boolean

	/** Magnitude of the number of paths leading down from this node. For binary tries it is always in the range of `0..2`.
	  * Other implementations, with nodes containing possibly a larger number of children, larger values are possible.
	  * @return `0` for empty tries, `1` for leaves and another value for inner nodes.
	  */
	def plurality :Int

	/** For value nodes, it is the value of the key which defines the location of the element it represents in this collection.
	  * Most implementations will store keys and values only in dedicated leaves to simplify implementation, but this
	  * is not strictly required.
	  * For branch (inner) nodes, the value is implementation dependent and generally represents partial information about
	  * stored elements / path to this node. For empty values it is undefined and should not be called.
	  */
	def key :K



	/** The key associated with this node if it carries a value.
	  * @return `Some(key)` for nodes with an associated key and `None` for inner nodes and empty tries.
	  */
	def keyOpt :Option[K]

	/** The key possibly associated with this trie node as an [[net.turambar.palimpsest.specialty.Unsure]] value.
	  * @return `Sure(key)` for nodes with an associated key and `Blank` for inner nodes and empty tries.
	  */
	def key_? : ?[K]


	/** Returns the node of this trie containing the `n`-th key in default traversal of this trie.
	  * @param n a non-negative in/out parameter carrying the index of the demanded node. After this method returns,
	  *          it is less than zero if the size of this trie was greater than its initial value and returned trie node
	  *          contains indeed the requested key. Otherwise - when `n &gt;= this.size` - `n.value` is decreased by `this.size`
	  *          and an empty trie is returned. The meeting point of both cases is when `size+1 == n`, in which case
	  *          the last node is returned and `n` will become zero after method's completion.
	  * @return the node in this trie carrying the `n`-th key if `0 &lt;= n &lt; this.size` or an empty trie otherwise.
	  */
	def keyNode(n :Var[Int]) :T


	/** The node in this trie carrying its first value, if any. Empty tries return themselves.
	  * Unless specified otherwise, it is the leftmost leaf.
	  */
	def headNode :T

	/** Leaf of this trie carrying its last value, if any. Empty tries return themselves. */
	def lastNode :T



	/** Same as standard collections' `drop(n :Int)`, but `count` is an ''in/out'' parameter. It is decreased by one for
	  * every key dropped. The motivation for this method is the desire to avoid calls of `size` on subtries, which might be `O(n)`.
	  * @param count number of leading keys to drop from this trie. After method's completion its value is lesser by the number
	  *              of keys actually dropped.
	  * @return trie resulting from dropping first `count` keys from this trie. It contains the number of keys equal to
	  *         `this.size - (count`&lt;''before''&gt;` - count`&lt;''after''&gt;`)`.
	  */
	def dropTrie(count :Var[Int]) :T

	/** Same as standard collections' `take(n :Int)`, but `count` is an ''in/out'' parameter. It is decreased by one for
	  * every key taken. The motivation for this method is the desire to avoid calls of `size` on subtries, which might be `O(n)`.
	  * @param count number of leading keys to take from this trie. After the call it is lesser by the size of the returned trie.
	  * @return trie resulting from taking first `count` keys from this trie. Its size is equal to
	  *         `count`&lt;''before ''&gt; - `count`&lt;''after''&gt;.
	  */
	def takeTrie(count :Var[Int]) :T


	/** Same as standard collections' `dropRight(n :Int)`, but `count` is an ''in/out'' parameter. It is decreased by one for
	  * every key dropped. The motivation for this method is the desire to avoid calls of `size` on subtries, which might be `O(n)`.
	  * @param count number of trailing keys to drop from this trie. After method's completion its value is lesser by the number
	  *              of keys actually dropped.
	  * @return trie resulting from dropping last `count` keys from this trie. It contains the number of keys equal to
	  *         `this.size - (count`&lt;''before''&gt;` - count`&lt;''after''&gt;`)`.
	  */
	def dropRightTrie(count :Var[Int]) :T


	/** Same as standard collections' `takeRight(n :Int)`, but `count` is an ''in/out'' parameter. It is decreased by one for
	  * every key taken. The motivation for this method is the desire to avoid calls of `size` on subtries, which might be `O(n)`.
	  * @param count number of leading keys to take from this trie. After the call it is lesser by the size of the returned trie.
	  * @return trie resulting from taking first `count` keys from this trie. Its size is equal to
	  *         `count`&lt;''before ''&gt; - `count`&lt;''after''&gt;.
	  */
	def takeRightTrie(count :Var[Int]) :T

	/** Similar to standard collections' `slice` method, but instead of index range, takes the index of the first desired
	  * element and number of elements to be included in the slice. Equivalent to `dropTrie(start).takeTrie(size)`, but
	  * possibly faster, especilly for small slices. The arguments are in/out parameters; the first one is decreased
	  * by one for every node dropped before the requested slice, and the second one for every element taken.
	  * If the first is non-zero after method's return, returned slice is an empty trie. If the second one is non-zero
	  * after method's return, returned slice is smaller than requested due to not sufficient number of elements in this trie.
	  * @param start ''In'': index of the first element to include. ''Out'': `max(0, this.size-start_{init}`.
	  * @param size ''In'': number of elements following `start` to include in the returned trie. ''Out'': initial value
	  *            less the size of the returned trie.
	  * @return an empty trie if this trie has no more than `start` keys, or a subtrie containing `min(this.size-start, size)`
	  *         keys following `start` in this trie.
	  */
	def sliceTrie(start :Var[Int], size :Var[Int]) :T

	/** Similar to standard collection's `splitAt` method, but the argument is an in/out parameter.
	  * @param idx number of leading keys to include in the first subtrie of the returned two. After method's return
	  *            contains number of keys by which this trie (and the first returned trie) is smaller than the initial value.
	  * @return a two of tries, the first containing `min(this.size, idx)` leading keys, the second all following keys.
	  */
	def splitTrie(idx :Var[Int]) :(T, T)



	//todo: iterate :Iterator[T] ???


}







/** Base trait for collections which can be represented, either internally or externally, as `key-value` pairs
  * ordered (or at least grouped) by their keys. It is a tree of recursive structure with elements only in the leaf nodes,
  * where inner nodes represent sub-collections with 'closer' key values - usually understood as sharing a prefix of the key in some sense.
  * Implementations are free to introduce any number of subclasses to represent the nodes according to their needs, and therefore
  * there are no definite case classes for particular nodes. However, three default base traits/classes are introduced
  * for most sensible division: [[EmptyTrie]], [[TrieLeaf]] and [[TrieBranch]].
  *
  * This trait is intended as an implementation interface, a bridge between implementation and standard
  * collection types rather than public API.
  * It extends ''IterableLike'' to gain common root declarations of collection methods and thus reduce conflicts in overrides,
  * but doesn't specify what it is actually a collection of; for this reason, an implementation of a this trie can
  * serve as a base class for various collection types, in particular both sets and maps.
  *
  * @tparam K type of the keys in this trie used during lookup, associated with leaves and defining their location in the trie.
  *           Most often an internal type, not exposed as part of the public interface (such as with hash codes).
  * @tparam T 'self type' of this trie, that is the `Repr` in `InterableLike`.
  * @author Marcin Mościcki
  */
trait Trie[@specialized(KeyTypes) +K, +T] extends TrieTemplate[K, T] { this :T => //self type needed only for asTrie

	def asTrie :T = this

	/** For leaf nodes, it is the value of the key which defines the position of the element it represents in this collection.
	  * For branch (inner) nodes, the value is implementation dependent and generally represents partial information about
	  * stored elements / path to this node.
	  * For empty values it is undefined and should not be called.
	  * Overriden for specialization.
	  */
	def key :K



	/** Checks if this trie contains a leaf for the corresponding key. */
	protected[this] def hasKey(key :K) :Boolean //= nodeFor(key).nonEmpty

//	/** Performs a quick check if it is theoretically possible for the given key to be present in this node.
//	  * Designed as a quick (''O(1)'') filter for key lookup.
//	  * @return `true` if the possibility of the given key belonging to this trie can be ruled out.
//	  */
	protected[this] def belongs(key :K) :Boolean


	/** Returns a leaf for the given key in this node. If there is no such key in this collection,
	  * an empty trie of the appropriate type is returned instead (as defined by [[Trie#isEmpty]] and [[Trie#plurality]]).
	  * Used as a basis for lookup implementations.
	  */
	protected[this] def nodeFor(key :K) :T



/*
	/** Applies the given function to all keys stored in leaves in this collection, in the same order
	  * as eventual `foreach` and `iterator` methods.
	  */
	def forEachKey(f :K=>Unit) :Unit

	def forEachKeyReversed(f :K=>Unit) :Unit


	/** Verifies if there is a leaf in this trie, for which key the given predicate is true. */
	def existsKey(f :K=>Boolean) :Boolean

	def forAllKeys(f :K=>Boolean) :Boolean

	def filterKeys(f :K=>Boolean) :T

	def filterKeysNot(f :K=>Boolean) :T

	def findKey(f: K=>Boolean) :Option[K]
*/

	/** A folding function working on subtries of this trie rooted in nodes on the path to the leaf containing the given key,
	  * in the reverse order of their positions on that path.
	  * It descends recursively down this trie to find the leaf holding key `key`. If found, the function `r` is applied to the given
	  * seed `z` and then recursion stack is rewound, applying `f` to its last result and the subtrie from that level
	  * of recursion, up until to the whole trie `this`.
	  * @tparam O accumulator type
	  * @param  z initial seed for the accumulator
	  * @param  key a key (possibly) identifying a leaf in this trie
	  * @param  f folding function
	  * @see {{{Trie.foldPath}}}
	  */
	//todo: does this method even make sense with the same initial accumulator regardless if this trie has the key?
//	protected[this] def foldNodeUp[@specialized(Fun2) O](z :O, key :K)(f :(O, T) => O) :O



}







object Trie {



	/** An alias to document an `Int` value as a discriminator of the trie node type, as returned and specified by [[Trie#plurality]].
	  * @see [[EmptyNode]]
	  * @see [[LeafNode]]
	  * @see [[BranchNode]]
	  */
	type NodeType = Int

	/** Discriminator of empty trie instances returned by their [[Trie#plurality]] methods. */
	@inline final val EmptyNode = 0

	/** Discriminator of singleton trie instances with a single leaf, returned by their [[Trie#plurality]] methods. */
	@inline final val LeafNode = 1

	/** Discriminator of inner trie node instances, representing collections of at least two leaves, returned by their [[Trie#plurality]] methods. */
	@inline final val BranchNode = 2


	/** Key types for which tries are specialized. */
	final val KeyTypes = new Specializable.Group((Int, Long))



	trait MutableTrieOwner[-T] {
		private[tries] def updateTrie(node :T) :Unit
	}


	/** A modification concerning a single key in a trie which can be used to implement
	  * key removal/addition or leaf modification, in mutable or immutable tries, all while remaining
	  * relatively ignorant about the particular trie structure and thus being more reusable.
	  * Used in conjunction with [[Trie#reducePath]] which implements actual path traversal to
	  * the leaf for the given key - or place in the trie where it should be placed - and invoking the method on
	  * this object appropriate to the case encountered. Once the appropriate node for the key has been found
	  * (either associated with the key or closest to it if the key is missing), one of the callback methods of this
	  * instance is invoked to obtain an initial value, depending on the case encountered. That value is then 'folded up'
	  * along the path leading to that leaf, allowing this operation to incrementally modify the result by combining it
	  * with information from subsequent parent nodes in the trie from the bottom up.
	  * @tparam K key type in the tries this operation works for
	  * @tparam T the type of tries this operation works for
	  * @tparam O result type computed based on the path in the trie associated with the given key.
	  */
	trait FoldPath[@specialized(KeyTypes) -K, -T, @specialized(TrieOpRes) O] {

		/** Callback invoked if and only if the fold operation is applied to an empty trie. Invoking [[Trie#foldPath]] for
		  * non-empty tries will not end up in this case, but rather [[FoldPath#whenNoKey]] or [[FoldPath#whenKeyExists]].
		  *
		  * @param key the key which was passed to [[Trie#foldPath]] together with this operation.
		  * @param empty the empty trie to which this operation was applied
		  * @return by default simply forwards to [[FoldPath#whenNoKey]]`(key, empty)`.
		  */
		def whenTrieEmpty(key :K, empty :T) :O //= whenNoKey(key, empty)

		/** Callback invoked when the descent down the trie proves that no leaf for the given `key` exists in the trie.
		  * Following its return, recursion which descended to this leaf rolls back up the path it took, invoking
		  * [[FoldPath#foldUp]] for all nodes along the way from the bottom up, passing the value computed previously.
		  *
		  * @param key the key which was passed to [[Trie#foldPath]] together with this operation.
		  * @param closest the leaf in the trie which is closest to `key` and could be replaced either with a leaf
		  *                with `key` or their union.
		  * @return a value which is appropriate for a singleton trie, which does not contain `key`, serving as
		  *         initial value for the accumulator used when folding up.
		  */
		def whenNoKey(key :K, closest :T) :O

		/** Callback invoked when a leaf associated with the given key is present in the trie.
		  * The value returned here is subsequently folded up the path leading to this leaf,
		  * combining partial results for the subtries with ancestor nodes one by one.
		  * @param key the key which was passed to [[Trie#foldPath]] together with this operation.
		  * @param leaf the leaf associated with this key (i.e. `leaf.key == key`, unless some shenanigans are involved).
		  * @return a value appropriate for a singleton trie containing `key`, serving as the initial value
		  *         for the accumulator used when folding up.
		  */
		def whenKeyExists(key :K, leaf :T) :O

		/** Callback invoked in the reverse order for each `parent -> child` two of nodes on the path in the trie
		  * associated with `key`. When [[Trie#doForLeaf]] is executed for tries with more than one leaf, the value
		  * returned by the method of this object appropriate to the given case is subsequently folded with the
		  * parents of previous nodes while recursion retreats up the trie. So, the first call to this method
		  * will pass the same `key` and `child` node that was passed to the leaf-based callback, together with
		  * the parent of that leaf in the original trie. Consecutive calls will then pass the `parent` as the `child`,
		  * together with the previously computed partial result.
		  * @param key the key which was passed to [[Trie#foldPath]] together with this operation, same for all the calls
		  *            being part of the same path traversal.
		  * @param parent the parent node of the `child` in the original trie.
		  * @param child the node for which `res` was previously computed - same as the leaf/parent argument of the
		  *              previous call.
		  * @param res the most recently returned value by this operator
		  * @return result of combining accumulator value `res` with the parent of the node for which it was computed.
		  */
		def foldUp(key :K, parent :T, child :T, res :O) :O
	}


	object FoldPath {
		def apply[@specialized(Int, Long) K, T, @specialized(TrieOpRes) O](z :O)(f :(T, O) => O) :FoldPath[K, T, O] =
			new FunctionFold(z, f)

		class FunctionFold[@specialized(Int, Long) K, -T, @specialized(TrieOpRes) O](z :O, f :(T, O) => O) extends FoldPath[K, T, O] {

			override def whenTrieEmpty(key :K, empty :T) :O = z

			override def whenNoKey(key: K, closest: T): O = z

			override def whenKeyExists(key: K, leaf: T): O = f(leaf, z)

			override def foldUp(key: K, parent: T, child: T, res: O): O = f(parent, res)
		}
	}





	/** Result types for which binary trie operators and trie patches are specialized. */
	final val TrieOpRes = new Specializable.Group((Int, Long, Boolean, Unit))

}
