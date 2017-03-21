package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.FitIterator.BaseIterator
import net.turambar.palimpsest.specialty.Specialized.Fun2
import net.turambar.palimpsest.specialty.iterables.{EmptyIterable, EmptyIterableTemplate, SingletonFoundation, SingletonTemplate}
import net.turambar.palimpsest.specialty.maps.LongTrie.BranchTrie
import net.turambar.palimpsest.specialty.sets.TrieHashSet
import net.turambar.palimpsest.specialty.tries.Trie.{MutableTrieRoot, TrieCombinator, TrieOperator, TriePatch}
import net.turambar.palimpsest.specialty.{Elements, FitIterator, IterableSpecialization, IterableTemplate, Specialize, Specialized}

import scala.annotation.{tailrec, unspecialized}
import scala.collection.{GenTraversableOnce, mutable}

/** Unspecialized base trait for [[Trie]] implementations. Instead of extending [[Trie]] directly,
  * classes and and traits not specialized for the key type `K` should extend only this generic interface.
  * It allows them access to most trie methods and avoids problems with linearizations in which
  * the generic version of the [[Trie]] trait is separated from the properly specialized version by other classes.
  * Such situations may lead to unexpected behaviours and break the wireing of specialized methods, which can
  * be accidentally overriden by other classes, leading to infinite recursion between the overrides and
  * specialized method variants.
  * @tparam K key type of this tries, used to organize the inner structure of this trie
  * @tparam V type of the value associated with each key
  * @tparam T self type of this trie; for all instance it will always be `this.type <: T with Trie[K, V, T]`,
  *           but the type here is unbounded to simplify corner case implementations.
  * @see [[Trie]]
  */
trait TrieTemplate[+K, +V, +E, +T] extends IterableTemplate[E, T] { //this :Trie[K, V, T] =>
	def asTrie :T
	def empty :T

	/** The size of a trie is understood as the number of leaves in it, as generally only leaves carry actual elements of this collection. */
	def size :Int

	/** True if the trie contains at least one leaf and thus is not an empty trie, but either a leaf singleton or a branch node.  */
	def nonEmpty :Boolean
	/** True if there are no leaves in this trie, which is the case only for special, empty tries. */
	def isEmpty :Boolean

	def plurality :Int

	/** For leaf nodes, it is the value of the key which defines the position of the element it represents in this collection.
	  * For branch (innner) nodes, the value is implementation dependent and generally represents partial information about
	  * stored elements / path to this node.
	  * For empty values it is undefined and should not be called.
	  */
	def key :K

	/** Properly defined only for leaf nodes, it stores (or computes) the value associated with the corresponding key in this collection type.
	  * Calling it for empty or inner nodes is an error which may result in an exception.
	  */
	def value :V



	//	/** For leaf nodes, it is the value of the key which defines the position of the element it represents in this collection.
//	  * For branch (innner) nodes, the value is implementation dependent and generally represents partial information about
//	  * stored elements / path to this node.
//	  * For empty values it is undefined and should not be called.
//	  */
//	def key :K

	def keyOpt :Option[K]

//	/** Properly defined only for leaf nodes, it stores (or computes) the value associated with the corresponding key in this collection type.
//	  * Calling it for empty or inner nodes is an error which may result in an exception.
//	  */
//	def value :V


	def valueOpt :Option[V]

//	/** Checks if this trie contains a leaf for the corresponding key. */
//	protected[this] def hasLeaf(key :K) :Boolean
//
//	/** Performs a quick check if it is theoretically possible for the given key to be present in this node.
//	  * Designed as a quick (''O(1)'') filter for key lookup.
//	  * @return `true` if the possibility of the given key belonging to this trie can be ruled out.
//	  */
//	protected[this] def belongs(key :K) :Boolean
//
//	/** Returns a leaf for the given key in this node. If there is no such key in this collection,
//	  * an empty trie of the appropriate type is returned instead (as defined by [[Trie#isEmpty]] and [[Trie#plurality]]).
//	  * Used as a basis for lookup implementations.
//	  */
//	protected[this] def leafFor(key :K) :T //todo: rename to leaf

	def leaf(n :Int) :T

	def trieHead :T

	def trieLast :T

	def forEachLeaf(f :T=>Unit) :Unit
	//	def forLeaves(f :T => Unit) :Unit
	//
	//
	//	def forLeavesReversed(f :T=>Unit) :Unit

	/** Applies the given function to all keys stored in leaves in this collection, in the same order
	  * as eventual `foreach` and `iterator` methods.
	  */
	def forEachKey(f :K=>Unit) :Unit

	def forEachKeysReversed(f :K=>Unit) :Unit

	/** Applies the given function to all values stored in leaves in this collection, in the same order
	  * as eventual `foreach` and `iterator` methods.
	  */
	def forEachValue(f :V=>Unit) :Unit

	def forEachValueReversed(f :V=>Unit)

	//	def existsLeaf(f :T => Boolean) :Boolean
	//
	def existsLeaf(f :T => Boolean) :Boolean = !forAllLeaves(!f(_))

	def forAllLeaves(f :T => Boolean) :Boolean

	/** Verifies if there is a leaf in this trie, for which key the given predicate is true. */
	def existsKey(f :K=>Boolean) :Boolean

	def forAllKeys(f :K=>Boolean) :Boolean

	/** Verifies if there is a leaf in this trie, for which value the given predicate is true. */
	def existsValue(f :V=>Boolean) :Boolean

	def forAllValues(f :V=>Boolean) :Boolean


	def filterLeaves(f :T=>Boolean) :T

	def filterKeys(f :K=>Boolean) :T

	def filterKeysNot(f :K=>Boolean) :T

	def filterValues(f :V=>Boolean) :T

	def filterValuesNot(f :V=>Boolean) :T


	def findLeaf(f :T=>Boolean) :T

	def findKey(f: K=>Boolean) :Option[K]

	def findValue(f :V=>Boolean) :Option[V]

	private[palimpsest] def properPrefixOrNull(f :T=>Boolean) :T = {
		val res = leafSpan(f)
		if (res._2.asInstanceOf[Iterable[Any]].isEmpty) null.asInstanceOf[T]
		else res._1
	}

	def takeLeaves(f :T=>Boolean) :T = leafSpan(f)._1

	def dropLeaves(f :T=>Boolean) :T = leafSpan(f)._2

	def leafSpan(f :T=>Boolean) :(T, T)

//	protected[this] def patch(key :K, mutant :TriePatch[K, V, T]) :T = patch(MutableTrieRoot[T], key, mutant)
//
//	protected[this] def patch(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]):T
//
//
//	protected[this] def mutate(root :MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
//		mutate(root, root, key, mutant)
//
//	protected[this] def mutate(root :MutableTrieRoot[T], parent :MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit


	/** Given another compatible trie and a binary operator working on this trie type, create a new trie
	  * by recursively applying the operator to corresponding fragments of both tries.
	  * This function recursively follows the trie structure, matching corresponding fragments
	  * of both tries and passing them to the `combinator` operator. Obtained results are then reduced
	  * to form the larger trie in a way defined by this trie (generally same as all filtering/slicing operations).
	  * Recursion stops and yields a trie pair to the combinator when either a trie containing only leaves not present in
	  * the other trie is encountered (in which case it is paired with an empty trie),
	  * when both tries are determined to be disjoint (but generally closely related according to the storage organization in the sense),
	  * or when both tries are determined to contain exactly the same keys (without any constraints on their values).
	  * The latter occurs usually only the level of a individual leaves.
	  *
	  * More formally, the algorithm partitions each trie into mutually disjoint subsets covering the whole trie,
	  * and then pairs the 'corresponding' subtries of both tries. Each such pair can belong to one of three kinds:
	  *   - keys present in only one of the tries are paired with an empty set (possibly grouping several such keys in the same set in a single pair),
	  *   - keys present in both subtries are paired with each other (by default such pairs consist of two singleton sets containing the corresponding leaves),
	  *   - keys present in only one of the tries, but which can be matched with a subtrie occupying the 'same spot' in the other trie,
	  *     are paired as part of the containing subtrie with the corresponding subtrie in the other trie.
	  *
	  * Note that this is a protected method intended for use by subclasses (or other units comprising the implementation
	  * of a particular trie type) and it is up to the subclass to maintain the invariants as well as enforce or verify that
	  * the given trie is really compatible with this instance.
	  * It is used to implement default algorithms for trie unions, differences and intersections - both mutable and immutable.
	  * @param other another trie of the same type
	  * @param combinator a binary operator
	  * @return
	  * @see [[TrieCombinator]]
	  */
	protected[this] def combine(other :T, combinator :TrieCombinator[T]) :T = combine(other, combinator :TrieOperator[T, T])
	protected[this] def combine[O](other :T, operator :TrieOperator[T, O]) :O

//	/** Remove any leaf and all values associated with the given key from this collection. */
//	protected[this] def without(key :K) :T

	/** Combines the elements of this collection with the other collection of the same type, treating both as sets of some sort.
	  * Note that the actual semantics are completely implementation dependent, in particular how leaves with common keys, or of differing
	  * values are treated.
	  * Basis for specialized [[net.turambar.palimpsest.specialty.sets.ValSet#++]] implementations and similar.
	  */
	protected[this] def unionTrie(other :T) :T

	protected[this] def diffTrie(other :T) :T

	protected[this] def intersectTrie(other :T) :T

	/** Verifies if all keys/elements in this collection have a 'match' in the other collection in some sense.
	  * Default target of [[net.turambar.palimpsest.specialty.sets.ValSet#subsetOf]] implementations for cases
	  * where both sets are of the same type, but how exactly a 'match' is defined is implementation dependent.
	  */
	protected[this] def subtrieOf(other :T) :Boolean

}


/** Base trait for collections which can be represented, either internally or externally, as `key-value` pairs
  * ordered (or at least grouped) by their keys. It is a tree of recursive structure with elements only in the leaf nodes,
  * where inner nodes represent sub-collections with 'closer' key values - usually understood as sharing a prefix of the key in some sense.
  * Implementations are free to introduce any number of subclasses to represent the nodes according to their needs, and therefore
  * there are no definite case classes for particular nodes. However, three default base traits/classes are introduced
  * for most sensible division: [[EmptyTrie]], [[TrieLeaf]] and [[BinaryTrie]].
  *
  * This trait is intended as an implementation interface, a bridge between implementation and standard
  * collection types rather than public API.
  * It extends ''IterableLike'' to gain common root declarations of collection methods and thus reduce conflicts in overrides,
  * but doesn't specify what it is actually a collection of; for this reason, an implementation of a this trie can
  * server as a base class for various collection types, in particular both sets and maps.
  * @tparam K type of the keys in this trie used during lookup, associated with leaves and defining their location in the trie.
  *           Most often an internal type, not exposed as part of the public interface (such as with hash codes).
  * @tparam V optional values stored in leaves beside keys, one value per leaf/key. Their meaning is implementation dependent.
  * @tparam T 'self type' of this trie, that is the `Repr` in `InterableLike`. This interface doesn't pose any bounds
  *           on this type or the relationship between 'this' and `T` to simplify implementation,
  *           but actual collections will generally assume that `this.type <: T`.
  * @author Marcin Mościcki
  */
trait Trie[@specialized(Int, Long) +K,/* @specialized(Elements) */+V, +T] extends TrieTemplate[K, V, Any, T] {

	/** For leaf nodes, it is the value of the key which defines the position of the element it represents in this collection.
	  * For branch (innner) nodes, the value is implementation dependent and generally represents partial information about
	  * stored elements / path to this node.
	  * For empty values it is undefined and should not be called.
	  */
	def key :K

	/** Properly defined only for leaf nodes, it stores (or computes) the value associated with the corresponding key in this collection type.
	  * Calling it for empty or inner nodes is an error which may result in an exception.
	  */
	def value :V


	/** Checks if this trie contains a leaf for the corresponding key. */
	protected[this] def hasLeaf(key :K) :Boolean //= leafFor(key).nonEmpty

	/** Performs a quick check if it is theoretically possible for the given key to be present in this node.
	  * Designed as a quick (''O(1)'') filter for key lookup.
	  * @return `true` if the possibility of the given key belonging to this trie can be ruled out.
	  */
	protected[this] def belongs(key :K) :Boolean

	/** Returns a leaf for the given key in this node. If there is no such key in this collection,
	  * an empty trie of the appropriate type is returned instead (as defined by [[Trie#isEmpty]] and [[Trie#plurality]]).
	  * Used as a basis for lookup implementations.
	  */
	protected[this] def leafFor(key :K) :T //todo: rename to leaf


	/** Applies the given function to all keys stored in leaves in this collection, in the same order
	  * as eventual `foreach` and `iterator` methods.
	  */
	def forEachKey(f :K=>Unit) :Unit

	def forEachKeysReversed(f :K=>Unit) :Unit


	/** Verifies if there is a leaf in this trie, for which key the given predicate is true. */
	def existsKey(f :K=>Boolean) :Boolean

	def forAllKeys(f :K=>Boolean) :Boolean

	def filterKeys(f :K=>Boolean) :T

	def filterKeysNot(f :K=>Boolean) :T

	def findKey(f: K=>Boolean) :Option[K]



	protected[this] def patch(key :K, mutant :TriePatch[K, V, T]) :T = patch(MutableTrieRoot[T], key, mutant)

	protected[this] def patch(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]):T


	protected[this] def mutate(root :MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
		mutate(root, root, key, mutant)
//		root.hang(patch(root, key, mutant))

	protected[this] def mutate(root: MutableTrieRoot[T], parent :MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
		parent.hang(patch(root, key, mutant))




	/** Remove any leaf and all values associated with the given key from this collection. */
	protected[this] def without(key :K) :T

}





object Trie {


//	trait KeyTrie[@specialized(Int, Long) K, +V, +T] extends Trie[K, V, T] {
//		override def leafFor(key :K) :T
//		def patched[U>:T](root :MutableTrieRoot[U], key :K, mutant :TriePatch[K, V, U]) :U
//		def patched[U>:T](key :K, mutant :TriePatch[K, V, U]) :U = patched(MutableTrieRoot[U], key, mutant)
//
//		override protected[this] def patch(key: K, mutant: TriePatch[K, V, T]): T =
//			patched(MutableTrieRoot[T], key, mutant)
//	}



	/** An alias to document an `Int` value as a discirminator of the trie node type, as returned and specified by [[Trie#plurality]].
	  * @see [[EmptyNode]]
	  * @see [[LeafNode]]
	  * @see [[BranchNode]]
	  */
	type NodeType = Int

	/** Discriminator of empty trie instances returned by their [[Trie#plurality]] methods. */
	final val EmptyNode = 0
	/** Discriminator of singleton trie instances with a single leaf, returned by their [[Trie#plurality]] methods. */
	final val LeafNode = 1
	/** Discriminator of inner trie node instances, representing collections of at least two leaves, returned by their [[Trie#plurality]] methods. */
	final val BranchNode = 2



	/** A modification of a leaf in a trie.
	  * Can be used to implement any operation involving a trie and a single element yielding another trie
	  * by substituting/modifying/removing/adding the leaf corresponding to the key in the appropriate place in the trie.
	  * Used by [[Trie#patch]], it allows the tries to implement such operations with a single method, by delegating the final modification
	  * to a specific instance of this class. This separates the actual traversal of a trie in search for the element
	  * from the nature of modification implemented, leaving the latter as a concern of trie implementations.
	  *
	  * Note that in order to eliminate unnecessary object creation of these instances for each trie operation,
	  * implementations of this class will often either be a static, global and reusable constant,
	  * or the objects involved will implement this interface themselves; in particular, it makes sense for
	  * concrete [[TrieLeaf]] implementations to implement this trait too, defining insertion/replacement/update
	  * operation, so that no additional object would be needed to be created.
	  *
	  * @tparam K key type of the associated trie type
	  * @tparam V leaf value type in the associated trie type
	  * @tparam T the [[Trie]] this patch works on (unbound here to simplify method declarations).
	  */
	trait TriePatch[@specialized(Int, Long) -K, -V, +T] {

		def notFound[S>:T](key :K, sibling :Trie[K, V, S]) :S


		def updateLeaf[S>:T](oldLeaf :TrieLeaf[K, V, S]) :S
	}

	//	class ReplaceTrie[@specialized(Int, Long) K, V, T](replacement :T) extends TriePatch[K, V, T] {
	//		override def notFound(key: K, sibling :Trie[K, V, T]): T = replacement
	//		override def updateLeaf(oldLeaf: TrieLeaf[K, V, T]): T = replacement
	//	}

	/** A patch removing the leaf associated with a given key - if any - from a trie. Always returns an empty trie. */
	class DeleteTrie[@specialized(Int, Long) K, -V, +T] extends TriePatch[K, V, T] {
		override def notFound[S>:T](key: K, sibling: Trie[K, V, S]): S = sibling.empty
		override def updateLeaf[S>:T](oldLeaf: TrieLeaf[K, V, S]): S = oldLeaf.empty
	}

	object DeleteTrie extends Specialize.For[({ type L[K] = DeleteTrie[K, Any, Nothing] })#L] {
		override final val forInt = new DeleteTrie[Int, Any, Nothing]
		override final val forLong = new DeleteTrie[Long, Any, Nothing]
		override def specialized[@specialized K: Specialized]: DeleteTrie[K, Any, Nothing] = new DeleteTrie[K, Any, Nothing]
	}

	class ReplaceLeaf[@specialized(Int, Long) K, V, T<:Trie[K, V, T]](leaf :T) extends TriePatch[K, V, T] {
		override def notFound[S >: T](key: K, sibling: Trie[K, V, S]): S = leaf
		override def updateLeaf[S >: T](oldLeaf: TrieLeaf[K, V, S]): S = leaf
	}



	trait TrieOperator[-T, @specialized(Int, Boolean, Unit) O] {
//		def apply(left :T, right :T) :O
//		def combine(first :T, second :T) :O
		//todo: pass also the first, empty trie, as second.empty may be of different type
		def emptyFirst(second :T) :O
		def emptySecond(first :T) :O
		def disjoint(first :T, second :T) :O
		def matched(first :T, second :T) :O
		def reduce(res1 :O, res2 :O) :O
		def reduced(original :T)(left :O, right :O) :O = reduce(left, right)
	}


	/** A function `(T, T)=>T`, where `T` is some type of a `Trie` representing a binary trie operation
	  * which can be implemented in the 'divide and conquer' fashion by taking advantage of the similarity
	  * of inner structures in the actual trie type. Allows for common implementation of methods such as set union,
	  * intersection or difference, by separating the semantics of the operation as defined by this class
	  * from the division and traversal of argument collections into the smaller problems (left to the actual `T` implementation).
	  *
	  * It is assumed that implementations may be specific to an individual trie type, and that `T` itself will
	  * be responsible for comparing the elements of both operands and pairing them before passing to this function.
	  * For convenience, the function is split into separate cases, which both occur naturally during comparison
	  * and are likely to be treated specially by the operator.
	  *
	  * Designed to be used with [[LongPrefixTrie.LongTrieBranch#combine(T, TrieCombinator)]].
	  *
	  * @tparam T
	  */
	trait TrieCombinator[T] extends TrieOperator[T, T] {
//		def apply(first :T, second :T) :T

		def emptyFirst(right :T) :T

		def emptySecond(left :T) :T

		def disjoint(left :T, right :T) :T

		def matched(left :T, right :T) :T

//		override def reduce(res1: T, res2: T): T //= disjoint(res1, res2)
}



	/** Factory with default and example implementations of the most commonly required binary collection operations. */
	object TrieCombinator {

		/** Wrap the given function in a [[TrieCombinator]] delegating all its methods to it, assuming
		  * it will discriminate between individual cases itself (or that it doesn't care).
		  */
//		def apply[T <: Trie[_, _, T]](op: (T, T) => T): TrieCombinator[T] = new TrieFunctionCombinator(op)
//
//		class TrieFunctionCombinator[T <: Trie[_, _, T]](op: (T, T) => T) extends TrieCombinator[T] {
//			override def emptyLeft(right: T): T = op(right.empty, right)
//
//			override def emptyRight(left: T): T = op(left, left.empty)
//
//			override def disjoint(left: T, right: T): T = op(left, right)
//
//			override def matched(left: T, right: T): T = op(left, right)
//		}

//		/** Any erased trie making reuse of cast combinator safe. */
//		private type GenericTrie = Trie[_, _, Trie[_, _, _]] //TrieHashSet[Any]
//
//		/** Implements intersection between two collections - if any argument is empty, or if they are disjoint,
//		  * the result is an empty trie. If both are considered equal/equivalent, left argument is returned as-is
//		  * @tparam T any `Trie` type desiring to have intersection implemented this way.
//		  */
//		@inline final def intersection[T<:Trie[_, _, T]]: TrieCombinator[T] = TrieIntersection.asInstanceOf[TrieCombinator[T]]
//
//		private object TrieIntersection extends TrieCombinator[GenericTrie] {
//			override def emptyFirst(right: GenericTrie): GenericTrie = (right :Trie[_, _, GenericTrie]).empty
//			override def emptySecond(left: GenericTrie): GenericTrie = (left :Trie[_, _, GenericTrie]).empty
//			override def disjoint(left: GenericTrie, right: GenericTrie): GenericTrie = (left :Trie[_, _, GenericTrie]).empty
//			override def matched(left: GenericTrie, right: GenericTrie): GenericTrie = left
//		}
//
//		@inline final def cloneIntersection[T <: Trie[_, _, T] with mutable.Cloneable[T]] :TrieCombinator[T] =
//			TrieCloneIntersection.asInstanceOf[TrieCombinator[T]]
//
//		private object TrieCloneIntersection extends TrieCombinator[GenericTrie] {
//			override def emptyFirst(right: GenericTrie): GenericTrie = (right :Trie[_, _, GenericTrie]).empty
//			override def emptySecond(left: GenericTrie): GenericTrie = (left :Trie[_, _, GenericTrie]).empty
//			override def disjoint(left: GenericTrie, right: GenericTrie): GenericTrie = (left :Trie[_, _, GenericTrie]).empty
//			override def matched(left: GenericTrie, right: GenericTrie): GenericTrie = left.clone()
//		}
//
//		/** Implements subtraction of the right trie from the left trie.
//		  * If the right trie is empty or disjoint with the left, left trie is returned as-is.
//		  * If the right trie is equivalent to the left, or left is empty, an empty trie is returned.
//		  * @tparam T any `Trie` type desiring to have subtraction implemented this way.
//		  */
//		@inline final def difference[T<:Trie[_, _, T]]: TrieCombinator[T] = TrieDifference.asInstanceOf[TrieCombinator[T]]
//
//		private object TrieDifference extends TrieCombinator[GenericTrie] {
//			override def emptyFirst(right: GenericTrie): GenericTrie = (right :Trie[_, _, GenericTrie]).empty
//			override def emptySecond(left: GenericTrie): GenericTrie = left
//			override def disjoint(left: GenericTrie, right: GenericTrie): GenericTrie = left
//			override def matched(left: GenericTrie, right: GenericTrie): GenericTrie = (left :Trie[_, _, GenericTrie]).empty
//		}
//
//		@inline final def cloneDifference[T <: Trie[_, _, T] with mutable.Cloneable[T]] :TrieCombinator[T] =
//			TrieCloneDifference.asInstanceOf[TrieCombinator[T]]
//
//		private object TrieCloneDifference extends TrieCombinator[GenericTrie] {
//			override def emptyFirst(right: GenericTrie): GenericTrie = (right :Trie[_, _, GenericTrie]).empty
//			override def emptySecond(left: GenericTrie): GenericTrie = left.clone()
//			override def disjoint(left: GenericTrie, right: GenericTrie): GenericTrie = left.clone()
//			override def matched(left: GenericTrie, right: GenericTrie): GenericTrie = (left :Trie[_, _, GenericTrie]).empty
//		}
/*
		private object TrieUnion extends TrieCombinator[GenericTrie] {
			override def emptyLeft(right: GenericTrie): GenericTrie = right
			override def emptyRight(left: GenericTrie): GenericTrie = left
			override def disjoint(left: GenericTrie, right: GenericTrie): GenericTrie = left.dis
			override def matched(left: GenericTrie, right: GenericTrie): GenericTrie = right
		}
*/

	}


	trait MutableTrieRoot[-T] {
		def hang(replacement :T) :Unit
		def size_++() :Unit = ()
		def size_--()  :Unit = ()
	}


	def MutableTrieRoot[T] :MutableTrieRoot[T] = NoRoot.asInstanceOf[MutableTrieRoot[T]]

	private[this] final val NoRoot = new MutableTrieRoot[Any] {
		override def hang(replacement: Any): Unit = ()
		override def size_++() = ()
		override def size_--() = ()
	}





	/** Base trait for tries which are `Iterable` instance with value type as the element type. */
	trait ValueTrie[+K, +V, T <: TrieTemplate[K, V, V, T]]
		extends TrieTemplate[K, V, V, T] //with IterableTemplate[V, T]
	{ //this :Trie[K, V, T] =>

		override def foreach[@specialized(Unit) U](f: (V) => U) = forEachValue(f.asInstanceOf[V=>Unit])

		override protected def reverseForeach(f: (V) => Unit): Unit = forEachValueReversed(f)

		override def headOption = trieHead.valueOpt
		override def lastOption = trieLast.valueOpt

		override def forall(p: (V) => Boolean) = forAllValues(p)
		override def exists(p: (V) => Boolean) = existsValue(p)
		override def find(p: (V) => Boolean) = findValue(p)

		override def foldLeft[@specialized(Fun2) O](z: O)(op: (O, V) => O) :O = {
			def foldl(trie :T, acc :O) :O = trie match {
				case branch :BinaryTrie[K, V, T] =>
					foldl(branch.right, foldl(branch.left, acc))
				case other => other.foldLeft(acc)(op) //fixme infinite loop possible!
			}
			foldl(asTrie, z)
//			foldl(right, foldl(left, z))
		}

		override def foldRight[@specialized(Fun2) O](z: O)(op: (V, O) => O) :O = {
			def foldr(trie :T, acc :O) :O = trie match {
				case branch :BinaryTrie[K, V, T] =>
					foldr(branch.left, foldr(branch.right, acc))
				case other => other.foldRight(acc)(op) //fixme infinite loop possible!
			}
			foldr(asTrie, z)
//			foldr(left, foldr(right, z))
		}


		override protected[this] def filter(p: (V) => Boolean, ourTruth: Boolean): T =
			if (ourTruth) filterValues(p) else filterValuesNot(p)


/*
		override def takeWhile(p: (V) => Boolean) = {
			def takeWithSize(trie :T) :T = trie match {
				case branch :BinaryTrie[K, V, T] =>
					val l = branch.left.takeWhile(p)
					if ((l eq branch.left) || (l.size == branch.left.size)) {
						val r = branch.right.takeWhile(p)
						if ((r eq branch.right) || (r.size == branch.right.size)) trie
						else if (r.isEmpty) l
						else copy(left, r)
					} else l
				case other =>
			}
		}

		override def dropWhile(p: (V) => Boolean) = super.dropWhile(p)

		override def span(p: (V) => Boolean) = super.span(p)

		override def toFitSeq = super.toFitSeq

		override def toFitBuffer[U >: V : Specialized] = super.toFitBuffer

		override def sameElements[U >: V](that: GenIterable[U]) = super.sameElements(that)
*/
	}




//	trait TrieKeys[+K, +V, +T<:TrieKeys]


	/** Creates an initial stack for use by a [[BinaryTrieIterator]] iterating over all leaves in the given trie.
	  * Simply creates the array of the size corresponding to maximal potential depth of the stack given as `depth`,
	  * with the given trie in its first cell. Resulting array can be passed to the iterator constructor, specifying
	  * `0` as the index of the top stack element. The iterator itself will fill the stack by descending to the first leaf
	  * in iteration order.
	  */
	@inline final private[tries] def initStack[K, V, T](trie :Trie[K, V, T], depth :Int) :Array[Trie[K, V, T]] = {
		val res = new Array[Trie[K, V, T]](depth)
		res(0) = trie
		res
	}

	/** An iterator traversing a trie using a manual stack implemented using an array.
	  * @param top index of the top element on the stack. Negative index means an empty iterator.
	  * @param stack path to the current node splitting the trie in two, with the 'right-hand' side containing remaining elements.
	  *              The top of the stack - `stack(top)`, providing `this` is not empty - is a singleton leaf node in the trie, being
	  *              the `head` of this iterator. Below it, are all inner nodes of type [[BinaryTrie]] from which we descended to the left child.
	  *              Thus, all right children of all inner nodes on the path remain to be iterated.
	  *              Whenever we descend right from an inner node, we remove it from the stack (replacing it with its child),
	  *              as there will be V more to do with that node.
	  */
	abstract class BinaryTrieIterator[+E, +K, +V, +T <: Trie[K, V, T]](stack :Array[Trie[K, V, T]], private[this] var top :Int)
		extends BaseIterator[E]
	{ this :FitIterator[E] =>
		def this(trie: Trie[K, V, T], depth :Int) = {
			this(initStack(trie, depth), 0)
			descend()
		}

		override def hasDefiniteSize = true
		override def hasNext = top >= 0

		override def size = {
			@tailrec def count(i :Int, sum :Int) :Int =
				if (i<0) sum
				else count(i-1, sum + stack(i).asInstanceOf[BinaryTrie[K, V, T]].right.size)
			if (top<0) 0
			else count(top-1, topNode.size)
		}

		override def ofAtLeast(n :Int) = {
			@tailrec def count(i :Int, left :Int) :Boolean =
				left <= 0 || i>=0 && count(i-1, left - stack(i).asInstanceOf[BinaryTrie[K, V, T]].right.size)
			n<=0 || top >= 0 && count(top-1, n-topNode.size)
		}

		final def skip(): Unit = {
			stack(top) = null
			top -= 1
			if (top >= 0 && stack(top).plurality > 1) {
				stack(top) = stack(top).asInstanceOf[BinaryTrie[K, V, T]].right
				descend()
			}
		}

		@tailrec final protected def descend(): Unit = stack(top).plurality match {
			case n if n > 1 => //descend into the left tree
				stack(top + 1) = stack(top).asInstanceOf[BinaryTrie[K, V, T]].left
				top += 1
				descend()
			case 0 => //blind path, rewind and check the right subtree
				stack(top) = null
				top -= 1
				if (top >= 0) { //replace our parent with its right child, as we won't ever need to look at it again
					stack(top) = stack(top).asInstanceOf[BinaryTrie[K, V, T]].right
					descend()
				}

			case _ => //found 'leftmost' singleton - minimum in the tree.
		}

		@inline protected[this] final def topNode :Trie[K, V, T] = stack(top)

		override final def drop(n: Int) = {
			if (n > 0 && top >= 0) {
				var left = n - 1
				stack(top) = null
				top -= 1
				while (top >= 0 && left > 0) {
					//the outer loop retreats up the tree dropping right subtrees of size smaller than remaining elements to drop
					val nextTree = stack(top).asInstanceOf[BinaryTrie[K, V, T]].right
					val leafCount = nextTree.size
					if (leafCount <= left) {
						//covers singletons and empty sets
						left -= leafCount
						stack(top) = null
						top -= 1
					} else {
						//now the next right child up is larger than we need; go down left as long as the tree contains non-dropped elements
						stack(top) = nextTree//.asInstanceOf[Branch].left
						var trieSize = nextTree.size
						while (trieSize > left) {
							//left is still >=1, so stack(top) has to be an inner node to enter loop
							val next = stack(top).asInstanceOf[BinaryTrie[K, V, T]].left
							stack(top + 1) = next
							top += 1; trieSize = next.size
						} //now we are at a node in the tree which has to be dropped in its entirety, so we return to the main retreating loop
						left -= trieSize
						stack(top) = null
						top -= 1
					}
				} //stack(top) - if it exists - is a Branch
				if (top>=0) {
					stack(top) = stack(top).asInstanceOf[BinaryTrie[K, V, T]].right
					descend()
				}
			}
			this
		}


/*
		protected def foreachLeaf(f: T => Unit) :Unit =
			if (top>=0) {
				stack(top).forLeaves(f)
				stack(top) = null; top -= 1
				while(top >= 0) {
					stack(top).asInstanceOf[BinaryTrie[K, V, T]].right.forLeaves(f)
					stack(top) = null; top-=1
				}
			}

		protected def existsLeaf(p: T => Boolean) :Boolean =
			top <=0 || stack(top).existsLeaf(p) && {
				stack(top) = null; top -=1
				while(top>=0 && !stack(top).asInstanceOf[BinaryTrie[K, V, T]].right.existsLeaf(p)) {
					stack(top) = null; top -= 1
				}
				top >= 0
			}
*/



	}

	class LeafIterator[L<:Trie[K, V, T], +K, +V, +T<:Trie[K, V, T]](path :Array[Trie[K, V, T]], stackSize :Int)
		extends BinaryTrieIterator[L, K, V, T](path, stackSize) with FitIterator[L]
	{
		def this(trie :Trie[K, V, T], depth :Int) = {
			this(initStack(trie, depth), 0)
			descend()
		}

//		def values :FitIterator[V]

		override def head: L = topNode.asInstanceOf[L]

		override def next(): L = { val leaf = topNode.asInstanceOf[L]; skip(); leaf }
	}

	class LeafValueIterator[+K, @specialized(Elements) +V, +T<:Trie[K, V, T]](path :Array[Trie[K, V, T]], stackSize :Int)
		extends BinaryTrieIterator[V, K, V, T](path, stackSize) with FitIterator[V]
	{
		def this(trie :Trie[K, V, T], depth :Int) = {
			this(initStack(trie, depth), 0)
			descend()
		}

		override def head: V = topNode.value

		override def next(): V = { val hd = topNode.value; skip(); hd }
	}

	class LeafKeyIterator[@specialized(Int, Long) +K, +V, +T<:Trie[K, V, T]](path :Array[Trie[K, V, T]], stackSize :Int)
		extends BinaryTrieIterator[K, K, V, T](path :Array[Trie[K, V, T]], stackSize :Int) with FitIterator[K]
	{
		def this(trie :Trie[K, V, T], depth :Int) = {
			this(initStack(trie, depth), 0)
			descend()
		}

		override def head: K = topNode.key

		override def next(): K = { val k = topNode.key; skip(); k }
	}

}
