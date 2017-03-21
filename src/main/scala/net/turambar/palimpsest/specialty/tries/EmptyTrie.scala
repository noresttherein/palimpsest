package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.iterables.{EmptyIterable, EmptyIterableTemplate}
import net.turambar.palimpsest.specialty.tries.Trie.{MutableTrieRoot, TrieCombinator, TrieOperator, TriePatch}

import scala.annotation.unspecialized
import scala.collection.GenTraversableOnce


trait EmptyTrieTemplate[+K, +V, +E, +T] extends TrieTemplate[K, V, E, T] with EmptyIterableTemplate[E, T] {
	/** Fixed to return [[Trie.EmptyNode]], equal to zero - denotes the magnitude of leaves (as in 'one, two, many') in this trie. */
	@inline final override def plurality :Trie.NodeType = 0

	override def empty = this.asInstanceOf[T]
	override def asTrie = this.asInstanceOf[T]

	override def key :K = throw new NoSuchElementException(s"$stringPrefix().key")
	override def value :V = throw new NoSuchElementException(s"$stringPrefix().value")

	override def keyOpt = None
	override def valueOpt = None

	override def trieHead: T = empty
	override def trieLast: T = empty

//	override protected[this] def hasLeaf(key :K) :Boolean = false
//	override protected[this] def leafFor(key :K) :T = empty
	override def leaf(idx :Int) :T = asTrie //throw new NoSuchElementException(s"$typeStringPrefix.leaf($idx)")

//	override protected[this] def belongs(key: K): Boolean = false


	override def forEachLeaf(f: T => Unit): Unit = ()
	//	@unspecialized override def forLeavesReversed(f: (T) => Unit): Unit = ()

	override def forEachKey(f: (K) => Unit): Unit = ()
	override def forEachKeysReversed(f: (K) => Unit): Unit = ()
	override def forEachValue(f: V => Unit): Unit = ()
	override def forEachValueReversed(f: V => Unit): Unit = ()

	override def existsLeaf(f: T => Boolean): Boolean = false
	override def forAllLeaves(f: T => Boolean): Boolean = true

	override def existsKey(f: (K) => Boolean): Boolean = false
	override def forAllKeys(f: (K) => Boolean): Boolean = true

	override def existsValue(f: (V) => Boolean): Boolean = false
	override def forAllValues(f :V => Boolean) :Boolean = true

	override def findLeaf(f: (T) => Boolean): T = empty

	override def findKey(f: K => Boolean): Option[K] = None
	override def findValue(f: (V) => Boolean): Option[V] = None

	override def filterLeaves(f: (T) => Boolean): T = asTrie

	override def leafSpan(f :T => Boolean) :(T, T) = (asTrie, asTrie)

	private[palimpsest] override def properPrefixOrNull(f: (T) => Boolean) = null.asInstanceOf[T]
	override def takeLeaves(f :T => Boolean) :T = asTrie
	override def dropLeaves(f :T => Boolean) :T = asTrie

	override def filterKeys(f: K => Boolean): T = asTrie
	override def filterKeysNot(f :K => Boolean) :T = asTrie
	override def filterValues(f: V => Boolean): T = asTrie
	override def filterValuesNot(f :V=>Boolean) :T = asTrie


	override protected[this] def unionTrie(other: T): T = other
	override protected[this] def diffTrie(other: T): T = empty
	override protected[this] def intersectTrie(other: T): T = empty
	override protected[this] def subtrieOf(other: T): Boolean = true


//	override protected[this] def without(key :K) :T = empty

//	override protected[this] def patch(key: K, mutant: TriePatch[K, V, T]): T = mutant.notFound(key, this)
//
//	override protected[this] def patch(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): T =
//		mutant.notFound(key, this) match {
//			case e if e == empty => e
//			case leaf => root.size_++(); leaf
//		}
//
//
//	override protected[this] def mutate(root: MutableTrieRoot[T], parent: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
//		mutant.notFound(key, this) match {
//			case e if e == empty => ()
//			case newLeaf => root.size_++(); parent.hang(newLeaf)
//		}


	override protected[this] def combine(other :T, combinator :TrieCombinator[T]) :T = combinator.emptyFirst(other)
	override protected[this] def combine[O](other: T, operator: TrieOperator[T, O]): O = operator.emptyFirst(other)


}


/** Base, minimal implementation of specialized empty trie implementations.
  * Doesn't attempt to implement actual collection operations itself, assuming derived classes will inherit their
  * specialized behaviour separately from either their root collection type or base traits such as [[EmptyIterableTemplate]].
  *
  * @tparam K type of the keys in this trie used during lookup, associated with leaves and defining their location in the trie.
  *           Most often an internal type, not exposed as part of the public interface (such as with hash codes).
  * @tparam T 'self type' of this trie, that is the `Repr` in `IterableLike`. This interface doesn't pose any bounds
  *           on this type or the relationship between 'this' and `T` to simplify implementation,
  *           but actual collections will generally assume that `this.type <: T`.
  */
trait EmptyTrie[@specialized(Int, Long) +K, +V, +T <: TrieTemplate[K, V, Any, T]] extends Trie[K, V, T] with EmptyTrieTemplate[K, V, Any, T] {
	/** Fixed to return [[Trie.EmptyNode]], equal to zero - denotes the magnitude of leaves (as in 'one, two, many') in this trie. */
//	@inline final override def plurality :Trie.NodeType = 0
//	override def empty = this.asInstanceOf[T]
//	override def asTrie = this.asInstanceOf[T]
//
//	@unspecialized
//	override def key :K = throw new NoSuchElementException(s"$stringPrefix().key")
//	override def value :V = throw new NoSuchElementException(s"$stringPrefix().value")
//
//	override def keyOpt = None
//	override def valueOpt = None
//
//	override def trieHead: T = empty
//	override def trieLast: T = empty
//
	override protected[this] def hasLeaf(key :K) :Boolean = false
	override protected[this] def leafFor(key :K) :T = empty
//	@unspecialized override def leaf(idx :Int) :T = asTrie //throw new NoSuchElementException(s"$typeStringPrefix.leaf($idx)")

	override protected[this] def belongs(key: K): Boolean = false


//	@unspecialized override def forEachLeaf(f: T => Unit): Unit = ()
//
//
//	@unspecialized override def forEachKey(f: (K) => Unit): Unit = ()
//	@unspecialized override def forEachKeysReversed(f: (K) => Unit): Unit = ()
//	override def forEachValue(f: V => Unit): Unit = ()
//	override def forEachValueReversed(f: V => Unit): Unit = ()
//
//	@unspecialized override def existsLeaf(f: T => Boolean): Boolean = false
//	@unspecialized override def forAllLeaves(f: T => Boolean): Boolean = true
//
//	@unspecialized override def existsKey(f: (K) => Boolean): Boolean = false
//	@unspecialized override def forAllKeys(f: (K) => Boolean): Boolean = true
//
//	override def existsValue(f: (V) => Boolean): Boolean = false
//	override def forAllValues(f :V => Boolean) :Boolean = true
//
//	@unspecialized override def findLeaf(f: (T) => Boolean): T = empty
//
//	@unspecialized override def findKey(f: K => Boolean): Option[K] = None
//	override def findValue(f: (V) => Boolean): Option[V] = None
//
//	@unspecialized override def filterLeaves(f: (T) => Boolean): T = asTrie
//
//	override def leafSpan(f :T => Boolean) :(T, T) = (asTrie, asTrie)
//
//	private[palimpsest] override def properPrefixOrNull(f: (T) => Boolean) = null.asInstanceOf[T]
//	override def takeLeaves(f :T => Boolean) :T = asTrie
//	override def dropLeaves(f :T => Boolean) :T = asTrie
//
//	@unspecialized override def filterKeys(f: K => Boolean): T = asTrie
//	@unspecialized override def filterKeysNot(f :K => Boolean) :T = asTrie
//	override def filterValues(f: V => Boolean): T = asTrie
//	override def filterValuesNot(f :V=>Boolean) :T = asTrie
//
//
//	@unspecialized override protected[this] def unionTrie(other: T): T = other
//	@unspecialized override protected[this] def diffTrie(other: T): T = empty
//	@unspecialized override protected[this] def intersectTrie(other: T): T = empty
//	@unspecialized override protected[this] def subtrieOf(other: T): Boolean = true
//

	override protected[this] def without(key :K) :T = empty

	override protected[this] def patch(key: K, mutant: TriePatch[K, V, T]): T = mutant.notFound(key, this)

	override protected[this] def patch(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): T =
		mutant.notFound(key, this) match {
//			case e if e == empty => e
			case e if e.isEmpty => e
			case leaf => root.size_++(); leaf
		}


	override protected[this] def mutate(root: MutableTrieRoot[T], parent: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
		mutant.notFound(key, this) match {
//			case e if e == empty => ()
			case e if e.isEmpty => ()
			case newLeaf => root.size_++(); parent.hang(newLeaf)
		}


//	override protected[this] def combine(other :T, combinator :TrieCombinator[T]) :T = combinator.emptyFirst(other)
//	override protected[this] def combine[O](other: T, operator: TrieOperator[T, O]): O = operator.emptyFirst(other)
}



object EmptyTrie {
	@inline final def unapply(elems :GenTraversableOnce[Any]) :Boolean = elems match {
		case e :Trie[_, _, _] => e.isEmpty
		case _ => false
	}

	object IsEmptyTrie {
		@inline final def unapply(trie :Trie[_, _, _]) :Boolean = trie.isInstanceOf[EmptyTrie[_, _, _]]
	}


	abstract class EmptyTrieFoundation[+K, +V, +E, +T <: TrieTemplate[K, V, E, T]]
		extends EmptyIterable[E, T] with EmptyTrieTemplate[K, V, E, T]

	abstract class AbstractEmptyTrie[@specialized(Int, Long) +K, +V, +E, +T <: TrieTemplate[K, V, E, T]] //extends EmptyIterable[E, T] with EmptyTrie[K, V, T] {
		extends EmptyTrieFoundation[K, V, E, T] with EmptyTrie[K, V, T]
	{
		override def empty :T = this.asInstanceOf[T]
		override def asTrie :T = this.asInstanceOf[T]


		override protected[this] def patch(key: K, mutant: TriePatch[K, V, T]) = mutant.notFound(key, this)

		override protected[this] def patch(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]) =
			mutant.notFound(key, this) match {
				case e if e.isEmpty => e
				case leaf => root.size_++(); leaf
			}

		override protected[this] def mutate(root: MutableTrieRoot[T], parent: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
			mutant.notFound(key, this) match {
				case e if e.isEmpty => ()
				case newLeaf => root.size_++(); parent.hang(newLeaf)
			}

		override protected[this] def mutate(root: MutableTrieRoot[T], key: K, mutant: TriePatch[K, V, T]): Unit =
			mutant.notFound(key, this) match {
				case e if e.isEmpty => ()
				case newLeaf => root.size_++(); root.hang(newLeaf)
			}

	}

}
