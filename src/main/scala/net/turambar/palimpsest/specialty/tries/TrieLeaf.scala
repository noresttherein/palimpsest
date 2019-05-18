package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.iterables.{SingletonFoundation, SingletonTemplate}
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.Fun1
import net.turambar.palimpsest.specialty.{?, Sure, Var}
import net.turambar.palimpsest.specialty.tries.Trie.KeyTypes

/** Root trait for leaf nodes implementing methods of [[TrieTemplate]] and [[Trie]] which don't need to be specialized
  * on key, value or element (of this collection) types. Concrete classes are expected to implement [[TrieLeaf]], too.
  */
trait TrieLeafTemplate[+K, +T <: TrieTemplate[K, T]] extends TrieTemplate[K, T] {
	/** Fixed to return [[Trie.LeafNode]], equal to `1` - denotes the magnitude of leaves (as in 'one, two, many') in this trie. */
	@inline final def plurality :Trie.NodeType = 1

	/** Wraps the key in an unspecialized [[Sure]] instance. Override in subclasses to avoid boxing. */
	override def key_? : ?[K] = Sure(key)

	override def keyOpt :Option[K] = Some(key)


	override def size :Int = 1
	override def ofAtLeast(elems :Int) :Boolean = elems <= 1
	override def nonEmpty :Boolean = true
	override def isEmpty :Boolean = false


	override def tail :T = emptyTrie
	override def init :T = emptyTrie

	override def headNode: T = asTrie
	override def lastNode: T = asTrie


	override def keyNode(idx :Var[Int]) :T =
		if (idx.get == 0) { idx -= 1; asTrie }
		else { idx -= 1; emptyTrie }


	override def dropTrie(count :Var[Int]) :T =
		if (count.get <= 0) asTrie
		else { count -= 1; emptyTrie }

	override def takeTrie(count :Var[Int]) :T =
		if (count >= 0) { count -= 1; asTrie }
		else emptyTrie

	override def dropRightTrie(count :Var[Int]) :T = //dropTrie(count)
		if (count.get <= 0) asTrie
		else { count -= 1; emptyTrie }

	override def takeRightTrie(count :Var[Int]) :T = //takeTrie(count)
		if (count.get >= 0) { count -= 1; asTrie }
		else emptyTrie

	override def sliceTrie(dropLeft :Var[Int], size :Var[Int]) :T =
		if (dropLeft.get <= 0) takeTrie(size)
		else { dropLeft -= 1; emptyTrie }


	override def splitTrie(idx :Var[Int]) :(T, T) =
		if (idx.get <= 0) (emptyTrie, asTrie)
		else (asTrie, emptyTrie)

	
	override def clone() :T = asTrie

}




/** Base, minimal implementation of single-element tries. Represents both a singleton collection of type `T`
  * and a leaf in a larger trie instance.
  * Doesn't attempt to implement actual collection operations itself, assuming derived classes will inherit their
  * appropriate behaviour separately from either their root collection type or base traits such as [[SingletonTemplate]].
  *
  * @tparam K type of the keys in this trie used during lookup, associated with leaves and defining their location in the trie.
  *           Most often an internal type, not exposed as part of the public interface (such as with hash codes).
  * @tparam T 'self type' of this trie, that is the `Repr` in `InterableLike`.
  */
trait TrieLeaf[@specialized(Int, Long) +K, +T <: Trie[K, T]] extends Trie[K, T] with TrieLeafTemplate[K, T] { this :T =>

	override def key_? : ?[K] = Sure(key)

//	override protected[this] def applyKey[@specialized(Boolean, Unit) O](f: K => O): O = f(key)

	override protected[this] def nodeFor(key :K) :T = if (this.key==key) this else emptyTrie
	override protected[this] def hasKey(key :K) :Boolean = this.key == key
	override protected[this] def belongs(key: K): Boolean = this.key == key


//	override protected[this] def foldPath[U >: T, @specialized(TrieOpRes) O](op: Trie.FoldPath[K, U, O])(key: K): O =
//		if (key == this.key) op.whenKeyExists(key, this)
//		else op.whenNoKey(key, this)


	override def toString :String = "{" + key + "}"
}






object TrieLeaf {


	abstract class TrieLeafFoundation[+K, +T <: Trie[K, T]] extends TrieLeafTemplate[K, T] { this :T => }

	abstract class AbstractTrieLeaf[@specialized(KeyTypes) +K, +T <: Trie[K, T]](k :K)
		extends TrieLeafFoundation[K, T] with TrieLeaf[K, T]
	{ this :T =>
		@inline final override def key :K = k
	}


//
//	/** An abstract base class for implementations representing trie leaves as singleton collections (for some element type).
//	  */
//	abstract class SingletonLeaf[@specialized(KeyTypes) +K, +E, +T <: Trie[K, T]]
//		extends SingletonFoundation[E, T] with TrieLeaf[K, T]
//	{ this :T =>
//		def this(key :K) = {this(); thisKey = key}
//
//		private[this] var thisKey :K = _
//
//		@inline final override def key :K = thisKey
//
//		@inline final protected[this] def key_=(key :K) :Unit = thisKey = key
//
//	}



}
