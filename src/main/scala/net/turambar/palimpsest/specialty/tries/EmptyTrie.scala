package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.slang.Nullable
import net.turambar.palimpsest.specialty.{?, Blank, ItemTypes, Var}
import net.turambar.palimpsest.specialty.iterables.{EmptyIterableFoundation, EmptyIterableTemplate}
import net.turambar.palimpsest.specialty.tries.Trie.TrieOpRes
import net.turambar.palimpsest.specialty.RuntimeType.Specialized.{Fun1, Fun2}

import scala.annotation.unspecialized
import scala.collection.GenTraversableOnce


trait EmptyTrieTemplate[+K, +T <: TrieTemplate[K, T]]
	extends TrieTemplate[K, T]
{ this :T =>
	/** Fixed to return [[Trie.EmptyNode]], equal to zero - denotes the magnitude of leaves (as in 'one, two, many') in this trie. */
	@inline final override def plurality :Trie.NodeType = 0

	@inline def emptyTrie :T = this


	override def key :K = throw new NoSuchElementException(s"$EmptyTrie().key")

	override def key_? : ?[K] = Blank

	override def keyOpt :Option[K] = None

	final override def headNode: T = this
	final override def lastNode: T = this

	final override def keyNode(idx :Var[Int]) :T = this

	override def dropTrie(count :Var[Int]) :T = this
	override def takeTrie(count :Var[Int]) :T = this
	override def dropRightTrie(count :Var[Int]) :T = this
	override def takeRightTrie(count :Var[Int]) :T = this
	override def sliceTrie(dropLeft :Var[Int], size :Var[Int]) :T = this
	override def splitTrie(idx :Var[Int]) :(T, T) = (this, this)


/*
	override def find_?[@specialized(Fun1) E](elements :ElementOf[E, T])(f :E => Boolean, where :Boolean): ?[E] = Blank
	override def exists[@specialized(Fun1) E](elements :ElementOf[E, T])(f :E => Boolean) :Boolean = false
	override def forall[@specialized(Fun1) E](elements :ElementOf[E, T])(f :E => Boolean) :Boolean = true
	override def count[@specialized(Fun1) E](elements :ElementOf[E, T])(f :E => Boolean) :Int = 0
	override def partition[@specialized(Fun1) E](elements :ElementOf[E, T])(f :E => Boolean) :(T, T) = (this, this)
	override def filter[@specialized(Fun1) E](elements :ElementOf[E, T])(f :E => Boolean, where :Boolean) :T = this
	override def foreach[@specialized(Fun1) E, @specialized(Unit) O](elements :ElementOf[E, T])(f :E => O) :Unit = ()
	override def reverseForeach[@specialized(Fun1) E](elements :ElementOf[E, T])(f :E => Unit) :Unit = ()

	override def dropWhile[@specialized(Fun1) E](elements :ElementOf[E, T])(f :E => Boolean) :T = this
	override def takeWhile[@specialized(Fun1) E](elements :ElementOf[E, T])(f :E => Boolean) :T = this
	override def span[@specialized(Fun1) E](elements :ElementOf[E, T])(f :E => Boolean) :(T, T) = (this, this)


	override def reduce[@specialized(Fun2) E](elements :ElementOf[E, T])(f :(E, E) => E) :E =
		throw new UnsupportedOperationException("Empty.reduce")

	override def foldLeft[@specialized(Fun2) E, @specialized(Fun2) O](elements :ElementOf[E, T])(acc :O)(f :(O, E) => O) :O = acc
	override def foldRight[@specialized(Fun2) E, @specialized(Fun2) O](elements :ElementOf[E, T])(acc :O)(f :(E, O) => O) :O = acc
	override def copyToArray[@specialized(Elements) E](elements :ElementOf[E, T])(array :Array[E], from :Int, max :Int) :Int = 0

	override def iterator[@specialized(Elements) E](elements :ElementOf[E, T]) :FitIterator[E] = FitIterator.empty[E]
*/


	override def clone() :T = this

	override def toString = "{}"
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
trait EmptyTrie[@specialized(Int, Long) +K, +T <: TrieTemplate[K, T]]
	extends Trie[K, T] with EmptyTrieTemplate[K, T]
{ this :T =>


	override def size :Int = 0
	override def ofAtLeast(elems :Int) :Boolean = elems >= 0
	override def nonEmpty :Boolean = false
	override def isEmpty :Boolean = true

	override def tail = throw new UnsupportedOperationException("EmptyTrie.tail")
	override def init = throw new UnsupportedOperationException("EmptyTrie.init")

	override protected[this] def hasKey(key :K) :Boolean = false
	override protected[this] def nodeFor(key :K) :T = this
	override protected[this] def belongs(key :K) :Boolean = false



//	override protected[this] def foldPath[U >: T, @specialized(TrieOpRes) O](op: Trie.FoldPath[K, U, O])(key: K): O =
//		op.whenTrieEmpty(key, this)

}



object EmptyTrie {

	abstract class EmptyTrieFoundation[+K, +T <: TrieTemplate[K, T]] extends EmptyTrieTemplate[K, T] { this :T => }

}
