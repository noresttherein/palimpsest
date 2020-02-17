package net.noresttherein.palimpsest.tries

import net.noresttherein.palimpsest.{?, Blank}



/** Base class for trie nodes containing exactly two (non-empty, unless explicitly allowed) children of the given type.
  * @tparam K type of keys in this trie
  * @tparam T self type of this trie
  * @tparam C type of the children of this branch (which may differ from `T` for adapter branches).
  */
trait TrieBranchTemplate[+K, +C <: TrieTemplate[K, C], +T <: TrieTemplate[K, T]] extends TrieTemplate[K, T] {
	/** Left subtrie, first in the traversing order. */
	def left :C
	/** Right subtrie, second in the traversing order. */
	def right :C

	/** Throws `UnsupportedOperationException` as by default no keys are stored in branches. */
	override def key :K = throw new UnsupportedOperationException("TrieBranch.key")
	/** Returns [[net.noresttherein.palimpsest.Blank]], as no keys are stored in branches by default. */
	override def key_? : ?[K] = Blank
	/** Returns `None`, as no keys are stored in branches by default. */
	override def keyOpt :Option[K] = None

	/** Fixed to return [[Trie.BranchNode]], equal to `2` - denotes the magnitude of leaves (as in 'one, two, many') in this trie. */
	override def plurality :Trie.NodeType = 2

	override def toString :String = "{" + left + ", " + right + "}"
}

