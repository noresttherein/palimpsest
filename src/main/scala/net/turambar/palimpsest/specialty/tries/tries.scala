package net.turambar.palimpsest.specialty


package object tries {

	/** A type alias for the most basic configuration of [[net.turambar.palimpsest.specialty.tries.GenericBinaryTrie]],
	  * without any distinction between stable and mutable versions. As `GenericBinaryTrie` is covariant regarding
	  * its self types, this alias is suitable both for implementations which do not have separate mutable and immutable
	  * trie versions and as a common base type for those which do (when parameterized with the stable type).
	  * @tparam K key type associated with this trie.
	  * @tparam T self type of the trie; all subtries of this type are of type `T` and any subtype of `BinaryTrie[K, T]`
	  *           must be also a subtype of `T`
	  */
	type BinaryTrie[@specialized(Int, Long) K, +T <: BinaryTrie[K, T]] = GenericBinaryTrie[K, T, T]

	val BinaryTrie = GenericBinaryTrie

	/** A type alias for the simplest version of [[net.turambar.palimpsest.specialty.tries.TrieBranchTemplate]], with
	  * its children being of the same type as this instance.
	  * @tparam K key type associated with this trie.
	  * @tparam T self type of this trie and the type of its branches. Any subtype of `TrieBrnch[K, T]` must be also
	  *           a subtype of `T`.
	  */
	type TrieBranch[K, +T <: TrieTemplate[K, T]] = TrieBranchTemplate[K, T, T]

}


