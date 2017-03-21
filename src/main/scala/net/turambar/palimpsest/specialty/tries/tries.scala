package net.turambar.palimpsest.specialty


package object tries {
	type BinaryTrie[+K, +V, +T<:TrieTemplate[K, V, Any, T]] = BinaryTrieFoundation[K, V, Any, T, T]

	final val BinaryTrie = BinaryTrieFoundation


}