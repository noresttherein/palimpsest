package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.tries.Trie.{MutableTrieRoot, TrieCombinator, TriePatch}

/**
  * @author Marcin Mościcki
  */
trait TrieValues[K, V, E, T] extends TrieTemplate[K, V, E, T] with TrieCombinator[T] {
	private[palimpsest] def update(root :MutableTrieRoot[T], element :V, mutant :TriePatch[K, V, T]) :Unit


}
