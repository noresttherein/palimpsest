package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.{?, Blank, ItemTypes, Sure}
import net.turambar.palimpsest.specialty.tries.TrieElements.ElementOf

/** A trie in which every key is associated with a value of type `V`.
  * @tparam K type of keys organizing this trie
  * @tparam V type of values associated with the keys
  * @tparam T self type of this trie
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait ValueTrie[+K, @specialized(ItemTypes) +V, +T <: ValueTrie[K, V, T]] extends TrieTemplate[K, T] {
	def value :V
	def value_? : ?[V]
	def valueOpt :Option[V]
}




object ValueTrie {



	trait EmptyValueTrie[+K, +V, +T <: ValueTrie[K, V, T]] extends ValueTrie[K, V, T] with EmptyTrieTemplate[K, T] { this :T =>
		override def value :V = throw new NoSuchElementException("EmptyValueTrie.value")
		override def value_? : ?[V] = Blank
		override def valueOpt :Option[V] = None
	}

	trait ValueTrieLeaf[+K, @specialized(ItemTypes) +V, +T <: ValueTrie[K, V, T]] extends ValueTrie[K, V, T] with TrieLeafTemplate[K, T] {
		override def value_? : ?[V] = Sure(value)
		override def valueOpt :Option[V] = Some(value)
	}


	trait ValueTrieBranch[+K, @specialized(ItemTypes) +V, +T <: ValueTrie[K, V, T]] extends TrieBranch[K, T] with ValueTrie[K, V, T] {
		override def value :V = throw new UnsupportedOperationException("ValueTrieBranch.value")
		override def value_? : ?[V] = Blank
		override def valueOpt :Option[V] = None
	}


	trait TrieValues[@specialized(TrieElements.Types) E, T <: ValueTrie[_, E, T]] extends ElementOf[E, T] {
		override def elementOf(keyNode :T) :E = keyNode.value

		override def satisfies(keyNode :T, f :E => Boolean) :Boolean = f(keyNode.value)

		override def satisfying(keyNode :T, f :E => Boolean, where :Boolean) : ?[E] = {
			val v = keyNode.value
			if (f(v) == where) Sure(v) else Blank
		}
	}




}




