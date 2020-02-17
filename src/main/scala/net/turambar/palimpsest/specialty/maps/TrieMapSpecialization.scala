package net.turambar.palimpsest.specialty.maps

import net.turambar.palimpsest.specialty.sets.{SetSpecialization, ValSet}
import net.turambar.palimpsest.specialty.tries.TrieElements.{ElementCounter, ElementOf}
import net.turambar.palimpsest.specialty.tries.{BinaryTrie, GenericBinaryTrie, TrieElements, TrieFriends, TriePotIterableFoundation, TriePotIterableSpecialization, ValueTrie}
import net.turambar.palimpsest.specialty.tries.GenericBinaryTrie.BinaryTriePatch
import net.turambar.palimpsest.specialty.{?, Vals}
import net.turambar.palimpsest.specialty.tries.BinaryTrieKeySetFactory.SharingTrieOp

import scala.annotation.unspecialized
import scala.collection.{GenTraversableOnce, LinearSeq}


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
trait TrieMapSpecialization[K, TK, +V, F <: BinaryTrie[TK, F] with ValueTrie[TK, V, F],
                            T <: GenericBinaryTrie[TK, F, T] with TrieFriends[TK, F, F] with TrieElements[TK, F, T] with F,
                            +M <: AptMap[K, V] with MapKeySpecialization[K, V, M]]
	extends TriePotIterableFoundation[TK, F, T, (K, V), M] with MapKeySpecialization[K, V, M]
	   with TriePotIterableSpecialization[TK, F, T, (K, V), M] with ElementOf[(K, V), F]
{
	//stuff which is exactly the same as for sets and a candidate for extraction.

	protected[this] override def elements :ElementOf[(K, V), F] = this

	protected[this] override def countingElements :ElementCounter[(K, V), F]

//	protected[this] def trieKey(mapKey :K) :TK
//
//	protected[this] def mapKey()


	/** Check if the given collection is of a compatible type as this instance and, if so, retrieve its backing trie. */
	protected[this] def friendTrie(elems :GenTraversableOnce[_]) :Option[F]


	protected[this] def patchTrie(t :T, mapKey :K)(patch :BinaryTriePatch[TK, F, T]) :T

	protected[this] def patchTrie(t :T, elems :Vals[(K, V)])(patch :((K, V)) => BinaryTriePatch[TK, F, T]) :T = {
		var res = t; val it = elems.toIterator
		while (it.hasNext) {
			val kv = it.next()
			res = patchTrie(res, kv._1)(patch(kv))
		}
		res
	}

	protected[this] def patchTrie(t :T, elems :GenTraversableOnce[(K, V)])(patch :BinaryTriePatch[TK, F, T]) :T = {
		var res = t
		elems match {
			case list :LinearSeq[(K, V)] =>
				var l = list
				while (l.nonEmpty) {
//					res = patchTrie(res, l.head)(patch); l = l.tail
				}
			case it :Iterator[(K, V)] =>
//				while (it.hasNext)
//					res = patchTrie(res, it.next)(patch)
			case _ =>
//				elems foreach { e => res = patchTrie(res, e)(patch) }
		}
		res
	}




	protected[this] def juxtaposition(other :F)(op :SharingTrieOp[F, F]) :M



	protected[this] def newMap(trie :F, size :Int = -1) :M //= plant(trie.stable, size)


	@unspecialized
	override def empty :M = plant(trie.emptyTrie, 0)


//	override def head :(K, V) = elementOf(trie.viewHead)
//
//	override def last :(K, V) = elementOf(trie.viewLast)

	protected[this] def nodeFor(key :K) :T


	override def ?(key :K) : ?[V] = nodeFor(key).value_?

	override def +[U >: V](kv :(K, U)) :collection.Map[K, U] = ???

	override def -(key :K) :M = {
		val t = patchTrie(trie, key)(???)
		if (t eq trie) carbon
		else replant(t, -1)
	}
}