package net.turambar.palimpsest.specialty.sets


import net.turambar.palimpsest.specialty.FitCompanion.CanFitFrom
import net.turambar.palimpsest.specialty.tries.Trie.{MutableTrieRoot, TrieCombinator, TriePatch}
import net.turambar.palimpsest.specialty.iterables.{SingletonFoundation, SingletonSpecialization}
import net.turambar.palimpsest.specialty.tries.{BinaryTrie, Trie}
import net.turambar.palimpsest.specialty.{Elements, FitBuilder, FitTraversableOnce, ImplementationIterableFactory, Specialized}

import scala.annotation.unspecialized
import scala.collection.{GenIterable, GenSet}
import scala.collection.generic.CanBuildFrom








/**
  * @author Marcin Mościcki
  */
trait TrieHashSet[@specialized(Elements) E]
	extends StableSet[E] with SpecializableSet[E, TrieHashSet] with Trie[Int, E, TrieHashSet[E]]
{
	override def companion = TrieHashSet

	override def contains(elem: E): Boolean = leafFor(elem.hashCode).contains(elem)
/*

	override def +(elem: E): TrieHashSet[E] = {
		val hash = elem.hashCode
		patch(hash, new HashLeaf(hash, elem))
	}

	override def -(elem: E): TrieHashSet[E] = {
		val hash = elem.hashCode
		patch(hash, new Remover(hash, elem))
	}
*/


	override def mutable :MutableSet[E] = MutableSet.from(this)

	override def typeStringPrefix = "HashTrie"
}

object TrieHashSet extends ImplementationIterableFactory[TrieHashSet] {


	@inline final override def empty[@specialized(Elements) E] :TrieHashSet[E] = ???

	@inline final override implicit def canBuildFrom[E](implicit fit: CanFitFrom[TrieHashSet[_], E, TrieHashSet[E]]): CanBuildFrom[TrieHashSet[_], E, TrieHashSet[E]] =
		fit.cbf

	override def newBuilder[@specialized(Elements) E]: FitBuilder[E, TrieHashSet[E]] = ???

	override def specializedBuilder[@specialized(Elements) E: Specialized]: FitBuilder[E, TrieHashSet[E]] = ???
}
