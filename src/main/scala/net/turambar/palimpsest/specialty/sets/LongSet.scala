package net.turambar.palimpsest.specialty.sets

import net.turambar.palimpsest.specialty.{?, Blank, AptBuilder, Vals, RuntimeType, Sure, Var}
import net.turambar.palimpsest.specialty.iterators.AptIterator
import net.turambar.palimpsest.specialty.ordered.ValOrdering
import net.turambar.palimpsest.specialty.sets.StableLongSet.{flipSign, LongElementCounter}
import net.turambar.palimpsest.specialty.sets.ValSet.ValSetBuilder
import net.turambar.palimpsest.specialty.tries.{TriePotIterableFoundation, LongTrie, LongTrieKeys, MutableLongTrie, TrieKeySetOps}
import net.turambar.palimpsest.specialty.tries.BinaryTrie.BinaryTriePatch
import net.turambar.palimpsest.specialty.tries.LongTrie.{EmptyLongTrie, LongTrieLeaf}
import net.turambar.palimpsest.specialty.tries.LongTrieKeys.LongKeyBranch
import net.turambar.palimpsest.specialty.tries.MutableLongTrie.{EmptyMutableLongTrie, MutableLongTrieBranch, MutableLongTrieLeaf}
import net.turambar.palimpsest.specialty.tries.TrieElements.{ElementCounter, ElementOf}

import scala.collection.GenTraversableOnce


/*
/** Mix-in trait for all sets which element types have homomorphic mappings with the `Long` type and use `MutableLongTrie`
  * as the backing data structure. Provides implementations of trie patches and binary operators as defined by [[MutableLongTrie]].
  */
trait LongTrieSet[E, +S <: ValSet[E] with SetSpecialization[E, S]]
	extends TrieKeySetSpecialization[Long, MutableLongTrie, MutableLongTrie, E, S]
{ this :S =>

//	override protected def keyDelete :TrieKeyPatch[Long, MutableLongTrie, MutableLongTrie] = MutableLongTrie.DeleteKey
//	override protected def keyInsert :TrieKeyPatch[Long, MutableLongTrie, MutableLongTrie] = MutableLongTrie.InsertKey
//	override protected def keyFlip :TrieKeyPatch[Long, MutableLongTrie, MutableLongTrie] = MutableLongTrie.FlipKey
//	override protected def intersection :SharingTrieOp[MutableLongTrie, MutableLongTrie] = MutableLongTrie.Intersection

}


/** Mix-in trait for mutable sets which element types have homomorphic mappings with the `Long` type and use `MutableLongTrie`
  * as the backing data structure. Provides implementations of trie patches and binary operators as defined by [[MutableLongTrie]]
  */
trait MutableLongTrieSet[E, +S <: MutableSet[E] with MutableSetSpecialization[E, S]]
	extends MutableTrieKeySetSpecialization[Long, MutableLongTrie, MutableLongTrie, E, S]
{ //this :S =>
//	override protected def keyDelete :TrieKeyPatch[Long, MutableLongTrie, MutableLongTrie] = MutableLongTrie.DeleteKey
//	override protected def keyInsert :TrieKeyPatch[Long, MutableLongTrie, MutableLongTrie] = MutableLongTrie.InsertKey
//	override protected def keyFlip :TrieKeyPatch[Long, MutableLongTrie, MutableLongTrie] = MutableLongTrie.FlipKey
//	override protected def intersection :SharingTrieOp[MutableLongTrie, MutableLongTrie] = MutableLongTrie.Intersection
}
*/



/** Common implementation trait for mutable and immutable sets of long values backed by a [[LongTrie]].
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed trait LongSetLike[T <: LongTrieKeys[LongTrie, T] with LongTrie, S <: OrderedSet[Long] with LongSetLike[T, S]]
	extends TriePotIterableFoundation[Long, LongTrie, T, Long, S] //for size tracking
       with SetSpecialization[Long, S] with OrderedSetTemplate[Long, S]
       with TrieKeySetSpecialization[Long, LongTrie, T, Long, S] //main set implementation
//	   with TraversableTrieKeySet[Long, MutableLongTrie, MutableLongTrie, Long, LongSet]  //traverse this set with foreach, not iterator
       with ElementOf[Long, LongTrie] //translate trie leaf (key) to the set element
//	   with TrieKeyPatch[Long, MutableLongTrie, MutableLongTrie] //use ourselves as insert
{ //this :S =>

	override implicit def ordering :ValOrdering[Long] = ValOrdering.LongOrdering

	override def specialization :RuntimeType[Long] = RuntimeType.OfLong


	@inline final override protected[this] def elements :ElementOf[Long, LongTrie] = this

	@inline final override protected[this] def countingElements :ElementCounter[Long, LongTrie] = new LongElementCounter

	@inline final override def elementOf(leaf :LongTrie) :Long = leaf.key ^ 0x8000000000000000L

	@inline final override def satisfies(leaf :LongTrie, f :Long=>Boolean) :Boolean = f(leaf.key ^ 0x8000000000000000L)

	final override def satisfying(leaf :LongTrie, f :Long=>Boolean, where :Boolean): ?[Long] = {
		val key = leaf.key ^ 0x8000000000000000L
		if (f(key) == where) Sure(key)
		else Blank
	}


	override protected def friendTrie(items :GenTraversableOnce[_]) :Option[LongTrie] = items match {
		case set :LongSetLike[_, _] => Some(set.trie)
		case _ => None
	}


	override protected def patchTrie(t :T, element :Long)(patch :BinaryTriePatch[Long, LongTrie, T]) :T =
		t.patchKey(patch)(element ^ 0x8000000000000000L)




	override def apply(elem :Long) :Boolean = trie.hasKey(flipSign(elem))

	override def contains(elem :Long) :Boolean = trie.hasKey(flipSign(elem))




	override def keyAt(n :Int) :Long = elementOf(trie.viewNode(Var(n)))

	override def from(key :Long) :S = plant(trie.fromKey(flipSign(key)), -1)

	override def until(key :Long) :S = plant(trie.untilKey(flipSign(key)), -1)

	override def to(key :Long) :S = plant(trie.toKey(flipSign(key)), -1)

	override def range(from :Long, until :Long) :S = plant(trie.keyRange(flipSign(from), flipSign(until)))

	override def rangeImpl(from: ?[Long], until: ?[Long]) :S =
		if (from.isDefined)
			plant (
                    if (until.isDefined) trie.keyRange(flipSign(from.get), flipSign(until.get))
					else trie.fromKey(flipSign(from.get))
				, -1)
		else
			if (until.isDefined) plant(trie.untilKey(flipSign(until.get)), -1)
			else (this  :SetSpecialization[Long, S]).clone()

	override def keysIteratorFrom(start :Long) :AptIterator[Long] = trie.iteratorFrom(this)(flipSign(start))




	override def stringPrefix = "Set[Long]"

	override def debugString :String =
		if (hasFastSize) debugPrefix + "<" + size + ">"
		else debugPrefix
}









/** An immutable set of `Long` values backed by a trie. */
final class StableLongSet private[sets] (keys :LongTrie, keyCount :Int = -1)
	extends TriePotIterableFoundation[Long, LongTrie, LongTrie, Long, StableLongSet](keys, keyCount)
	   with StableOrderedSet[Long] with LongSetLike[LongTrie, StableLongSet]
	   with StableTrieKeySetTemplate[Long, LongTrie, Long, StableLongSet]
{

	override protected def ops :TrieKeySetOps[Long, LongTrie, LongTrie] = LongTrie

//	override protected[this] def newRoot(trie :LongTrie) :LongTrie = trie.stable

	override protected[this] def newSet(trie :LongTrie, size :Int) :StableLongSet = new StableLongSet(trie.stable, size)

	override protected[this] def plant(trie :LongTrie, size :Int) :StableLongSet = new StableLongSet(trie, size)

	override def empty :StableLongSet = StableLongSet.Empty

//	override def stable :StableLongSet = this

	override def mutable :MutableLongSet = new MutableLongSet(MutableLongTrie.newRoot(trie), unsureSize)

	override def newBuilder = new ValSetBuilder[Long, MutableLongSet, StableLongSet](MutableLongSet.empty, _.stable)

	protected[this] override def debugPrefix = "StableLongSet"

}





object StableLongSet {

	final val Empty :StableLongSet = new StableLongSet(EmptyLongTrie, 0)


	/** Flips the highest bit of the given value. Note that with two's complement encoding this *does not* produce `-key`. */
	@inline final private[palimpsest] def flipSign(key :Long) :Long = key ^ 0x8000000000000000L


	class LongElementCounter extends ElementOf[Long, LongTrie] with ElementCounter[Long, LongTrie] {
		private[this] var invocations = 0

		override def elementOf(keyNode :LongTrie) :Long = { invocations += 1; keyNode.key ^ 0x8000000000000000L }

		override def satisfies(keyNode :LongTrie, f :Long => Boolean) :Boolean = {
			invocations += 1
			f(keyNode.key ^ 0x8000000000000000L)
		}

		override def satisfying(keyNode :LongTrie, f :Long => Boolean, where :Boolean) : ?[Long] = {
			invocations += 1
			val key = keyNode.key
			if (f(key ^ 0x8000000000000000L)) Sure(key)
			else Blank
		}

		override def count :Int = invocations
	}
}






/** A mutable set of `Long` values backed by a trie. */
class MutableLongSet private[sets] (keys :MutableLongTrie, keyCount :Int = -1)
	extends TriePotIterableFoundation[Long, LongTrie, MutableLongTrie, Long, MutableLongSet](keys, keyCount)
	   with MutableOrderedSet[Long] with MutableSetSpecialization[Long, MutableLongSet]
	   with MutableTrieKeySetSpecialization[Long, LongTrie, MutableLongTrie, Long, MutableLongSet] //override default mutable methods
//	   with MutableLongTrieSet[Long, MutableLongSet] //get mutable trie patches and operations for MutableLongTrie
	   with LongSetLike[MutableLongTrie, MutableLongSet]
{

	override protected def ops :TrieKeySetOps[Long, LongTrie, MutableLongTrie] = MutableLongTrie

	override protected def newRoot(other :LongTrie) :MutableLongTrie = other match {
		case mutable :MutableLongTrie => mutable
		case branch :LongKeyBranch[LongTrie] => new MutableLongTrieBranch(branch.center, branch.left, branch.right)
		case _ if other.isEmpty => EmptyMutableLongTrie
		case _ => new MutableLongTrieLeaf(other.key)
	}


	override protected[this] def plant(trie :MutableLongTrie, size :Int) :MutableLongSet = new MutableLongSet(trie, size)

	override protected def patchTrie(elem :Long)(patch :BinaryTriePatch[Long, LongTrie, MutableLongTrie]) :Boolean =
		trie.patchKey(patch, this)(elem ^ 0x8000000000000000L)

	override protected def patchTrie(elems :Vals[Long])(patch :BinaryTriePatch[Long, LongTrie, MutableLongTrie]) :Unit =
		elems foreach { elem => trie.patchKey(patch, this)(elem ^ 0x8000000000000000L) }


	override def empty :MutableLongSet = new MutableLongSet(trie.emptyTrie, 0)

	override def stable :StableLongSet = new StableLongSet(trie.stable, unsureSize)

//	override def mutable :MutableLongSet = new MutableLongSet(trie.copy, unsureSize)

	override def origin :AnyRef = MutableLongSet
//	override def newBuilder :AptBuilder[Long, MutableLongSet] = empty

	protected[this] override def debugPrefix = "MutableLongSet"

}





object MutableLongSet {
	@inline def empty :MutableLongSet = new MutableLongSet(EmptyMutableLongTrie, 0)

	@inline def apply(single :Long) :MutableLongSet = new MutableLongSet(new LongTrieLeaf(flipSign(single)), 1)

}

