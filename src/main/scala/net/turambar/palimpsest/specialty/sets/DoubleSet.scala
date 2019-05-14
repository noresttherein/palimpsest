package net.turambar.palimpsest.specialty.sets

import java.lang.Double.{doubleToLongBits, doubleToRawLongBits, longBitsToDouble, NaN}

import net.turambar.palimpsest.specialty.tries.{IterableTriePotFoundation, LongTrie, LongTrieKeys, MutableLongTrie, TrieKeySetOps}
import net.turambar.palimpsest.specialty.tries.LongTrie.{EmptyLongTrie, LongTrieLeaf}
import net.turambar.palimpsest.specialty.tries.TrieElements.{ElementCounter, ElementOf}
import net.turambar.palimpsest.specialty.{?, Blank, FitTraversableOnce, RuntimeType, Sure, Var}
import net.turambar.palimpsest.specialty.sets.StableDoubleSet.{doubleToKey, DoubleElementCounter}
import net.turambar.palimpsest.specialty.sets.ValSet.ValSetBuilder
import net.turambar.palimpsest.specialty.tries.GenericBinaryTrie.BinaryTriePatch
import net.turambar.palimpsest.specialty.tries.MutableLongTrie.{EmptyMutableLongTrie, MutableLongTrieBranch, MutableLongTrieLeaf}

import scala.collection.GenTraversableOnce


/** Common implementation trait for mutable and immutable sets of long values backed by a [[net.turambar.palimpsest.specialty.tries.LongTrie]].
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed trait DoubleSetLike[T <: LongTrieKeys[LongTrie, T] with LongTrie, S <: ValSet[Double] with DoubleSetLike[T, S]]
	extends IterableTriePotFoundation[Long, LongTrie, T, Double, S] //for size tracking
		with SetSpecialization[Double, S]
		with TrieKeySetSpecialization[Long, LongTrie, T, Double, S] //main set implementation
		//	   with TraversableTrieKeySet[Long, MutableLongTrie, MutableLongTrie, Long, LongSet]  //traverse this set with foreach, not iterator
		with ElementOf[Double, LongTrie] //translate trie leaf (key) to the set element
		//	   with TrieKeyPatch[Long, MutableLongTrie, MutableLongTrie] //use ourselves as insert
{ //this :S =>


	
	override def specialization :RuntimeType[Double] = RuntimeType.OfDouble


//	@inline final override protected[this] def elements :ElementOf[Double, LongTrie] = this

	@inline final override protected[this] def countingElements :ElementCounter[Double, LongTrie] = new DoubleElementCounter

	@inline final override def elementOf(leaf :LongTrie) :Double = longBitsToDouble(leaf.key)

	@inline final override def satisfies(leaf :LongTrie, f :Double=>Boolean) :Boolean = f(longBitsToDouble(leaf.key))

	final override def satisfying(leaf :LongTrie, f :Double=>Boolean, where :Boolean): ?[Double] = {
		val key = longBitsToDouble(leaf.key)
		if (f(key) == where) Sure(key)
		else Blank
	}


	override protected def friendTrie(items :GenTraversableOnce[_]) :Option[LongTrie] = items match {
		case set :DoubleSetLike[_, _] => Some(set.trie)
		case _ => None
	}


	override protected def patchTrie(t :T, element :Double)(patch :BinaryTriePatch[Long, LongTrie, T]) :T =
		t.patchKey(patch)(doubleToKey(element))



	override def apply(elem :Double) :Boolean = trie.hasKey(doubleToKey(elem))

	override def contains(elem :Double) :Boolean = trie.hasKey(doubleToKey(elem))


	override def stringPrefix = "Set[Double]"
	override def typeStringPrefix = "DoubleSet"
}









/** An immutable set of `Long` values backed by a trie. */
final class StableDoubleSet private[sets] (keys :LongTrie, keyCount :Int = -1)
	extends IterableTriePotFoundation[Long, LongTrie, LongTrie, Double, StableDoubleSet](keys, keyCount)
		with StableSet[Double] with DoubleSetLike[LongTrie, StableDoubleSet]
		with StableTrieKeySetTemplate[Long, LongTrie, Double, StableDoubleSet]
{

	override protected def ops :TrieKeySetOps[Long, LongTrie, LongTrie] = LongTrie

	override protected[this] def newSet(trie :LongTrie, size :Int) :StableDoubleSet = new StableDoubleSet(trie.stable, size)

	override protected[this] def plant(trie :LongTrie, size :Int) :StableDoubleSet = new StableDoubleSet(trie, size)

	override def empty :StableDoubleSet = StableDoubleSet.Empty

	override def stable :StableDoubleSet = this

	override def mutable :MutableDoubleSet = new MutableDoubleSet(MutableLongTrie.newRoot(trie), unsureSize)

	override def newBuilder = new ValSetBuilder[Double, MutableDoubleSet, StableDoubleSet](MutableDoubleSet.empty, _.stable)
}



object StableDoubleSet {

	private[sets] final val NaNKey = 0x7ff8000000000000L
	private[sets] final val ZeroKey = doubleToRawLongBits(0.0)
	
	private[sets] def doubleToKey(value :Double) :Long =
		if (value == 0.0) ZeroKey
		else if (value != value) NaNKey
		else doubleToRawLongBits(value)
	
	private[sets] def keyToDouble(key :Long) :Double = longBitsToDouble(key)
		
	
	final val Empty :StableDoubleSet = new StableDoubleSet(EmptyLongTrie, 0)



	class DoubleElementCounter extends ElementOf[Double, LongTrie] with ElementCounter[Double, LongTrie] {
		private[this] var invocations = 0

		override def elementOf(keyNode :LongTrie) :Double = { invocations += 1; longBitsToDouble(keyNode.key) }

		override def satisfies(keyNode :LongTrie, f :Double => Boolean) :Boolean = {
			invocations += 1
			f(longBitsToDouble(keyNode.key))
		}

		override def satisfying(keyNode :LongTrie, f :Double => Boolean, where :Boolean) : ?[Double] = {
			invocations += 1
			val key = keyNode.key
			if (f(longBitsToDouble(keyNode.key))) Sure(key)
			else Blank
		}

		override def count :Int = invocations
	}
}






/** A mutable set of `Long` values backed by a trie. */
class MutableDoubleSet private[sets] (keys :MutableLongTrie, keyCount :Int = -1)
	extends IterableTriePotFoundation[Long, LongTrie, MutableLongTrie, Double, MutableDoubleSet](keys, keyCount)
		with MutableSet[Double] with MutableSetSpecialization[Double, MutableDoubleSet]
		with MutableTrieKeySetSpecialization[Long, LongTrie, MutableLongTrie, Double, MutableDoubleSet] //override default mutable methods
		//	   with MutableLongTrieSet[Long, MutableLongSet] //get mutable trie patches and operations for MutableLongTrie
		with DoubleSetLike[MutableLongTrie, MutableDoubleSet]
{

	override protected def ops :TrieKeySetOps[Long, LongTrie, MutableLongTrie] = MutableLongTrie

	override protected def newRoot(other :LongTrie) :MutableLongTrie = MutableLongTrie.newRoot(other)


	override protected[this] def plant(trie :MutableLongTrie, size :Int) :MutableDoubleSet = new MutableDoubleSet(trie, size)

	override protected def patchTrie(elem :Double)(patch :BinaryTriePatch[Long, LongTrie, MutableLongTrie]) :Boolean =
		trie.patchKey(patch, this)(doubleToKey(elem))

	override protected def patchTrie(elems :FitTraversableOnce[Double])(patch :BinaryTriePatch[Long, LongTrie, MutableLongTrie]) :Unit =
		elems foreach { elem => trie.patchKey(patch, this)(doubleToKey(elem)) }


	override def empty :MutableDoubleSet = new MutableDoubleSet(trie.emptyTrie, 0)

	override def stable :StableDoubleSet = new StableDoubleSet(trie.stable, unsureSize)

	override def mutable :MutableDoubleSet = new MutableDoubleSet(trie.copy, unsureSize)

	override def origin :AnyRef = MutableDoubleSet
}



object MutableDoubleSet {
	@inline def empty :MutableDoubleSet = new MutableDoubleSet(EmptyMutableLongTrie, 0)

	@inline def apply(single :Double) :MutableDoubleSet = new MutableDoubleSet(new LongTrieLeaf(doubleToKey(single)), 1)

}
