package net.noresttherein.palimpsest.tries

import net.noresttherein.palimpsest.tries.BinaryTrie.{BinaryTrieBranch, BinaryTrieNode, MutableBinaryTrieLeaf, MutableEmptyBinaryTrie}
import net.noresttherein.palimpsest.tries.GenericBinaryTrie.{BranchPatch, StableBinaryTrieBranch}
import net.noresttherein.palimpsest.tries.LongTrie.{EmptyLongTrie, LongTrieBranch}
import net.noresttherein.palimpsest.tries.LongTrieKeys.{centerLabel, EmptyLongKeys, GenericLongKeyBranch, LongKeyBranch, LongKeyLeaf, MutableLongKeyBranch}
import net.noresttherein.palimpsest.tries.TrieFriends.TrieOp
import net.noresttherein.palimpsest.tries.BinaryTrieKeySetFactory.{SharingTrieOp, TrackingTrieKeyPatch, TrieKeyPatch}
import net.noresttherein.palimpsest.tries.MutableLongTrie.EmptyMutableLongTrie



/** A common concrete type for tries with `Long` keys. Derived types share core implementation, making direct comparisons
  * and interoperability possible. In particular, a `LongTrie` instance may internally consist of nodes of various
  * subtypes of `LongTrieKeys`, representing fragments obtained from different collections. For example,
  * an implementation of `Map` based on `LongTrieKeys` may use its trie as-is to represent its key set. Adding
  * new elements to the latter would result in a `Set` implementation consisting of both 'set' and 'map' nodes.
  *
  * Unlike [[LongTrieKeys]] it lacks self type parameter which makes it possible to use as a type argument.
  */
trait LongTrie extends LongTrieKeys[LongTrie, LongTrie] { this :BinaryTrieNode => }





trait BaseLongTriePatch extends BranchPatch[Long, LongTrie, LongTrie, LongTrie] {
	final override def patchLeft(branch :BinaryTrieBranch[Long, LongTrie], left :LongTrie) :LongTrie =
		if (left eq branch.left) branch.asTrie
		else new LongTrieBranch(branch.asTrie.label, left, branch.right)

	final override def patchRight(branch :BinaryTrieBranch[Long, LongTrie], right :LongTrie) :LongTrie =
		if (right eq branch.right) branch.asTrie
		else new LongTrieBranch(branch.asTrie.label, branch.left, right)
}





object LongTrie extends BinaryTrieKeySetFactory[Long, LongTrie, LongTrie]
                   with StableBinaryTrieKeySetFactory[Long, LongTrie] with BaseLongTriePatch
{

	val EmptyLongTrie :LongTrie = MutableLongTrie.EmptyMutableLongTrie

	type LongTrieLeaf = MutableLongTrie.MutableLongTrieLeaf





	sealed class LongTrieBranch(centre :Long, l :LongTrie, r :LongTrie)
		extends LongKeyBranch[LongTrie](centre, l, r) with StableBinaryTrieBranch[Long, LongTrie] with LongTrie with BaseLongTriePatch
	{
		override def emptyTrie :LongTrie = EmptyMutableLongTrie

		override def cloneLeaf(leaf :LongTrie) :LongTrie = leaf

		override def likeLeaf(leaf :LongTrie) :LongTrie = leaf

		override protected def patchBranch(branch :BinaryTrieBranch[Long, LongTrie])
		                                  (left :LongTrie, right :LongTrie) :LongTrie =
			if (left == branch.left & right == branch.right) branch.asTrie
			else new LongTrieBranch(branch.asTrie.label, left, right)

		override protected def swapLeft(branch :BinaryTrieBranch[Long, LongTrie], left :LongTrie) :LongTrie =
			new LongTrieBranch(branch.asTrie.label, left, branch.right)

		override protected def swapRight(branch :BinaryTrieBranch[Long, LongTrie], right :LongTrie) :LongTrie =
			new LongTrieBranch(branch.asTrie.label, branch.left, right)


		override protected def branchLike(branch :LongTrie)(l :LongTrie, r :LongTrie) :LongTrie =
			new LongTrieBranch(branch.label, l, r)

	}






	type LongTrieOp = SharingTrieOp[LongTrie, LongTrie]

	trait BaseLongTrieOp extends TrieOp[LongTrie, LongTrie] {
		override def reduce(path :LongTrie)(res1 :LongTrie, res2 :LongTrie) :LongTrie =
			new LongTrieBranch(path.label, res1, res2)
	}




	type LongTriePatch = TrieKeyPatch[Long, LongTrie, LongTrie]

	type TrackingLongTriePatch = TrackingTrieKeyPatch[Long, LongTrie, LongTrie]



	class DeleteKey extends super.DeleteKey with BaseLongTriePatch {
		override def trackSize :TrackingLongTriePatch =  new TrackingDelete
	}

	class TrackingDelete extends super.TrackingDelete with BaseLongTriePatch

	final override val DeleteKey :LongTriePatch = new DeleteKey

	@inline override def TrackingDelete :TrackingLongTriePatch = new TrackingDelete




	class InsertKey extends super.InsertKey with BaseLongTriePatch {
		override def trackSize :TrackingLongTriePatch = new TrackingInsert
	}

	class TrackingInsert extends super.TrackingInsert with TrackingLongTriePatch

	final override val InsertKey :LongTriePatch = new InsertKey

	@inline override def TrackingInsert :TrackingLongTriePatch = new TrackingInsert




	class FlipKey extends super.FlipKey with BaseLongTriePatch {
		override def trackSize :TrackingLongTriePatch = new TrackingFlip
	}

	class TrackingFlip extends super.TrackingFlip with BaseLongTriePatch

	override val FlipKey :LongTriePatch = new FlipKey

	override def TrackingFlip :TrackingLongTriePatch = new TrackingFlip





	override val Difference :LongTrieOp = new StableDifference with BaseLongTrieOp

	override val SelfDifference :LongTrieOp = new SelfDifference with BaseLongTrieOp


	override val Union :LongTrieOp = new StableUnion with BaseLongTrieOp

	override val SelfUnion :LongTrieOp = new SelfUnion with BaseLongTrieOp


	override val SymmetricDifference :LongTrieOp = new StableSymmetricDifference with BaseLongTrieOp

	override val SelfSymmetricDifference :LongTrieOp = new SelfSymmetricDifference with BaseLongTrieOp


	override val Intersection :LongTrieOp = new StableIntersection with BaseLongTrieOp

	override val SelfIntersection :LongTrieOp = new SelfIntersection with BaseLongTrieOp




	override def emptyTrie :LongTrie = EmptyLongTrie

	override def newRoot(trie :LongTrie) :LongTrie = trie.stable


	override protected def newLeaf(key :Long) :LongTrie = new LongTrieLeaf(key)

	@inline override protected def newLeaf(key :Long, closest :LongTrie) :LongTrie = {
		val center = centerLabel(closest.label, key)
		if ((key & center) == center)
			new LongTrieBranch(center, closest, new LongTrieLeaf(key))
		else
			new LongTrieBranch(center, new LongTrieLeaf(key), closest)
	}


	@inline override protected def joinTries(t1 :LongTrie, t2 :LongTrie) :LongTrie = {
		val key1 = t1.label
		val center = centerLabel(key1, t2.label)
		if ((key1 & center) == center)
			new LongTrieBranch(center, t2, t1)
		else
			new LongTrieBranch(center, t1, t2)
	}


	@inline override protected def reduce(parent :LongTrie)(left :LongTrie, right :LongTrie) :LongTrie =
		new LongTrieBranch(parent.label, left, right)


	override def trieTypeName = "LongTrie"
}
