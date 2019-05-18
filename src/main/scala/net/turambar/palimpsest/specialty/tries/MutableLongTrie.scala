package net.turambar.palimpsest.specialty.tries

import net.turambar.palimpsest.specialty.tries.BinaryTrie.{BinaryTrieBranch, BinaryTrieNode, MutableBinaryTrieLeaf, MutableEmptyBinaryTrie}
import net.turambar.palimpsest.specialty.tries.LongTrie.{LongTrieBranch, LongTrieLeaf}
import net.turambar.palimpsest.specialty.tries.LongTrieKeys.{centerLabel, EmptyLongKeys, GenericEmptyLongKeys, GenericLongKeyBranch, GenericLongKeyLeaf, LongKeyBranch, LongKeyLeaf, MutableLongKeyBranch}
import net.turambar.palimpsest.specialty.tries.BinaryTrieKeySetFactory.{SharingTrieOp, TrieKeyPatch}
import net.turambar.palimpsest.specialty.tries.GenericBinaryTrie.BranchPatch
import net.turambar.palimpsest.specialty.tries.MutableLongTrie.{EmptyMutableLongTrie, MutableLongTrieBranch}




/** A concrete implementation of tries using `Long` values as keys, without any associated values.
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed trait MutableLongTrie
	extends LongTrie with LongTrieKeys[LongTrie, MutableLongTrie] with MutableBinaryTrie[Long, LongTrie, MutableLongTrie]
{ this :BinaryTrieNode =>

	override def emptyTrie :MutableLongTrie = EmptyMutableLongTrie

}





trait BaseMutableLongTriePatch extends BranchPatch[Long, LongTrie, MutableLongTrie, MutableLongTrie] {
	@inline final override protected def patchLeft(template :BinaryTrieBranch[Long, LongTrie], left :MutableLongTrie) :MutableLongTrie =
		new MutableLongTrieBranch(template.asTrie.label, left, template.rightView)

	@inline final override protected def patchRight(template :BinaryTrieBranch[Long, LongTrie], right :MutableLongTrie) :MutableLongTrie =
		new MutableLongTrieBranch(template.asTrie.label, template.left, template.rightView)
}





object MutableLongTrie extends MutableBinaryTrieKeySetFactory[Long, LongTrie, MutableLongTrie] with BaseMutableLongTriePatch {


	object EmptyMutableLongTrie extends GenericEmptyLongKeys[LongTrie, MutableLongTrie]
		with MutableEmptyBinaryTrie[Long, LongTrie, MutableLongTrie] with MutableLongTrie


	class MutableLongTrieLeaf(k :Long) extends GenericLongKeyLeaf[LongTrie, MutableLongTrie](k)
		with MutableBinaryTrieLeaf[Long, LongTrie, MutableLongTrie] with MutableLongTrie



	final class MutableLongTrieBranch(centre :Long, l :LongTrie, r :LongTrie)
		extends GenericLongKeyBranch[LongTrie, MutableLongTrie](centre, l, r)
		   with MutableLongKeyBranch[LongTrie, MutableLongTrie] with MutableLongTrie with BaseMutableLongTriePatch
	{

		override def stable :LongTrie = {
			val l = left.view; left = l
			val r = right.view; right = r
			new LongTrieBranch(center, l, r)
		}

		override def view :LongTrie = new LongTrieBranch(center, left, right)


		override def copy :MutableLongTrie = {
			val l = left.view; left = l
			val r = right.view; right = r
			new MutableLongTrieBranch(center, l, r)
		}


		override protected def cloneLeaf(leaf :LongTrie) :MutableLongTrie = new MutableLongTrieLeaf(leaf.key)

		override protected def share(subtrie :LongTrie) :MutableLongTrie = subtrie match {
			case branch :LongKeyBranch[LongTrie] => new MutableLongTrieBranch(branch.center, branch.leftView, branch.rightView)
			case leaf :MutableLongTrieLeaf => leaf
			case _ => new MutableLongTrieLeaf(subtrie.key)
		}

		protected override def likeLeaf(leaf :LongTrie) :MutableLongTrie = leaf match {
			case mutable :MutableLongTrieLeaf => mutable
			case _ => new MutableLongTrieLeaf(leaf.key)
		}

		override protected def branchLike(branch :LongTrie)(l :LongTrie, r :LongTrie) :MutableLongTrieBranch =
			new MutableLongTrieBranch(branch.label, l, r)

		override protected def swapLeft(branch :SuperBranch, left :LongTrie) :MutableLongTrie =
			new MutableLongTrieBranch(branch.asTrie.label, left, branch.rightView)

		override protected def swapRight(branch :SuperBranch, right :LongTrie) :MutableLongTrie =
			new MutableLongTrieBranch(branch.asTrie.label, branch.leftView, branch.right)

		override def patchBranch(branch :SuperBranch)(left :LongTrie, right :LongTrie) :MutableLongTrie =
			new MutableLongTrieBranch(branch.asTrie.label, left, right)

	}



	override def emptyTrie :MutableLongTrie = EmptyMutableLongTrie

	override def newRoot(trie :LongTrie) :MutableLongTrie = trie match {
		case mutable :MutableLongTrie => mutable
		case branch :LongKeyBranch[LongTrie] => new MutableLongTrieBranch(branch.center, branch.left, branch.right)
		case _ if trie.isEmpty => EmptyMutableLongTrie
		case _ => new MutableLongTrieLeaf(trie.key)
	}


	/** Adapts the most generic compatible leaf type to the leaf of the associated trie type `T`. */
	override protected def leafLike(leaf :LongTrie) :MutableLongTrie = leaf match {
		case mutable :MutableLongTrieLeaf => mutable
		case _ => new MutableLongTrieLeaf(leaf.key)
	}

	/** Factory for leaves/singleton tries used by patch implementations. */
	override protected def newLeaf(key :Long) :MutableLongTrie = new MutableLongTrieLeaf(key)

	@inline override protected def newLeaf(key :Long, closest :LongTrie) :MutableLongTrie = {
		val center = centerLabel(closest.label, key)
		if ((key & center) == center)
			new MutableLongTrieBranch(center, closest, new LongTrieLeaf(key))
		else
			new MutableLongTrieBranch(center, new LongTrieLeaf(key), closest.copy)
	}


	@inline override protected def joinTries(t1 :LongTrie, t2 :LongTrie) :MutableLongTrie = {
		val key1 = t1.label
		val center = centerLabel(key1, t2.label)
		if ((key1 & center) == center)
			new MutableLongTrieBranch(center, t2, t1)
		else
			new MutableLongTrieBranch(center, t1, t2)
	}


	@inline override protected def reduce(parent :LongTrie)(left :LongTrie, right :LongTrie) :MutableLongTrie =
		new MutableLongTrieBranch(parent.label, left, right)



	override def trieTypeName = "MutableLongTrie"
}
