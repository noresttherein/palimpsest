package net.turambar.palimpsest.specialty.sets

//import net.turambar.palimpsest.specialty.FitIterator
//import net.turambar.palimpsest.specialty.FitTraversableOnce.OfKnownSize
//import net.turambar.palimpsest.specialty.iterables.SingletonSpecialization
//import net.turambar.palimpsest.specialty.ordered.OrderedAs.EmptyOrderedTemplate
//import net.turambar.palimpsest.specialty.ordered.ValOrdering
//import net.turambar.palimpsest.specialty.ordered.ValOrdering.LongOrdering
//import net.turambar.palimpsest.specialty.sets.LongTrieSet.{addElement, removeElement, LongTrieSet1, LongTrieSetN}
//import net.turambar.palimpsest.specialty.sets.TrieSetValues.{EditableTrieSetKeys, StableTrieSetKeys, TrieValueSetNode}
//import net.turambar.palimpsest.specialty.tries.BinaryTrie.BinaryTrieNode
//import net.turambar.palimpsest.specialty.tries.TrieBranch.ValueTrieTemplate
//import net.turambar.palimpsest.specialty.tries.EmptyTrie.AbstractEmptyTrie
//import net.turambar.palimpsest.specialty.tries.{LongTrieKeys, Trie, TrieKeys, TrieLeaf}
//import net.turambar.palimpsest.specialty.tries.LongTrieKeys.{commonPath, flipSign, EmptyLongKeys, LongKeyBranch, LongKeyLeaf, MutableLongTrie}
//import net.turambar.palimpsest.specialty.tries.OrderedLongKeyedTrie.{FlippedLongBranch, FlippedLongLeaf}
//import net.turambar.palimpsest.specialty.tries.Trie.{MutableTrieRoot, ReplaceLeaf, TrieCombinator, AbstractTriePatch}
//import net.turambar.palimpsest.specialty.tries.TrieLeaf.ValueLeaf
//
//import scala.annotation.tailrec
//
//
///**
//  * @author Marcin Mościcki
//  */
//trait LongTrieSet extends StableOrderedSet[Long] with OrderedSetTemplate[Long, LongTrieSet] with SetSpecialization[Long, LongTrieSet]
//	                 with TrieKeys[Long, Long, LongTrieSet] with TrieSetValues[Long, Long, LongTrieSet] with LongTrieKeys[Long, LongTrieSet]
//	                 with AbstractTriePatch[Long, Long, LongTrieSet]
//{ this :BinaryTrieNode =>
//	override implicit def ordering: ValOrdering[Long] = LongOrdering
//
////	override def asTrie :LongTrieSet = this
//
//	override def empty :LongTrieSet = LongTrieSet.Empty
//
//	override def contains(elem: Long): Boolean = hasKey(flipSign(elem))
//
//	override def +(elem: Long): LongTrieSet = patch(flipSign(elem), this)
//
//	override def -(elem: Long): LongTrieSet = patch(flipSign(elem), removeElement)
//
//	override def patchMissing[S >: LongTrieSet](key: Long, sibling: Trie[Long, Long, S]): S = new LongTrieSet1(key)
//
//	override def patchLeaf[S >: LongTrieSet](oldLeaf: TrieLeaf[Long, Long, S]): S = oldLeaf.asInstanceOf[LongTrieSet]
//
//	override private[sets] def update(root: MutableTrieRoot[LongTrieSet], element: Long, mutant: AbstractTriePatch[Long, Long, LongTrieSet]) :Unit =
//		mutate(root, flipSign(element), mutant)
//
//
////	override def disjoint(first: LongTrieSet, second: LongTrieSet): LongTrieSet = reduce(first, second)
//
//	override def reduced(first :LongTrieSet, second :LongTrieSet) :LongTrieSet = {
//		val path2 = second.key
//		val path = commonPath(first.key, path2)
//		if ((path & path2) == path)
//			newBranch(path, first, second)
//		else
//			newBranch(path, second, first)
//	}
//
//
//	override protected[this] def newBranch(rpath: Long, left: LongTrieSet, right: LongTrieSet): LongTrieSet =
//		new LongTrieSetN(rpath, left, right)
//
//
//
//	override def editable :LongTrieSet = this
//	override def stable :LongTrieSet = this
//
//	override def mutable :MutableOrderedSet[Long] = new MutableOrderedTrieSet[Long, Long, LongTrieSet](this, size)
//}
//
//
//
//
//
//object LongTrieSet {
//	@inline final def SignBit :Long = 0x8000000000000000L
//
//
//
//	object Empty extends EmptyLongKeys[Long, Long, LongTrieSet] with LongTrieSet
//						 with EmptyOrderedTemplate[Long, LongTrieSet] with EmptySetSpecialization[Long, LongTrieSet]
//	{
//
////		@inline final override def asTrie :LongTrieSet = this
//	}
//
//	class LongTrieSet1(flipped :Long)
//		extends LongKeyLeaf[Long, Long, LongTrieSet](flipped) with ValueLeaf[Long, Long, LongTrieSet]
//				with LongTrieSet with FlippedLongLeaf[Long, LongTrieSet] with SingletonSpecialization[Long, LongTrieSet] //with AbstractTriePatch[Long, Long, LongTrieSet]
//	{
//
//		@inline final override def value = flipSign(key)
//		@inline final override def head = flipSign(key)
//	}
//
//
//
//	/** Base class for all long trie sets with at least two elements. In itself, it serves
//	  * a niche function of adapting a stable (immutable) set for use with [[MutableTrieSet]].
//	  * When adding an immutable set to a mutable one as an independent subtrie, sets of size above
//	  * a predefined threshold do not clone their data into mutable/editable instances, but instead
//	  * return an instance of this class, which marks any branches below it as immutable and prevents
//	  * their modification. Only when an in-place update of this subtrie is requested, this instance
//	  * is converted into an [[EditableLongTrieSet]] reflecting the modified contents.
//	  *
//	  * Grouping both immutable and mutable sets under single base class allows for rather seamless interoperability
//	  * between them, resulting in more efficient handling of cases such as adding an immutable set to a mutable one.
//	  * It also limits class file size for all classes by reusing common set methods and static forwarders to implemented interfaces.
//	  * @see [[EditableLongTrieSet]]
//	  * @see [[LongTrieSetN]]
//	  */
//	class LongTrieSetNode(path :Long, zeros :LongTrieSet, ones :LongTrieSet)
//		extends LongKeyBranch[Long, LongTrieSet](path, zeros, ones)
//				with ValueTrieTemplate[Long, Long, LongTrieSet]
//				with FlippedLongBranch[Long, LongTrieSet] with LongTrieSet
//				with TrieValueSetNode[Long, Long, LongTrieSet]
//	{
////		@inline final override def asTrie :LongTrieSet = this
//
//		override def value = flipSign(key) //?
//		override def iterator: FitIterator[Long] = new FlippedKeyIterator(this)
//
////		@inline final override def head = left.head
////		@inline final override def last = right.last
//
//
////		override private[sets] def update(root: MutableTrieRoot[LongTrieSet], element: Long, mutant: AbstractTriePatch[Long, Long, LongTrieSet]) =
////			root.hang(patchSubtrie(root, flipSign(element), mutant)(this))
//
////		override def emptyLeft(right: LongTrieSet): LongTrieSet = right.editable
////		override def emptyRight(left: LongTrieSet): LongTrieSet = left.editable
////		override def matched(left: LongTrieSet, right: LongTrieSet): LongTrieSet = left.editable
//
//
//		override def reduced(first: LongTrieSet, second: LongTrieSet) = {
//			val key = first.key; val split = commonPath(key, second.key)
//			if ((split & key) == split)
//				new EditableLongTrieSet(split, second, first)
//			else
//				new EditableLongTrieSet(split, first, second)
//		}
//
//		override def disjoint(first: LongTrieSet, second: LongTrieSet): LongTrieSet = reduced(first.editable, second.editable)
//
//
//		override protected[this] def copy(l: LongTrieSet, r: LongTrieSet) =
//			new LongTrieSetNode(center, left, right)
//
//		override protected[this] def newBranch(rpath: Long, left: LongTrieSet, right: LongTrieSet): LongTrieSet =
//			new EditableLongTrieSet(rpath, left.editable, right.editable)
//
////		override def clone() = new LongTrieSetNode(center, left, right)
//		override def stable :LongTrieSet = new LongTrieSetN(center, left, right)
//
//	}
//
//
//	@inline final def EditableLongTrieSet(s1 :LongTrieSet, s2 :LongTrieSet) :LongTrieSetN = {
//		val path2 = s2.key
//		val path = commonPath(s1.key, path2)
//		if ((path & path2) == path)
//			new LongTrieSetN(path, s1, s2)
//		else
//			new LongTrieSetN(path, s2, s1)
//	}
//
//
//	/** A non-immutable trie branch used internally by [[MutableTrieSet]]. By implementing [[MutableLongTrie]],
//	  * it allows in-place modification of its subtries. Note that it is not a full implementation of a mutable
//	  * set as in [[MutableSet]]/[[collection.mutable.Set]], and should be treated more as data than a
//	  * first-class entity.
//	  */
//	class EditableLongTrieSet(path :Long, zeros :LongTrieSet, ones :LongTrieSet)
//		extends LongTrieSetNode(path, zeros, ones) with EditableTrieSetKeys[Long, Long, LongTrieSet] with MutableLongTrie[Long, LongTrieSet] with OfKnownSize
//	{
////		override private[sets] def update(root: MutableTrieRoot[LongTrieSet], element: Long, mutant: AbstractTriePatch[Long, Long, LongTrieSet]) =
////			mutate(root, flipSign(element), mutant)
//
//
////		override def emptyRight(left: LongTrieSet): LongTrieSet = left
////		override def matched(left: LongTrieSet, right: LongTrieSet): LongTrieSet = left
//
////		override def disjoint(first: LongTrieSet, second: LongTrieSet): LongTrieSet = {
////			val key = first.key; val split = commonPath(key, second.key)
////			if ((split & key) == split)
////				new EditableLongTrieSet(split, second.editable, first)
////			else
////				new EditableLongTrieSet(split, first, second.editable)
////		}
//
//		override protected[this] def newBranch(key :Long, l :LongTrieSet, r :LongTrieSet) :EditableLongTrieSet =
//			new EditableLongTrieSet(key, l, r)
//
//		override protected[this] def copy(l: LongTrieSet, r: LongTrieSet) :EditableLongTrieSet =
//			new EditableLongTrieSet(center, left, right)
//
//
//		override def stable = new LongTrieSetN(center, left.stable, right.stable)
////		override def clone() = new EditableLongTrieSet(center, left.clone(), right.clone())
//
//	}
//
//
//	@inline final def StableLongTrieSet(s1 :LongTrieSet, s2 :LongTrieSet) :LongTrieSetN = {
//		val path2 = s2.key
//		val path = commonPath(s1.key, path2)
//		if ((path & path2) == path)
//			new LongTrieSetN(path, s1, s2)
//		else
//			new LongTrieSetN(path, s2, s1)
//	}
//
//
//	/** Proper implementation of really immutable long tries of size >= 2, used wherever the application
//	  * requires a [[LongTrieSet]] (or [[StableOrderedSet[Long] ]] instance, unlike the other
//	  * trie branch classes here, which are used only internally by the mutable set.
//	  */
//	class LongTrieSetN(path :Long, zeros :LongTrieSet, ones :LongTrieSet, private[this] var leaves :Int)
//		extends LongTrieSetNode(path, zeros, ones) with StableTrieSetKeys[Long, Long, LongTrieSet] with OfKnownSize
//	{
//		def this(path :Long, zeros :LongTrieSet, ones :LongTrieSet) = this(path, zeros, ones, zeros.size + ones.size)
//
//		@inline final override def size = leaves
//
//
////		override def keyAt(idx: Int) :Long =
////			if (idx<0 || idx>=size)
////				throw new IndexOutOfBoundsException(s"$stringPrefix<$size>.nth($idx)")
////			else {
////				@tailrec def skip(trie :LongTrieSet, n :Int) :Long = trie match {
////					case branch :LongTrieSetN =>
////						val lsize = branch.left.size
////						if (n < lsize) skip(branch.left, n)
////						else skip(branch.right, n-lsize)
////					case leaf :LongTrieSet1 => leaf.value
////				}
////				if (idx < left.size)
////					skip(left, idx)
////				else
////					skip(right, idx - left.size)
////			}
////
////
////		/** These methods are used to implement `++`/`union` via `combine`. */
////		override def emptyLeft(right: LongTrieSet): LongTrieSet = right//.stable
////		override def emptyRight(left: LongTrieSet): LongTrieSet = left
////		override def matched(left: LongTrieSet, right: LongTrieSet): LongTrieSet = left
//
//		override def reduced(first: LongTrieSet, second: LongTrieSet) :LongTrieSet = {
//			val key = first.key; val split = commonPath(key, second.key)
//			if ((split & key) == split)
//				new LongTrieSetN(split, second, first)
//			else
//				new LongTrieSetN(split, first, second)
//		}
//
//		override def disjoint(first: LongTrieSet, second: LongTrieSet): LongTrieSet = reduced(first, second.stable)
//
//
//
//		final override protected[this] def copy(l: LongTrieSet, r: LongTrieSet) :LongTrieSetN =
//			new LongTrieSetN(center, left, right, size)
//
//		override protected[this] def newBranch(rpath: Long, left: LongTrieSet, right: LongTrieSet): LongTrieSet =
//			new LongTrieSetN(rpath, left, right)
//
////		override def clone() = this //new LongTrieSetN(center, left.clone(), right.clone(), size)
//
////		override def stable = this
//
//		override def editable =
//			if (size <= 8) new EditableLongTrieSet(center, left.editable, right.editable)
//			else new LongTrieSetNode(center, left, right)
//
//
//	}
//
//
//
//
//
//
//	@inline final def addElement :AbstractTriePatch[Long, Long, LongTrieSet] = NewLeaf
//
//	private[this] final val NewLeaf = new AbstractTriePatch[Long, Long, LongTrieSet] {
//		override def patchMissing[S>:LongTrieSet](key: Long, sibling: Trie[Long, Long, S]): S = new LongTrieSet1(key)
//		override def patchLeaf[S>:LongTrieSet](oldLeaf: TrieLeaf[Long, Long, S]): S = oldLeaf.asTrie
//	}
//
//	@inline final def removeElement :AbstractTriePatch[Long, Long, LongTrieSet] = RemoveLeaf
//
//	private[this] final val RemoveLeaf = new ReplaceLeaf[Long, Long, LongTrieSet](Empty)
///*
//
//
//	@inline final def union :TrieCombinator[LongTrieSet] = SharedUnion
//
//	private[this] final val SharedUnion = new TrieCombinator[LongTrieSet] {
//		override def emptyLeft(right: LongTrieSet): LongTrieSet = right
//		override def emptyRight(left: LongTrieSet): LongTrieSet = left
//		override def disjoint(left: LongTrieSet, right: LongTrieSet): LongTrieSet = StableLongTrieSet(left, right)
//		override def matched(left: LongTrieSet, right: LongTrieSet): LongTrieSet = right
//	}
//
//	@inline final def stableUnion :TrieCombinator[LongTrieSet] = StableUnion
//
//	private[this] final val StableUnion = new TrieCombinator[LongTrieSet] {
//		override def emptyLeft(right: LongTrieSet): LongTrieSet = right.stable
//		override def emptyRight(left: LongTrieSet): LongTrieSet = left
//		override def disjoint(left: LongTrieSet, right: LongTrieSet): LongTrieSet = StableLongTrieSet(left, right.stable)
//		override def matched(left: LongTrieSet, right: LongTrieSet): LongTrieSet = left
//	}
//
//	@inline final def mutableUnion :TrieCombinator[LongTrieSet] = MutableUnion
//
//	private[this] final val MutableUnion = new TrieCombinator[LongTrieSet] {
//		override def emptyLeft(right: LongTrieSet): LongTrieSet = right.clone().editable
//		override def emptyRight(left: LongTrieSet): LongTrieSet = left.clone().editable
//		override def disjoint(left: LongTrieSet, right: LongTrieSet): LongTrieSet = EditableLongTrieSet(left.clone().editable, right.clone().editable)
//		override def matched(left: LongTrieSet, right: LongTrieSet): LongTrieSet = left.clone().editable
//	}
//
//
//
//	@inline final def mutableDifference :TrieCombinator[LongTrieSet] = MutableDifference
//
//	private[this] final val MutableDifference = new TrieCombinator[LongTrieSet] {
//		override def emptyLeft(right: LongTrieSet): LongTrieSet = Empty
//		override def emptyRight(left: LongTrieSet): LongTrieSet = left.clone()
//		override def disjoint(left: LongTrieSet, right: LongTrieSet): LongTrieSet = left.clone()
//		override def matched(left: LongTrieSet, right: LongTrieSet): LongTrieSet = Empty
//	}
//
//
//	@inline final def mutableIntersection :TrieCombinator[LongTrieSet] = MutableIntersection
//
//	private[this] final val MutableIntersection = new TrieCombinator[LongTrieSet] {
//		override def emptyLeft(right: LongTrieSet): LongTrieSet = Empty
//		override def emptyRight(left: LongTrieSet): LongTrieSet = Empty
//		override def disjoint(left: LongTrieSet, right: LongTrieSet): LongTrieSet = Empty
//		override def matched(left: LongTrieSet, right: LongTrieSet): LongTrieSet = left.clone()
//	}
//	*/
//
//}
